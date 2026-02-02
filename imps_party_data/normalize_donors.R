# ============================================================
#  Donor Normalisation (3-column output)
# Columns:
#   donor_raw
#   donor_display
#   ultimate_donor_normalized
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(stringi)
})

# -----------------------------
# Read the data
# -----------------------------
donors <- read_csv("distinct_donors_for_matching.csv", show_col_types = FALSE)

# -----------------------------
# Helpers
# -----------------------------
`%||%` <- function(a, b) ifelse(is.na(a), b, a)

trim_quotes <- function(x) {
  str_replace_all(x, '^"+|"+$', "") %>% str_squish()
}

normalize_unicode <- function(x) {
  stringi::stri_trans_nfkc(x)
}

standardize_corp_suffixes <- function(x) {
  x %>%
    str_replace_all(regex("\\bPROPRIETARY\\s+LIMITED\\b", TRUE), "PTY LTD") %>%
    str_replace_all(regex("\\bP\\s*\\.?\\s*T\\s*\\.?\\s*Y\\b", TRUE), "PTY") %>%
    str_replace_all(regex("\\bPTY\\s+LTD\\b", TRUE), "PTY LTD") %>%
    str_replace_all(regex("\\bLIMITED\\b", TRUE), "LTD") %>%
    str_replace_all(regex("\\bL\\s*\\.?\\s*T\\s*\\.?\\s*D\\b", TRUE), "LTD") %>%
    str_replace_all(regex("\\bP\\/L\\b", TRUE), "PTY LTD") %>%
    str_squish()
}

make_display_name <- function(x) {
  x %>%
    normalize_unicode() %>%
    trim_quotes() %>%
    standardize_corp_suffixes() %>%
    str_replace_all(regex("\\s+/\\s+"), " / ") %>%
    str_squish()
}

make_canonical_key <- function(x) {
  x %>%
    normalize_unicode() %>%
    trim_quotes() %>%
    str_to_upper() %>%
    standardize_corp_suffixes() %>%
    str_replace_all("&", " AND ") %>%
    str_replace_all("[/]", " ") %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>%
    str_replace_all(
      regex("\\b(PTY LTD|LTD|INC|CO|CORP|CORPORATION|GROUP|HOLDINGS|SERVICES)\\b", TRUE),
      ""
    ) %>%
    str_squish()
}

normalize_missing <- function(x) {
  x2 <- trim_quotes(x)
  bad <- str_to_lower(x2) %in% c(
    "", "unknown", "none disclosed", "no disclosures",
    "general donations", "source", "election funding"
  )
  ifelse(is.na(x2) | bad, NA_character_, x2)
}

# -----------------------------
# Dictionary (high-confidence exact matches)
# -----------------------------
ultimate_dict <- tibble::tribble(
  ~donor_key,                          ~ultimate_donor_normalized,

  # Political parties
  "AUSTRALIAN GREENS",                 "The Australian Greens",
  "THE AUSTRALIAN GREENS",             "The Australian Greens",
  "ALP",                               "Australian Labor Party",
  "AUSTRALIAN LABOR PARTY",            "Australian Labor Party",
  "LIBERAL PARTY",                     "Liberal Party of Australia",
  "LIBERAL PARTY OF AUSTRALIA",        "Liberal Party of Australia",
  "THE NATIONALS",                     "The Nationals",
  "NATIONAL PARTY",                    "The Nationals",
  "NATIONAL PARTY OF AUSTRALIA",       "The Nationals",

  # Corporates
  "ALLIANZ AUSTRALIA",                 "Allianz Australia",
  "ALLIANZ AUSTRALIA INSURANCE",       "Allianz Australia",
  "AIA AUSTRALIA",                     "AIA Australia",
  "AIA VITALITY",                      "AIA Australia",
  "ANZ BANKING",                       "ANZ",
  "AUSTRALIA AND NEW ZEALAND BANKING", "ANZ",

  # Media
  "NINE ENTERTAINMENT",                "Nine Entertainment",
  "NINE NETWORK AUSTRALIA",            "Nine Entertainment",
  "CHANNEL 9",                         "Nine Entertainment",

  # Health
  "BUPA",                              "Bupa",
  "BUPA AUSTRALIA",                    "Bupa",
  "BUPA ASIA PACIFIC",                 "Bupa"
)

# -----------------------------
# Regex-based ultimate donor rules
# -----------------------------
apply_ultimate_rules <- function(donor_key, donor_display) {
  k <- donor_key %||% ""
  d <- donor_display %||% ""

  if (str_detect(k, "\\bAUSTRALIAN GREENS\\b|\\bGREENS\\b"))
    return("The Australian Greens")

  if (str_detect(k, "\\bALP\\b|\\bAUSTRALIAN LABOR PARTY\\b"))
    return("Australian Labor Party")

  if (str_detect(k, "\\bLIBERAL PARTY\\b"))
    return("Liberal Party of Australia")

  if (str_detect(k, "\\bTHE NATIONALS\\b|\\bNATIONAL PARTY\\b"))
    return("The Nationals")

  if (str_detect(k, "\\bALLIANZ\\b"))
    return("Allianz Australia")

  if (str_detect(k, "\\bAIA\\b"))
    return("AIA Australia")

  if (str_detect(k, "\\bANZ\\b|\\bAUSTRALIA AND NEW ZEALAND BANKING\\b"))
    return("ANZ")

  if (str_detect(k, "\\bNINE\\b|\\bCHANNEL 9\\b"))
    return("Nine Entertainment")

  if (str_detect(k, "\\bBUPA\\b"))
    return("Bupa")

  # Estates → strip wrapper, keep person
  if (str_detect(str_to_lower(d), "^estate of\\s+"))
    return(str_replace(d, regex("^Estate of\\s+", TRUE), "") %>% str_squish())

  d
}

# -----------------------------
# Normalisation pipeline
# -----------------------------
donors_normalized <- donors %>%
  mutate(
    donor_raw = normalize_missing(Donor_Name),
    donor_display = if_else(is.na(donor_raw), NA_character_, make_display_name(donor_raw)),
    donor_key = if_else(is.na(donor_raw), NA_character_, make_canonical_key(donor_raw))
  ) %>%
  left_join(ultimate_dict, by = "donor_key") %>%
  mutate(
    ultimate_donor_normalized = if_else(
      is.na(ultimate_donor_normalized),
      mapply(apply_ultimate_rules, donor_key, donor_display, USE.NAMES = FALSE),
      ultimate_donor_normalized
    )
  ) %>%
  select(donor_raw, donor_display, ultimate_donor_normalized)

# -----------------------------
# Write final CSV
# -----------------------------
write_csv(donors_normalized, "donor_normalized_ultimate.csv")


