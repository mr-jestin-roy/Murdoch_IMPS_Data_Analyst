# ============================================================================
# H1A: POLITICAL DONATIONS COMPOSITION - PATTERNED GRAYSCALE (AJPS COMPLIANT)
# ============================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpattern)
library(scales)

# --- STEP 1: LOAD AND NORMALIZE DATA ---
excel_files <- list(
  list(file = "NORMALISED_HEADER_FILES/ALP_Combined.xlsx", sheet = "ALP (Combined)", party = "ALP"),
  list(file = "NORMALISED_HEADER_FILES/LPA_Combined.xlsx", sheet = "LPA (Combined)", party = "LPA"),
  list(file = "NORMALISED_HEADER_FILES/Nationals_Combined.xlsx", sheet = "Nationals (Combined)", party = "Nationals"),
  list(file = "NORMALISED_HEADER_FILES/Australian Greens 2025.xlsx", sheet = "AG (national)", party = "Greens"),
  list(file = "NORMALISED_HEADER_FILES/One Nation 2025.xlsx", sheet = "ONP", party = "ONP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "AJP", party = "AJP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "ACP", party = "ACP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "KAP", party = "KAP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "Shooters", party = "Shooters"),
  list(file = "NORMALISED_HEADER_FILES/Independents.xlsx", sheet = "Wilkie", party = "Wilkie"),
  list(file = "NORMALISED_HEADER_FILES/Independents.xlsx", sheet = "Haines McGowan", party = "McGowan/Haines")
)

all_data <- do.call(rbind, lapply(excel_files, function(f) {
  read_excel(f$file, sheet = f$sheet) %>%
    select(YEAR, DONOR_NAME, AMOUNT, Category) %>%
    rename(year = YEAR, donor_name = DONOR_NAME, amount = AMOUNT, category_raw = Category) %>%
    mutate(category = toupper(trimws(as.character(category_raw))), party = f$party)
}))

# --- STEP 2: CATEGORIZE AND CLASSIFY ---
categorize_label <- function(category) {
  if (is.na(category) || category == "" || category == "NA") return("Other")
  cat_str <- gsub("\\n| ", "", category)
  if (grepl("^1[A-F]?$", cat_str)) return("Individual donations")
  if (cat_str %in% c("2", "2.0"))   return("For profit entities")
  if (cat_str %in% c("3", "3.0"))   return("Civil society organisations")
  if (grepl("^4", cat_str))         return("Party/candidate")
  if (cat_str %in% c("5", "5.0"))   return("Subventions")
  if (cat_str %in% c("7", "7.0"))   return("Associations")
  if (cat_str == "8")              return("Political fundraising vehicles")
  return("Other")
}

chart_data <- all_data %>%
  mutate(donor_category = sapply(category, categorize_label)) %>%
  group_by(party, donor_category) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = 'drop') %>%
  group_by(party) %>%
  mutate(percentage = (amount / sum(amount)) * 100) %>%
  ungroup()

# Set factor levels for correct stacking and party ordering
cat_levels <- c("Individual donations", "For profit entities", "Associations", 
                "Civil society organisations", "Party/candidate", "Subventions", 
                "Political fundraising vehicles", "Other")

chart_data$donor_category <- factor(chart_data$donor_category, levels = rev(cat_levels))

# Order parties by type: Major -> Minor -> Independent
chart_data <- chart_data %>%
  mutate(party_order = case_when(
    party %in% c("ALP", "LPA", "Nationals") ~ 1,
    party %in% c("Greens", "ONP", "AJP", "ACP", "KAP", "Shooters") ~ 2,
    party %in% c("Wilkie", "McGowan/Haines") ~ 3
  )) %>%
  arrange(party_order, party) %>%
  mutate(party = factor(party, levels = unique(party)))

# --- STEP 3: SCHEMA MAPPING ---
pattern_map <- c(
  "Individual donations"           = "none",       
  "For profit entities"            = "stripe",     
  "Associations"                   = "crosshatch", 
  "Civil society organisations"    = "circle",     
  "Party/candidate"                = "stripe",     
  "Subventions"                    = "none",       
  "Political fundraising vehicles" = "crosshatch", 
  "Other"                          = "none"        
)

# --- STEP 4: VISUALIZATION ---
p_h1a_final <- ggplot(chart_data, aes(x = party, y = percentage, fill = donor_category)) +
  geom_col_pattern(
    aes(pattern = donor_category),
    position = "stack",
    width = 0.65,
    color = "black",           
    pattern_fill = "black",    
    pattern_color = "black", 
    pattern_density = 0.1,     
    pattern_spacing = 0.02,
    linewidth = 0.3
  ) +
  
  # Data Labels: White text on dark gray, black on light gray
  geom_text(aes(label = ifelse(percentage > 4, paste0(round(percentage, 1), "%"), ""),
                color = donor_category),
            position = position_stack(vjust = 0.5),
            size = 3.2, fontface = "bold", show.legend = FALSE) +
  
  scale_color_manual(values = c(
    "Individual donations" = "white", "Party/candidate" = "white", 
    "Political fundraising vehicles" = "white", "For profit entities" = "black",
    "Associations" = "black", "Civil society organisations" = "black",
    "Subventions" = "black", "Other" = "black"
  )) +
  
  # Consistent Grayscale & Pattern Scales
  scale_fill_grey(start = 0.95, end = 0.1, name = "Donor Category") +
  scale_pattern_manual(values = pattern_map, name = "Donor Category") +
  
  # Labels & Theme (Titles removed per feedback)
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0), limits = c(0, 105)) +
  labs(x = "Party / Candidate", y = "Share of Total Funding (%)",
       caption = "Source: AEC | Data range: 1999-2024 (Varies by party)") +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(fill = guide_legend(ncol = 1), pattern = guide_legend(ncol = 1))

# --- STEP 5: EXPORT ---
ggsave("AJPS_Composition_Political_Donations_Patterned_Grayscale.png", 
       p_h1a_final, width = 12, height = 8, dpi = 600, bg = "white")