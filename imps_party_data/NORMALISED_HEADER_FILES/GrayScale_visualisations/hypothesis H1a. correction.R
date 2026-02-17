# ============================================================================
# H1A: INDIVIDUAL DONOR SHARE - UNWEIGHTED PATTERNED GRAYSCALE
# SCHEMA: CONSISTENT WITH H2 (NUMERIC CODES)
# ============================================================================

# Load necessary libraries
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpattern")) install.packages("ggpattern")
if (!require("scales")) install.packages("scales")
if (!require("tidyr")) install.packages("tidyr")

library(readxl)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(scales)
library(tidyr)

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
  list(file = "NORMALISED_HEADER_FILES/Independents.xlsx", sheet = "Haines McGowan", party = "Haines McGowan")
)

all_data <- do.call(rbind, lapply(excel_files, function(f) {
  read_excel(f$file, sheet = f$sheet) %>%
    select(YEAR, DONOR_NAME, AMOUNT, Category) %>%
    rename(year = YEAR, donor_name = DONOR_NAME, amount = AMOUNT, category_raw = Category) %>%
    mutate(category = toupper(trimws(as.character(category_raw))), party = f$party)
}))

# --- STEP 2: DEFINE CONSISTENT SCHEMA (MATCHING H2) ---

# 1. Define Labels Map (Numeric Code -> Text)
labels_map <- c(
  "1" = "Individual donations",
  "2" = "For profit entities",
  "3" = "Civil society organisations",
  "4" = "Party/candidate",
  "5" = "Subventions",
  "7" = "Associations",
  "8" = "Political fundraising vehicles",
  "10" = "Other"
)

# 2. Define Pattern Map (Numeric Code -> Pattern)
pattern_map <- c(
  "1" = "none",        
  "2" = "stripe",      
  "3" = "crosshatch",  
  "4" = "circle",      
  "5" = "none",        
  "7" = "stripe",      
  "8" = "crosshatch",  
  "10" = "none"        
)

# 3. Categorization Function (Returns Numeric Codes)
categorize_code <- function(category) {
  if (is.na(category) || category == "" || category == "NA") return("10")
  cat_str <- gsub("\\n| ", "", category)
  
  if (grepl("^1[A-F]?$", cat_str)) return("1")  # Individuals
  if (cat_str %in% c("2", "2.0"))   return("2")  # For-Profit
  if (cat_str %in% c("3", "3.0"))   return("3")  # Civil Society
  if (grepl("^4", cat_str))         return("4")  # Party/candidate
  if (cat_str %in% c("5", "5.0"))   return("5")  # Subventions
  if (cat_str %in% c("7", "7.0"))   return("7")  # Associations
  if (cat_str == "8")              return("8")  # Fundraising
  return("10")                                  # Other
}

# --- STEP 3: AGGREGATE WITH UNWEIGHTED MEAN & ZERO HANDLING ---
h1a_data <- all_data %>%
  mutate(
    party_type = case_when(
      party %in% c("ALP", "LPA", "Nationals") ~ "Major Parties",
      party %in% c("Greens", "ONP", "AJP", "ACP", "KAP", "Shooters") ~ "Minor Parties",
      party %in% c("Wilkie", "Haines McGowan") ~ "Independents\n(Wilkie, McGowan/Haines)",
      TRUE ~ "Unknown"
    ),
    # Map raw strings to H2 Numeric Codes
    donor_code = sapply(category, categorize_code)
  ) %>%
  # 1. Summarize amounts per party/code
  group_by(party, party_type, donor_code) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = 'drop') %>%
  
  # 2. FORCE ZEROES: Ensure every party has every numeric code present
  complete(party, donor_code = names(labels_map), fill = list(amount = 0)) %>%
  group_by(party) %>%
  fill(party_type, .direction = "downup") %>%
  
  # 3. Calculate Unweighted Percentages
  mutate(percentage = amount / sum(amount)) %>%
  ungroup() %>%
  
  # 4. Final Aggregate Mean per Actor Class
  group_by(party_type, donor_code) %>%
  summarise(percentage = mean(percentage, na.rm = TRUE), .groups = 'drop')

# Set factor levels using the numeric map keys (consistent with H2)
h1a_data$donor_code <- factor(h1a_data$donor_code, levels = rev(names(labels_map)))
h1a_data$party_type <- factor(h1a_data$party_type, 
                              levels = c("Major Parties", "Minor Parties", "Independents\n(Wilkie, McGowan/Haines)"))

# --- STEP 4: VISUALIZATION ---
p_h1a_final <- ggplot(h1a_data, aes(x = party_type, y = percentage, fill = donor_code)) +
  geom_col_pattern(
    aes(pattern = donor_code),
    position = "fill",        # Force 100% Stacked
    width = 0.65,
    color = "black",
    pattern_fill = "black",
    pattern_color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    linewidth = 0.3
  ) +
  
  # # Percentage Labels with Contrast Logic
  # geom_text(
  #   aes(label = ifelse(percentage > 0.01, paste0(round(percentage * 100, 1), "%"), ""),
  #       color = donor_code),
  #   position = position_fill(vjust = 0.5),
  #   size = 2.8, fontface = "bold"
  # ) +
  
  # Contrast Mapping: Map Numeric Codes to White/Black text
  scale_color_manual(values = c(
    "1" = "black",  # Individuals (Light) -> Black Text
    "2" = "white",  # Profit (Dark/Stripe) -> White Text
    "3" = "white",  # Civil (Dark) -> White Text
    "4" = "white",  # Party (Dark) -> White Text
    "5" = "black",  # Subventions (Light) -> Black Text
    "7" = "black",  # Associations (Mid) -> Black Text
    "8" = "white",  # Fundraising (Dark) -> White Text
    "10" = "black"  # Other (Light) -> Black Text
  ), guide = "none") +
  
  # SCALES: Map Numeric Codes to Labels & Patterns
  scale_fill_grey(start = 0.2, end = 0.9, labels = labels_map, name = "Donor Category") +
  scale_pattern_manual(values = pattern_map, labels = labels_map, name = "Donor Category") +
  
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  labs(x = NULL, y = "Share of Total Funding (%)") +
  
  theme_minimal() +
  theme(
    
    axis.line = element_line(color = "black", linewidth = 0.3),  # Adds X and Y axis lines
    axis.ticks = element_line(color = "black", linewidth = 0.3), # Adds tick marks
    axis.text = element_text(size = 11, face = "bold", color = "black"),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Unified Legend
  guides(fill = guide_legend(ncol = 1), pattern = guide_legend(ncol = 1))

# --- STEP 5: EXPORT ---
print(p_h1a_final)
ggsave("[UPDATED]AJPS_H1a_FINAL_Patterned_Grayscale.png", p_h1a_final, width = 12, height = 8, dpi = 600)
