# ============================================================================
# H1A: INDIVIDUAL DONOR SHARE - PATTERNED GRAYSCALE (AJPS COMPLIANT)
# ============================================================================

# Load necessary libraries
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpattern")) install.packages("ggpattern")
if (!require("scales")) install.packages("scales")

library(readxl)
library(dplyr)
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
  list(file = "NORMALISED_HEADER_FILES/Independents.xlsx", sheet = "Haines McGowan", party = "Haines McGowan")
)

all_data <- do.call(rbind, lapply(excel_files, function(f) {
  read_excel(f$file, sheet = f$sheet) %>%
    select(YEAR, DONOR_NAME, AMOUNT, Category) %>%
    rename(year = YEAR, donor_name = DONOR_NAME, amount = AMOUNT, category_raw = Category) %>%
    mutate(category = toupper(trimws(as.character(category_raw))), party = f$party)
}))

# Returns the global year range for your entire dataset
range(all_data$year, na.rm = TRUE)
# Returns a table showing the min and max year for every party/candidate
all_data %>% group_by(party) %>% summarize(start = min(year), end = max(year))

# --- STEP 2: CATEGORIZE AND CLASSIFY ---
categorize_donation <- function(category) {
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

h1a_data <- all_data %>%
  mutate(
    party_type = case_when(
      party %in% c("ALP", "LPA", "Nationals") ~ "Major Parties",
      party %in% c("Greens", "ONP", "AJP", "ACP", "KAP", "Shooters") ~ "Minor Parties",
      party %in% c("Wilkie", "Haines McGowan") ~ "Independents\n(Wilkie, McGowan/Haines)",
      TRUE ~ "Unknown"
    ),
    donor_category = sapply(category, categorize_donation)
  ) %>%
  group_by(party_type, donor_category) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = 'drop') %>%
  group_by(party_type) %>%
  mutate(percentage = amount / sum(amount)) %>%
  ungroup()

# --- STEP 3: FIXED MAPPING ---
# Ensure "Individual donations" is at the base of the factor levels
cat_levels <- c("Individual donations", "For profit entities", "Associations",
                "Civil society organisations", "Party/candidate", "Subventions",
                "Political fundraising vehicles", "Other")

h1a_data$donor_category <- factor(h1a_data$donor_category, levels = rev(cat_levels))
h1a_data$party_type <- factor(h1a_data$party_type,
                              levels = c("Major Parties", "Minor Parties", "Independents\n(Wilkie, McGowan/Haines)"))

# [CORRECTION]: Changed 'point' to 'circle' for ggpattern compatibility
pattern_map <- c(
  "Individual donations"           = "none",
  "For profit entities"            = "stripe",
  "Associations"                   = "crosshatch",
  "Civil society organisations"    = "circle",
  "Party/candidate"                = "stripe",
  "Subventions"                    = "none",
  "Political fundraising vehicles"  = "crosshatch",
  "Other"                          = "none"
)

# --- STEP 4: VISUALIZATION ---
p_h1a_pattern <- ggplot(h1a_data, aes(x = party_type, y = percentage, fill = donor_category)) +
  geom_col_pattern(
    aes(pattern = donor_category),
    position = "stack",
    width = 0.6,
    color = "black",
    pattern_fill = "black",
    pattern_color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    linewidth = 0.3 # Use 'linewidth' instead of 'size'
  ) +
  scale_fill_grey(start = 0.95, end = 0.1, name = "Donor Category") +
  scale_pattern_manual(values = pattern_map, name = "Donor Category") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  labs(x = NULL, y = "Share of Total Funding") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 11, face = "bold", color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid = element_blank()
  )
# --- STEP 4: VISUALIZATION WITH LABELS ---
p_h1a_pattern <- ggplot(h1a_data, aes(x = party_type, y = percentage, fill = donor_category)) +
  
  # 1. Main Patterned Bar Layer
  geom_col_pattern(
    aes(pattern = donor_category),
    position = "stack",
    width = 0.6,
    color = "black",           
    pattern_fill = "black",    
    pattern_color = "black", 
    pattern_density = 0.1,     
    pattern_spacing = 0.02,
    linewidth = 0.3
  ) +
  
  # 2. NEW: Percentage Labels Layer
  # Uses 'color' aes to switch between white/black text based on background
  geom_text(
    aes(
      label = ifelse(percentage > 0.5, paste0(round(percentage, 1), "%"), ""),
      color = donor_category
    ),
    position = position_stack(vjust = 0.5),
    size = 3,          # Smaller font to fit in narrow bars
    fontface = "bold",
    family = "sans"
  ) +
  
  # 3. Text Color Scale (High Contrast)
  # Maps specific categories to White or Black text for readability
  scale_color_manual(values = c(
    "Individual donations" = "black",          # Light background -> Black text
    "For profit entities" = "white",           # Dark/Striped -> White text
    "Associations" = "white",
    "Civil society organisations" = "white",
    "Party/candidate" = "white",
    "Subventions" = "black",                   # Light background -> Black text
    "Political fundraising vehicles" = "white",
    "Other" = "black"
  ), guide = "none") + # Hide the text color legend
  
  # 4. Fill and Pattern Scales
  scale_fill_grey(start = 0.95, end = 0.1, name = "Donor Category") +
  scale_pattern_manual(values = pattern_map, name = "Donor Category") +
  
  # 5. Axes and Theme
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0,0)) +
  labs(x = NULL, y = "Share of Total Funding") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 11, face = "bold", color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid = element_blank()
  ) +
  
  # 6. Merge Legends
  guides(fill = guide_legend(ncol = 1), pattern = guide_legend(ncol = 1))

  # Print the plot to check labels
  print(p_h1a_pattern)
# --- STEP 5: EXPORT ---
ggsave("AJPS_H1a_Individual_Donor_Share_Patterned_Grayscale.png", 
       p_h1a_pattern, width = 10, height = 8, dpi = 600)