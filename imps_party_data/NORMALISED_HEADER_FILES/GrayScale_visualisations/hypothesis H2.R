# ============================================================================
# H2: FUNDING SOURCE DIVERSIFICATION - PATTERNED GRAYSCALE (AJPS COMPLIANT)
# ============================================================================
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(ggpattern)

# --- STEP 1: LOAD DATA ---
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

all_data <- data.frame()
for (file_info in excel_files) {
  df <- read_excel(file_info$file, sheet = file_info$sheet)
  df <- df %>%
    select(YEAR, DONOR_NAME, AMOUNT, Category) %>%
    rename(year = YEAR, donor_name = DONOR_NAME, amount = AMOUNT, category_raw = Category) %>%
    mutate(category_clean = toupper(trimws(as.character(category_raw)))) %>%
    mutate(party = file_info$party)
  all_data <- bind_rows(all_data, df)
}

# --- STEP 2: CLASSIFY AND MAP CODES ---

# Function to return Category CODES (1, 2...)
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

# Process Data
h2_plot_data <- all_data %>%
  mutate(
    party_type = case_when(
      party %in% c("ALP", "LPA", "Nationals") ~ "Major Party",
      party %in% c("Greens", "ONP", "AJP", "ACP", "KAP", "Shooters") ~ "Minor Party",
      party %in% c("Wilkie", "Haines McGowan") ~ "Independent",
      TRUE ~ "Unknown"
    ),
    donor_code = sapply(category_clean, categorize_code)
  ) %>%
  # Update label for Independent group
  mutate(party_type = factor(party_type, 
                             levels = c("Major Party", "Minor Party", "Independent"),
                             labels = c("Major Party", "Minor Party", "Independents\n(Wilkie, McGowan/Haines)"))) %>%
  # Aggregate totals per category
  group_by(party_type, donor_code) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  filter(total_amount > 0)


# Currency Formatter
currency_format <- function(x) {
  case_when(
    x >= 1e6 ~ paste0("$", round(x / 1e6, 1), "M"),
    x >= 1e3 ~ paste0("$", round(x / 1e3, 0), "k"),
    TRUE ~ paste0("$", format(x, big.mark = ","))
  )
}

# Add label text to data
h2_plot_data <- h2_plot_data %>% mutate(label_text = currency_format(total_amount))

# 1. Define the descriptive labels (No numbers)
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

# 2. Define Patterns (Keeping your mapping)
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

# 3. Loop to create 3 separate figures
actor_types <- unique(h2_plot_data$party_type)

for (actor in actor_types) {
  
  # Filter data
  plot_subset <- h2_plot_data %>% 
    filter(party_type == actor) %>%
    # Set factor levels to control the order (reverse so 1 is at top after flip)
    mutate(donor_code = factor(donor_code, levels = rev(names(labels_map))))
  
  # Clean filename
  file_name_clean <- gsub("\n|\\(.*?\\)", "", actor)
  file_name_clean <- trimws(gsub(" ", "_", file_name_clean))
  
  # Create Plot
  p <- ggplot(plot_subset, aes(x = donor_code, y = total_amount, fill = donor_code)) +
    
    # Patterned Bars
    geom_col_pattern(
      aes(pattern = donor_code),
      width = 0.7,
      color = "black",
      pattern_fill = "black",
      pattern_color = "black",
      pattern_density = 0.05,
      pattern_spacing = 0.02,
      linewidth = 0.3
    ) +
    
    # Value Labels (Currency)
    geom_text(aes(label = label_text), hjust = -0.2, size = 3, fontface = "bold") +
    
    # Scales: Map the CODES (1,2..) to NAMES ("Individual..") on the Axis
    scale_x_discrete(labels = labels_map) +
    
    # Scales: Grayscale & Patterns (Legends removed via guide="none")
    scale_fill_grey(start = 0.2, end = 0.9, guide = "none") +
    scale_pattern_manual(values = pattern_map, guide = "none") +
    
    # Log Scale for Money
    scale_y_log10(labels = scales::dollar_format(), expand = expansion(mult = c(0, 0.2))) +
    
    # Flip to horizontal bars
    coord_flip() +
    facet_wrap(~party_type) +
    
    # Axis Titles (Y-axis label removed as categories are self-explanatory)
    labs(x = 'Donor category', y = "Aggregate Financial Receipts ($AUD, Log10 Scale)") +
    
    theme_bw() +
    theme(
      # Remove Legend completely
      legend.position = "none",
      
      # Strip (Header) Styling
      strip.background = element_rect(fill = "gray90"),
      strip.text = element_text(size = 12, face = "bold"),
      
      # Axis Text Styling (Ensure category names are readable)
      axis.text.y = element_text(size = 10, face = "bold", color = "black"),
      panel.grid.minor = element_blank()
    )
  
  # Save High-Res Version
  ggsave(paste0("Clean_Label_H2_", file_name_clean, ".png"), p, width = 10, height = 7, dpi = 600)
  
  cat("✓ Saved clean figure for:", actor, "\n")
}