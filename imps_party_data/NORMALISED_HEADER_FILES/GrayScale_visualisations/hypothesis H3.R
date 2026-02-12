# ==============================================================================
# 0. SETUP & LIBRARIES
# ==============================================================================
if (!require("readxl")) install.packages("readxl")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("scales")) install.packages("scales")
if (!require("zoo")) install.packages("zoo")

library(zoo)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(stringr)

# ==============================================================================
# 1. DATA LOADING & CLEANING
# ==============================================================================

# Define file paths (Adjust "NORMALISED_HEADER_FILES/" to your actual folder path)
file_list <- list(
  list(file = "NORMALISED_HEADER_FILES/Australian Greens 2025.xlsx", sheet = "AG (national)", actor = "Greens"),
  list(file = "NORMALISED_HEADER_FILES/Independents.xlsx", sheet = "Wilkie", actor = "Wilkie"),
  list(file = "NORMALISED_HEADER_FILES/Nationals_Combined.xlsx", sheet = "Nationals (Combined)", actor = "Nationals"),
  list(file = "NORMALISED_HEADER_FILES/Independents.xlsx", sheet = "Haines McGowan", actor = "McGowan/Haines"),
  list(file = "NORMALISED_HEADER_FILES/One Nation 2025.xlsx", sheet = "ONP", actor = "One Nation"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "AJP", actor = "AJP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "ACP", actor = "ACP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "KAP", actor = "KAP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "Shooters", actor = "Shooters"),
  list(file = "NORMALISED_HEADER_FILES/ALP_Combined.xlsx", sheet = "ALP (Combined)", actor = "ALP"),
  list(file = "NORMALISED_HEADER_FILES/LPA_Combined.xlsx", sheet = "LPA (Combined)", actor = "LPA")
)

# Load and aggregate data
full_data <- data.frame()

for (item in file_list) {
  if(file.exists(item$file)) {
    df <- read_excel(item$file, sheet = item$sheet)

    # Clean Data
    clean_df <- df %>%
      select(YEAR, AMOUNT) %>%
      mutate(
        # Remove $ and ,
        AMOUNT = as.numeric(gsub("[$,]", "", as.character(AMOUNT))),
        # Extract first 4 digits for Year (handles "2025ER", "2022-2023")
        YEAR_CLEAN = as.numeric(str_extract(as.character(YEAR), "\\d{4}")),
        Actor = item$actor
      ) %>%
      filter(!is.na(AMOUNT), !is.na(YEAR_CLEAN)) %>%
      group_by(YEAR_CLEAN, Actor) %>%
      summarise(Total_Receipts = sum(AMOUNT, na.rm = TRUE), .groups = "drop") %>%
      rename(Year = YEAR_CLEAN)

    full_data <- bind_rows(full_data, clean_df)
    cat("✓ Loaded:", item$actor, "\n")
  } else {
    cat("⚠ Skipped (Not Found):", item$file, "\n")
  }
}
# ==============================================================================
# Step 2: H3 ANALYSIS: VOLATILITY BY PARTY TYPE AND ACTOR (FINALIZED)
# ==============================================================================

# 1. Define the Mapping for Hypothesis 3 (Move this up for global use)
party_type_map <- c(
  "ALP"            = "Major Party",
  "LPA"            = "Major Party",
  "Nationals"      = "Major Party",
  "Greens"         = "Minor Party",
  "One Nation"     = "Minor Party",
  "AJP"            = "Minor Party",
  "ACP"            = "Minor Party",
  "KAP"            = "Minor Party",
  "Shooters"       = "Minor Party",
  "Wilkie"         = "Independent",
  "McGowan/Haines" = "Independent"
)

# 2. H3 DATA PREPARATION
h3_data <- full_data %>%
  arrange(Actor, Year) %>%
  group_by(Actor) %>%
  mutate(
    Party_Type = party_type_map[Actor],
    Prev_Receipts = lag(Total_Receipts),
    # Convert to percentage (x100) immediately for consistency with (%) labels
    Pct_Change = ((Total_Receipts - Prev_Receipts) / Prev_Receipts) * 100
  ) %>%
  ungroup()

# 3. ROLLING VOLATILITY CALCULATION
final_data <- h3_data %>%
  arrange(Actor, Year) %>%
  group_by(Actor) %>%
  mutate(
    # Rolling SD of the percentage growth
    Rolling_Vol = rollapply(Pct_Change, width = 3, FUN = sd, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  select(
    `Actor Name` = Actor,
    `Organisation Type` = Party_Type, # Added to main data for Appendix
    Year,
    `Total Receipts ($)` = Total_Receipts,
    `H3: Yearly Growth (%)` = Pct_Change,
    `H3: Rolling Volatility (3-Year Window)` = Rolling_Vol
  )

# 4. EXPORT APPENDIX (WITHOUT 2025, 3 DECIMAL PLACES)
final_data_appendix <- final_data %>%
  filter(Year <= 2024) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

write_csv(final_data_appendix, "H3_Appendix_Yearly_Volatility.csv")

# 5. H3 SUMMARY: VOLATILITY BY ACTOR
h3_volatility_by_actor <- final_data %>%
  filter(Year <= 2024) %>%
  group_by(`Organisation Type`, `Actor Name`) %>%
  summarise(
    `Overall Volatility (%)` = sd(`H3: Yearly Growth (%)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(`Overall Volatility (%)` = round(`Overall Volatility (%)`, 3))

write_csv(h3_volatility_by_actor, "H3_Volatility_by_Actor.csv")

# 6. H3 TEST: AGGREGATE VOLATILITY BY Organisation TYPE
h3_volatility_by_party_type <- h3_volatility_by_actor %>%
  group_by(`Organisation Type`) %>%
  summarise(
    `Average Volatility (%)` = mean(`Overall Volatility (%)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(`Average Volatility (%)` = round(`Average Volatility (%)`, 3))

write_csv(h3_volatility_by_party_type, "H3_Volatility_by_Organisation_Type.csv")

cat("✓ All H3 Analysis Tables Exported.\n")


# # ==============================================================================
# # 5. H3 VISUALIZATION: GRANULAR X-AXIS & LOG SCALE
# # ==============================================================================
# # FILTER: Remove Major Parties for this specific chart
# h3_plot_data <- h3_data %>%
#   filter(!Actor %in% c("ALP", "LPA", "Nationals"))
# 
# # Create a sequence of ALL years for the X-axis
# min_year <- min(h3_plot_data$Year, na.rm = TRUE)
# max_year <- max(h3_plot_data$Year, na.rm = TRUE)
# all_years <- seq(min_year, max_year, 1)
# 
# p1 <- ggplot(h3_plot_data, aes(x = Year, y = Total_Receipts, color = Actor, group = Actor)) +
#   geom_line(linewidth = 0.8, alpha = 0.7) +
#   geom_point(size = 1.5) +
# 
#   # Highlight Pivotal Years (Large Dots)
#   geom_point(data = subset(h3_plot_data, Is_Pivotal == "Yes"),
#              aes(fill = Actor), shape = 21, size = 3.5, color = "black", stroke = 1) +
# 
#   # Annotation Text for Pivotal Years
#   geom_text_repel(data = subset(h3_plot_data, Is_Pivotal == "Yes"),
#                   aes(label = paste(Year, Type, sep="\n")),
#                   size = 2.5, fontface = "bold", color = "black",
#                   box.padding = 0.5, min.segment.length = 0) +
# 
#   # SCALES
#   # Log10 Scale for Y-Axis (Critical for comparing Major vs Minor parties)
#   scale_y_log10(labels = dollar_format(), breaks = trans_breaks("log10", function(x) 10^x)) +
#   annotation_logticks(sides = "l") + # Adds log tick marks on the left
# 
#   # Granular X-Axis (Every single year)
#   scale_x_continuous(breaks = all_years) +
# 
#   labs(
#     title = "H3: Leverage and Stability (All Actors)",
#     subtitle = "Logarithmic Scale used to visualize Major Parties and Independents simultaneously",
#     y = "Total Receipts (Log Scale $)",
#     x = "Year"
#   ) +
# 
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     # Rotate X-axis labels so they don't overlap
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
#     panel.grid.minor.x = element_blank() # Remove minor vertical lines for clarity
#   )
# 
# print(p1)
# # ggsave("H3_Leverage_and_Stability.png", plot = p1, width = 12, height = 8, dpi = 300)
# # ggsave("H3_Leverage_and_Stability.pdf", plot = p1, width = 12, height = 8, bg = "white")
# 
# # ==============================================================================
# # . H3a VISUALIZATION (TENURE VS VOLATILITY)
# # ==============================================================================
# 
# # Define Tenure Map
# tenure_map <- tribble(
#   ~Actor,           ~Tenure_Years, ~Status,
#   "ALP",            25,            "Continuous",
#   "LPA",            25,            "Continuous",
#   "Nationals",      25,            "Continuous",
#   "Greens",         25,            "Continuous",
#   "Wilkie",         12,            "Continuous",
#   "KAP",            9,             "Continuous",
#   "One Nation",     6,             "Somewhat",
#   "McGowan/Haines", 6,             "Continuous",
#   "AJP",            0,             "Nil",
#   "ACP",            0,             "Nil",
#   "Shooters",       0,             "Nil"
# )
# 
# # Calculate Volatility
# h3a_stats <- h3_data %>%
#   group_by(Actor) %>%
#   summarise(
#     Mean_Receipts = mean(Total_Receipts, na.rm = TRUE),
#     # Calculate Standard Deviation of the Year-on-Year % Change
#     Volatility_StdDev_Pct = sd(Pct_Change, na.rm = TRUE) * 100,
#     .groups = "drop"
#   ) %>%
#   left_join(tenure_map, by = "Actor")
# 
# # ==============================================================================
# # H3a VISUALIZATION (TENURE VS VOLATILITY - GRAYSCALE)
# # ==============================================================================
# 
# p2 <- ggplot(h3a_stats, aes(x = Tenure_Years, y = Volatility_StdDev_Pct)) +
#   # Points in grayscale (using 'fill' for shades of gray)
#   geom_point(aes(fill = Volatility_StdDev_Pct), shape = 21, size = 5, color = "black", stroke = 0.8) +
#   
#   # Grayscale Labels
#   geom_text_repel(aes(label = Actor), size = 4, box.padding = 0.5, color = "black") +
#   
#   # Grayscale Gradient for Points
#   scale_fill_gradient(low = "gray80", high = "gray20", guide = "none") +
#   
#   # Scales and Axis Titles (Title removed per instruction)
#   scale_y_continuous(labels = function(x) paste0(x, "%")) +
#   labs(
#     x = "Years of Continuous Tenure",
#     y = "Volatility (Std Dev of YoY % Change)"
#   ) +
#   
#   # Clean Grayscale Theme
#   theme_classic(base_size = 12) +
#   theme(
#     legend.position = "none",
#     axis.line = element_line(color = "black"),
#     axis.text = element_text(color = "black"),
#     panel.grid.major = element_line(color = "gray95")
#   )
# 
# print(p2)
# ggsave("AJPS_H3a_Legislative_Tenure_Grayscale.png", plot = p2, width = 10, height = 6, dpi = 600)
# ggsave("AJPS_H3a_Legislative_Tenure_Grayscale.pdf", plot = p2, width = 10, height = 6, bg = "white")
