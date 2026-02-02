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
# 2. H3 PREPARATION (PIVOTAL EVENTS & AGGREGATE TABLE)
# ==============================================================================

pivotal_events <- tribble(
  ~Actor,       ~Year, ~Type,
  "Wilkie",      2010, "Pivotal (HoR)",
  "KAP",         2010, "Pivotal (HoR)",
  "Greens",      1998, "Pivotal (Sen)",
  "Greens",      2001, "Pivotal (Sen)",
  "Greens",      2007, "Pivotal (Sen)",
  "Greens",      2010, "Pivotal (HoR/Sen)",
  "Greens",      2013, "Pivotal (Sen)",
  "Greens",      2016, "Pivotal (Sen)",
  "Greens",      2019, "Pivotal (Sen)",
  "Greens",      2022, "Pivotal (Sen)",
  "One Nation",  2001, "Pivotal (Sen)",
  "One Nation",  2016, "Pivotal (Sen)",
  "One Nation",  2019, "Pivotal (Sen)"
)

# Join events and calculate stats
h3_data <- full_data %>%
  left_join(pivotal_events, by = c("Actor", "Year")) %>%
  mutate(Is_Pivotal = ifelse(!is.na(Type), "Yes", "No")) %>%
  arrange(Actor, Year) %>%
  group_by(Actor) %>%
  mutate(
    Prev_Receipts = lag(Total_Receipts),
    Pct_Change = (Total_Receipts - Prev_Receipts) / Prev_Receipts
  ) %>%
  ungroup()

# ==============================================================================
# 3. H3a REVISED: ROLLING VOLATILITY (Changing Over Time)
# ==============================================================================

# We calculate the Standard Deviation of the LAST 3 RETURNS for each year.
# This shows how "bumpy" the ride has been recently.

final_data <- h3_data %>%
  arrange(Actor, Year) %>%
  group_by(Actor) %>%
  mutate(
    # ROLLING WINDOW: Calculate SD of % Change for the current + previous 2 points
    # align="right" means the value at 2022 includes data from 2022, 2019, 2016
    H3a_Rolling_Volatility = rollapply(Pct_Change, width = 3, FUN = sd, fill = NA, align = "right") * 100
  ) %>%
  ungroup() %>%
  
  # Select and Rename for the Final Table
  select(
    Actor, 
    Year, 
    Is_Pivotal, 
    Total_Receipts, 
    Pct_Change, 
    H3a_Rolling_Volatility
  ) %>%
  rename(
    `Actor Name` = Actor,
    `Year` = Year,
    `Pivotal Year?` = Is_Pivotal,
    `Total Receipts ($)` = Total_Receipts,
    `H3: Yearly Growth (%)` = Pct_Change,
    `H3a: Rolling Volatility (3-Year Window)` = H3a_Rolling_Volatility
  )

# ==============================================================================
# 4. EXPORT UPDATED TABLE
# ==============================================================================

write_csv(final_data, "Final_Table_Rolling_Volatility.csv")
cat("✓ Successfully exported 'Final_Table_Rolling_Volatility.csv'\n")
cat("  Note: The first 2 years for each actor will be NA (Need 3 years to calculate SD).\n")


# ==============================================================================
# 5. H3 VISUALIZATION: GRANULAR X-AXIS & LOG SCALE
# ==============================================================================
# FILTER: Remove Major Parties for this specific chart
h3_plot_data <- h3_data %>%
  filter(!Actor %in% c("ALP", "LPA", "Nationals"))

# Create a sequence of ALL years for the X-axis
min_year <- min(h3_plot_data$Year, na.rm = TRUE)
max_year <- max(h3_plot_data$Year, na.rm = TRUE)
all_years <- seq(min_year, max_year, 1)

p1 <- ggplot(h3_plot_data, aes(x = Year, y = Total_Receipts, color = Actor, group = Actor)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 1.5) +
  
  # Highlight Pivotal Years (Large Dots)
  geom_point(data = subset(h3_plot_data, Is_Pivotal == "Yes"), 
             aes(fill = Actor), shape = 21, size = 3.5, color = "black", stroke = 1) +
  
  # Annotation Text for Pivotal Years
  geom_text_repel(data = subset(h3_plot_data, Is_Pivotal == "Yes"),
                  aes(label = paste(Year, Type, sep="\n")),
                  size = 2.5, fontface = "bold", color = "black",
                  box.padding = 0.5, min.segment.length = 0) +
  
  # SCALES
  # Log10 Scale for Y-Axis (Critical for comparing Major vs Minor parties)
  scale_y_log10(labels = dollar_format(), breaks = trans_breaks("log10", function(x) 10^x)) + 
  annotation_logticks(sides = "l") + # Adds log tick marks on the left
  
  # Granular X-Axis (Every single year)
  scale_x_continuous(breaks = all_years) +
  
  labs(
    title = "H3: Leverage and Stability (All Actors)",
    subtitle = "Logarithmic Scale used to visualize Major Parties and Independents simultaneously",
    y = "Total Receipts (Log Scale $)",
    x = "Year"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    # Rotate X-axis labels so they don't overlap
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    panel.grid.minor.x = element_blank() # Remove minor vertical lines for clarity
  )

print(p1)
ggsave("H3_Leverage_and_Stability.png", plot = p1, width = 12, height = 8, dpi = 300)

# ==============================================================================
# . H3a VISUALIZATION (TENURE VS VOLATILITY)
# ==============================================================================

# Define Tenure Map
tenure_map <- tribble(
  ~Actor,           ~Tenure_Years, ~Status,
  "ALP",            25,            "Continuous",
  "LPA",            25,            "Continuous",
  "Nationals",      25,            "Continuous",
  "Greens",         25,            "Continuous",
  "Wilkie",         12,            "Continuous",
  "KAP",            9,             "Continuous",
  "One Nation",     6,             "Somewhat",
  "McGowan/Haines", 6,             "Continuous",
  "AJP",            0,             "Nil",
  "ACP",            0,             "Nil",
  "Shooters",       0,             "Nil"
)

# Calculate Volatility
h3a_stats <- h3_data %>%
  group_by(Actor) %>%
  summarise(
    Mean_Receipts = mean(Total_Receipts, na.rm = TRUE),
    # Calculate Standard Deviation of the Year-on-Year % Change
    Volatility_StdDev_Pct = sd(Pct_Change, na.rm = TRUE) * 100, 
    .groups = "drop"
  ) %>%
  left_join(tenure_map, by = "Actor")

p2 <- ggplot(h3a_stats, aes(x = Tenure_Years, y = Volatility_StdDev_Pct, color = Actor)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_text_repel(aes(label = Actor), size = 4, box.padding = 0.5) +
  
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "H3a: Legislative Tenure vs. Volatility",
    subtitle = "Hypothesis: Longer continuous presence predicts lower volatility",
    x = "Years of Tenure",
    y = "Volatility (Std Dev of YoY % Change)"
  ) +
  theme_light() +
  theme(legend.position = "none")

print(p2)
ggsave("H3a_Legislative_Tenure.png", plot = p2, width = 10, height = 6, dpi = 300)