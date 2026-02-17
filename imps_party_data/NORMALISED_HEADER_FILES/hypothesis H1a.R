# R Script: Vertical 100% Stacked Bar Chart (Individual Bars Per Party)
# Political Donations Analysis: One bar per party
# Shows donation composition for each of 10 parties/candidates
# COMPREHENSIVE DONOR CATEGORY BREAKDOWN
# *** UPDATED WITH CATEGORY UPPERCASE NORMALIZATION ***

# Install required packages if not already installed
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("ggplot2")) install.packages("ggplot2")

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================================
# STEP 1: READ THE COMPLETE POLITICAL DONATIONS DATA # nolint
# ============================================================================

cat("STEP 1: LOADING POLITICAL DONATIONS DATA\n")

# Read all Excel files and combine data
# Working directory should be: imps_party_data/
excel_files <- list(
  list(file = "NORMALISED_HEADER_FILES/ALP_Combined.xlsx", sheet = "ALP (Combined)", party = "ALP"), # nolint
  list(file = "NORMALISED_HEADER_FILES/LPA_Combined.xlsx", sheet = "LPA (Combined)", party = "LPA"),
  list(file = "NORMALISED_HEADER_FILES/Nationals_Combined.xlsx", sheet = "Nationals (Combined)", party = "Nationals"), # nolint: line_length_linter.
  list(file = "NORMALISED_HEADER_FILES/Australian Greens 2025.xlsx", sheet = "AG (national)", party = "Greens"),
  list(file = "NORMALISED_HEADER_FILES/One Nation 2025.xlsx", sheet = "ONP", party = "ONP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "AJP", party = "AJP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "ACP", party = "ACP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "KAP", party = "KAP"),
  list(file = "NORMALISED_HEADER_FILES/Other Minor Parties_2025_A.xlsx", sheet = "Shooters", party = "Shooters"),
  list(file = "NORMALISED_HEADER_FILES/Independents.xlsx", sheet = "Wilkie", party = "Wilkie"),
  list(file = "NORMALISED_HEADER_FILES/Independents.xlsx", sheet = "Haines McGowan", party = "Haines McGowan")
)

# Read and combine all data WITH CATEGORY NORMALIZATION
all_data <- data.frame()

for (file_info in excel_files) {
  df <- read_excel(file_info$file, sheet = file_info$sheet)
  df <- df %>%
    select(YEAR, DONOR_NAME, AMOUNT, Category) %>%
    rename(year = YEAR, donor_name = DONOR_NAME, amount = AMOUNT, category_raw = Category) %>%
    # *** NEW: UPPERCASE NORMALIZATION OF CATEGORY ***
    mutate(category = toupper(trimws(as.character(category_raw)))) %>%
    select(-category_raw) %>%
    mutate(party = file_info$party)

  all_data <- bind_rows(all_data, df)

  cat("✓ Loaded:", file_info$party, "-", nrow(df), "records\n")
}

cat("\nTotal records loaded:", nrow(all_data), "\n")
cat("Categories normalized to uppercase\n\n")

# ============================================================================
# STEP 2: CLASSIFY PARTIES AND CATEGORIZE DONATIONS
# ============================================================================

cat("STEP 2: CLASSIFYING PARTIES AND DONOR CATEGORIES\n")

# Add party type classification
all_data <- all_data %>%
  mutate(
    party_type = case_when(
      party %in% c("ALP", "LPA", "Nationals") ~ "Major Party",
      party %in% c("Greens", "ONP", "AJP", "ACP", "KAP", "Shooters") ~ "Minor Party",
      party %in% c("Wilkie", "Haines McGowan") ~ "Independent",
      TRUE ~ "Unknown"
    )
  )

# COMPREHENSIVE DONOR CATEGORY MAPPING (NOW WORKS WITH NORMALIZED UPPERCASE)
categorize_donation <- function(category) {
  # Handle NA, NULL, or empty values
  if (is.na(category) || is.null(category) || category == "") {
    return("Other")
  }

  # Already normalized to UPPERCASE in Step 1
  cat_str <- trimws(category)
  cat_str <- gsub("\\n| ", "", cat_str)  # Remove newlines and spaces

  # Handle "NA" string or empty after cleaning
  if (cat_str == "NA" || cat_str == "") {
    return("Other")
  }

  # Categorize based on code (uppercase normalized)
  if (grepl("^1[A-F]?$", cat_str)) {
    return("Individual Donors")
  } else if (cat_str %in% c("2", "2.0")) {
    return("For-Profit Entities")
  } else if (cat_str %in% c("3", "3.0")) {
    return("Civil Society Organizations")
  } else if (grepl("^4", cat_str)) {
    return("Party/Corporate/Self")
  } else if (cat_str %in% c("5", "5.0")) {
    return("Government Subventions")
  } else if (cat_str %in% c("7", "7.0")) {
    return("Associations")
  } else if (cat_str == "8") {
    return("Political Fundraising Vehicles")
  } else {
    return("Other")
  }
}

# Count missing/NA categories before applying categorization
na_count <- sum(is.na(all_data$category))
empty_count <- sum(all_data$category == "" | all_data$category == "NA", na.rm = TRUE)
total_missing <- na_count + empty_count

cat("\n--- MISSING CATEGORY VALUES ---\n")
cat("NA values:", na_count, "\n")
cat("Empty/NA strings:", empty_count, "\n")
cat("Total missing (will be categorized as 'Other'):", total_missing, "\n")
cat("Percentage missing:", round(total_missing / nrow(all_data) * 100, 2), "%\n\n")

# Apply categorization
all_data <- all_data %>%
  mutate(donor_category = sapply(category, categorize_donation))

cat("Donor categories identified (post-normalization):\n")
cat_summary <- all_data %>%
  group_by(donor_category) %>%
  summarise(
    records = n(),
    total = sum(amount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(total))

print(cat_summary)

cat("\n")

# ============================================================================
# STEP 3: CALCULATE COMPOSITION BY PARTY (100% STACKED)
# ============================================================================

cat("STEP 3: CALCULATING DONATION COMPOSITION BY PARTY\n")

# Calculate total donations per party and category
party_composition <- all_data %>%
  group_by(party, party_type, donor_category) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = 'drop') %>%
  # Calculate percentages within each party
  group_by(party, party_type) %>%
  mutate(
    total = sum(amount),
    percentage = (amount / total) * 100
  ) %>%
  ungroup()

# Ensure all combinations exist (fill with 0)
all_categories <- unique(party_composition$donor_category)
all_parties <- unique(party_composition$party)

party_composition_complete <- expand_grid(
  party = all_parties,
  donor_category = all_categories
) %>%
  left_join(
    party_composition %>%
      select(party, party_type, donor_category, percentage),
    by = c("party", "donor_category")
  ) %>%
  mutate(percentage = replace_na(percentage, 0)) %>%
  left_join(
    all_data %>%
      select(party, party_type) %>%
      distinct() %>%
      rename(party_type_all = party_type),
    by = "party"
  ) %>%
  mutate(party_type = coalesce(party_type, party_type_all)) %>%
  select(party, party_type, donor_category, percentage)

cat("Composition by party (sample):\n\n")
print(head(party_composition_complete, 16))

# ============================================================================
# STEP 4: VERTICAL 100% STACKED BAR CHART (PUBLICATION-QUALITY)
# ============================================================================

cat("STEP 4: CREATING VERTICAL 100% STACKED BAR CHART (ACADEMIC STYLE)\n\n")

# DEBUG: Check which parties have data and their totals
party_totals <- party_composition_complete %>%
  group_by(party) %>%
  summarise(total_pct = sum(percentage), records = n(), .groups = 'drop') %>%
  arrange(desc(total_pct))

cat("DEBUG - Party totals check:\n")
print(party_totals)
cat("\n")

# Filter to parties WITH ACTUAL DONATIONS (>0 total records loaded)
active_parties <- party_totals %>%
  filter(total_pct > 0) %>%
  pull(party)

cat("Active parties for chart:", paste(active_parties, collapse = ", "), "\n\n")

# Filter data to active parties only + CORRECTED ORDERING
chart_data <- party_composition_complete %>%
  filter(party %in% active_parties) %>%
  mutate(
    party_order = case_when(
      party %in% c("ALP", "LPA", "Nationals") ~ 1,
      party %in% c("Greens", "ONP", "AJP", "ACP", "KAP", "Shooters") ~ 2,
      party %in% c("Wilkie", "Haines McGowan") ~ 3,
      TRUE ~ 4
    )
  ) %>%
  arrange(party_order, party) %>%
  mutate(party = factor(party, levels = unique(party))) %>%
  select(party, party_type, donor_category, percentage, party_order)

cat("Chart data ready. Categories per party:\n")
print(table(chart_data$party, chart_data$donor_category))
cat("\n")

# ============================================================================
# PROFESSIONAL COLOR PALETTE (Suitable for Economic Research Papers)
# Based on: Paul Tol Colorblind-Safe Palette + Qualitative Research Standards
# ============================================================================

# High-contrast, publication-ready colors
research_palette <- c(
  "Individual Donors" = "#1B9E77",           # Teal (distinct, professional)
  "Government Subventions" = "#D95F02",      # Orange-brown (public funding)
  "For-Profit Entities" = "#7570B3",         # Purple (corporate/business)
  "Party/Corporate/Self" = "#E7298A",        # Magenta (political parties)
  "Civil Society Organizations" = "#66A61E", # Green (non-profits)
  "Associations" = "#E6AB02",                # Gold/Yellow (member-based)
  "Political Fundraising Vehicles" = "#A6761D", # Brown (specialized)
  "Other" = "#999999"                        # Gray (residual)
)

# CREATE VERTICAL 100% STACKED BAR CHART
p <- ggplot(chart_data, aes(x = party, y = percentage, fill = donor_category)) +
  # Stacked bar geometry
  geom_bar(stat = "identity", position = "stack", width = 0.65,
           color = "white", size = 0.3, alpha = 0.95) +

  # ADD PERCENTAGE LABELS ON EACH STACKED SECTION
  geom_text(aes(label = ifelse(percentage > 4, paste0(round(percentage, 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.8,
            fontface = "bold",
            color = "white",
            family = "sans") +

  # Scale formatting
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0, 0.02)),
    breaks = seq(0, 100, by = 20),
    limits = c(0, 105)
  ) +

  # Professional color palette
  scale_fill_manual(
    values = research_palette,
    name = "Donor Category",
    guide = guide_legend(
      ncol = 2,
      title.position = "top",
      label.position = "bottom",
      keywidth = 0.8,
      keyheight = 0.5,
      reverse = FALSE
    )
  ) +

  # Labels and titles
  labs(
    title = "Political Donations Composition by Party/Candidate",
    subtitle = "Distribution of donation sources | 100% stacked columns",
    x = "Party / Candidate",
    y = "Share of Total Donations (%)",
    caption = "Source: Australian Electoral Commission (AEC) | Data normalized to UPPERCASE category codes | Chart: Vertical stacked composition"
  ) +

  # Publication-quality theme
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    # Title and subtitle
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "#333333",
      margin = margin(b = 12)
    ),

    # Axis styling
    axis.title.x = element_text(
      size = 11,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 11,
      face = "bold",
      margin = margin(r = 10)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#333333",
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.text.y = element_text(
      size = 10,
      color = "#333333"
    ),
    axis.line = element_line(color = "#333333", size = 0.3),

    # Grid
    panel.grid.major.y = element_line(color = "#EBEBEB", size = 0.3, linetype = "solid"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

    # Legend
    legend.position = "bottom",
    legend.title = element_text(
      size = 10,
      face = "bold",
      hjust = 0.5
    ),
    legend.text = element_text(size = 9, color = "#333333"),
    legend.box = "vertical",
    legend.margin = margin(t = 15),

    # Plot area
    plot.caption = element_text(
      size = 8,
      color = "#666666",
      hjust = 0,
      margin = margin(t = 10)
    ),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
  )

# Display chart
print(p)
cat("✅ Vertical chart created successfully!\n\n")

# ============================================================================
# EXPORT PUBLICATION-QUALITY FILES
# ============================================================================

# HIGH-RESOLUTION PNG (for presentations, web)
# ggsave("Political_Donations_Vertical_100pct_Stacked.png",
       # p,
       # width = 12,
       # height = 8,
       # dpi = 600,
       # bg = "white")


cat("✅ EXPORT COMPLETE:\n")
cat("   📊 .PNG  (600 DPI) - For presentations & web\n")



# ============================================================================
# HYPOTHESIS TESTING VISUALIZATION
# H1a: Minor parties derive higher individual-donor share than independents
# ============================================================================

cat("HYPOTHESIS TEST H1a VISUALIZATION\n")
cat("H1a: Individual Donor Share - Minor Parties vs Independents vs Major Parties\n\n")

# Step 1: Group parties by type
hypothesis_data <- chart_data %>%
  # Collapse to party type level (aggregate across all parties within each group)
  group_by(party_type, donor_category) %>%
  summarise(
    percentage = mean(percentage),  # Average composition within party type
    .groups = 'drop'
  ) %>%
  # Create aggregated party groups for hypothesis testing
  mutate(
    party_group = case_when(
      party_type == "Major Party" ~ "Major Parties\n(ALP, LPA, Nationals)",
      party_type == "Minor Party" ~ "Minor Parties\n(Greens, ONP, AJP, ACP, KAP, Shooters)",
      party_type == "Independent" ~ "Independents\n(Wilkie, Haines McGowan)",
      TRUE ~ party_type
    ),
    party_group = factor(party_group,
                         levels = c("Major Parties\n(ALP, LPA, Nationals)",
                                    "Minor Parties\n(Greens, ONP, AJP, ACP, KAP, Shooters)",
                                    "Independents\n(Wilkie, Haines McGowan)"))
  ) %>%
  select(party_group, donor_category, percentage)

cat("Hypothesis test data prepared:\n")
print(hypothesis_data)
cat("\n")

# Step 2: Calculate individual donor share by party type (for hypothesis testing)
individual_donor_share <- hypothesis_data %>%
  filter(donor_category == "Individual Donors") %>%
  arrange(party_group)

cat("HYPOTHESIS H1a RESULTS:\n")
cat(strrep("=", 70), "\n")
cat("Individual Donor Share by Party Type:\n")
print(individual_donor_share)
cat("\n")

# Interpretation
major_ind <- individual_donor_share %>% filter(grepl("Major", party_group)) %>% pull(percentage)
minor_ind <- individual_donor_share %>% filter(grepl("Minor", party_group)) %>% pull(percentage)
indep_ind <- individual_donor_share %>% filter(grepl("Indep", party_group)) %>% pull(percentage)

cat("COMPARISON:\n")
cat(sprintf("  • Major Parties: %.1f%% from Individual Donors\n", major_ind))
cat(sprintf("  • Minor Parties: %.1f%% from Individual Donors\n", minor_ind))
cat(sprintf("  • Independents:  %.1f%% from Individual Donors\n\n", indep_ind))

if (minor_ind > indep_ind) {
  cat("✅ SUPPORTS H1a: Minor parties (", sprintf("%.1f%%", minor_ind),
      ") > Independents (", sprintf("%.1f%%", indep_ind), ")\n", sep = "")
  cat("   Difference: ", sprintf("%.1f percentage points\n", minor_ind - indep_ind), sep = "")
} else {
  cat("❌ CONTRADICTS H1a: Minor parties (", sprintf("%.1f%%", minor_ind),
      ") ≤ Independents (", sprintf("%.1f%%", indep_ind), ")\n", sep = "")
  cat("   Difference: ", sprintf("%.1f percentage points\n", indep_ind - minor_ind), sep = "")
}
cat("\n")

# # ============================================================================
# # CREATE HYPOTHESIS TEST VISUALIZATION (3 PARTY TYPES)
# # ============================================================================
# 
# p_h1a <- ggplot(hypothesis_data, aes(x = party_group, y = percentage, fill = donor_category)) +
#   geom_bar(stat = "identity", position = "stack", width = 0.60,
#            color = "white", size = 0.4, alpha = 0.95) +
# 
#   # Scale formatting
#   scale_y_continuous(
#     labels = scales::percent_format(scale = 1, accuracy = 1),
#     expand = expansion(mult = c(0, 0.02)),
#     breaks = seq(0, 100, by = 20),
#     limits = c(0, 105)
#   ) +
# 
#   # Professional color palette (Paul Tol Colorblind-Safe)
#   scale_fill_manual(
#     values = research_palette,
#     name = "Donor Category",
#     guide = guide_legend(
#       ncol = 2,
#       title.position = "top",
#       label.position = "bottom",
#       keywidth = 0.8,
#       keyheight = 0.5
#     )
#   ) +
# 
#   # Labels and titles
#   labs(
#     title = "Hypothesis H1a: Individual Donor Share Across Party Types",
#     subtitle = "Are minor parties more reliant on individual donors than independents? | Mean composition by party classification",
#     x = "Party Classification",
#     y = "Share of Total Donations (%)",
#     caption = "Source: Australian Electoral Commission (AEC) | Method: Mean composition within each party type group"
#   ) +
# 
#   # Academic theme
#   theme_minimal(base_size = 11, base_family = "sans") +
#   theme(
#     plot.title = element_text(
#       size = 15,
#       face = "bold",
#       hjust = 0.5,
#       margin = margin(b = 4)
#     ),
#     plot.subtitle = element_text(
#       size = 11,
#       hjust = 0.5,
#       color = "#333333",
#       margin = margin(b = 14)
#     ),
#     axis.title.x = element_text(
#       size = 11,
#       face = "bold",
#       margin = margin(t = 12)
#     ),
#     axis.title.y = element_text(
#       size = 11,
#       face = "bold",
#       margin = margin(r = 12)
#     ),
#     axis.text.x = element_text(
#       size = 10,
#       color = "#333333",
#       vjust = 0.5
#     ),
#     axis.text.y = element_text(
#       size = 10,
#       color = "#333333"
#     ),
#     axis.line = element_line(color = "#333333", size = 0.3),
#     panel.grid.major.y = element_line(color = "#EBEBEB", size = 0.3),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank(),
#     legend.position = "bottom",
#     legend.title = element_text(size = 10, face = "bold", hjust = 0.5),
#     legend.text = element_text(size = 9, color = "#333333"),
#     legend.box = "vertical",
#     legend.margin = margin(t = 15),
#     plot.caption = element_text(size = 8, color = "#666666", hjust = 0, margin = margin(t = 10)),
#     plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
#   )
# ============================================================================
# CREATE HYPOTHESIS TEST VISUALIZATION (3 PARTY TYPES)
# ============================================================================

p_h1a <- ggplot(hypothesis_data, aes(x = party_group, y = percentage, fill = donor_category)) +
  geom_bar(stat = "identity", position = "stack", width = 0.60,
           color = "white", size = 0.4, alpha = 0.95) +
  
  # --- NEW: ADD PERCENTAGE LABELS FOR ALL SECTIONS ---
  # We use > 0 so exact zeroes are hidden, but even 0.1% will be shown.
  # Size is reduced to 2.8 to help fit text into the smallest slivers.
  geom_text(aes(label = ifelse(percentage > 0, paste0(round(percentage, 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 2, 
            fontface = "bold",
            color = "black",
            family = "sans") +
  
  # Scale formatting
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0, 0.02)),
    breaks = seq(0, 100, by = 20),
    limits = c(0, 105)
  ) +
  
  # Professional color palette (Paul Tol Colorblind-Safe)
  scale_fill_manual(
    values = research_palette,
    name = "Donor Category",
    guide = guide_legend(
      ncol = 2,
      title.position = "top",
      label.position = "bottom",
      keywidth = 0.8,
      keyheight = 0.5
    )
  ) +
  
  # Labels and titles
  labs(
    title = "Hypothesis H1a: Individual Donor Share Across Party Types",
    subtitle = "Are minor parties more reliant on individual donors than independents? | Mean composition by party classification",
    x = "Party Classification",
    y = "Share of Total Donations (%)",
    caption = "Source: Australian Electoral Commission (AEC) | Method: Mean composition within each party type group"
  ) +
  
  # Academic theme
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#333333", margin = margin(b = 14)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 12)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 12)),
    axis.text.x = element_text(size = 10, color = "#333333", vjust = 0.5),
    axis.text.y = element_text(size = 10, color = "#333333"),
    axis.line = element_line(color = "#333333", size = 0.3),
    panel.grid.major.y = element_line(color = "#EBEBEB", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 9, color = "#333333"),
    legend.box = "vertical",
    legend.margin = margin(t = 15),
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
  )
print(p_h1a)

# Save hypothesis test chart
ggsave("Hypothesis_H1a_Individual_Donor_Share.png",
       p_h1a,
       width = 12,
       height = 8,
       dpi = 300,
       bg = "white")
# ggsave("Hypothesis_H1a_Individual_Donor_Share.pdf",
#        p_h1a,
#        width = 12,
#        height = 8,
#        bg = "white")

cat("✅ Hypothesis H1a visualization saved!\n")
cat("   📊 Hypothesis_H1a_Individual_Donor_Share.png\n")
cat("   📄 Hypothesis_H1a_Individual_Donor_Share.pdf\n\n")

# ============================================================================
# ADDITIONAL HYPOTHESIS METRICS TABLE
# ============================================================================

cat("\nDETAILED COMPOSITION TABLE (By Party Type):\n")
cat(strrep("=", 80), "\n")

composition_table <- hypothesis_data %>%
  pivot_wider(
    names_from = donor_category,
    values_from = percentage,
    values_fill = 0
  ) %>%
  mutate(across(-party_group, ~round(., 1))) %>%
  select(party_group, starts_with("Individual"), starts_with("Government"),
         starts_with("For-Profit"), everything())

print(composition_table)

# Statistical comparison
cat("\n\nHYPOTHESIS TESTING SUMMARY:\n")
cat(strrep("=", 80), "\n")
cat("H1:  Non-major actors derive higher share from individuals & public subventions\n")
cat("H1a: Minor parties > Independents on individual-donor share\n\n")
cat("RESULT: ",
    if_else(minor_ind > indep_ind, "✅ SUPPORTED", "❌ NOT SUPPORTED"), "\n\n")
