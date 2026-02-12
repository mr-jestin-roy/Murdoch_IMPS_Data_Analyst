
# ============================================================================
# H1b: SMALL DONORS (Categories 1A, 1B, 1C) - GROUPED BAR CHART
# ============================================================================
# H1b: Minor parties attract a higher SHARE OF SMALL DONORS (by count)
#      than independents
#
# Focus: Individual donor subcategories 1A, 1B, 1C
# Question: How do small donor counts compare across party types?
#   - Major Parties (Combined)
#   - Minor Parties (Combined)
#   - Independents (Combined)
#
# Visualization: Grouped bar chart showing donor counts by subcategory
# ============================================================================

# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

cat("\n")
cat(strrep("=", 80), "\n")
cat("H1b: SMALL DONOR COUNT ANALYSIS (Categories 1A, 1B, 1C)\n")
cat("Grouped bar chart: Donor counts by subcategory and party type\n")
cat(strrep("=", 80), "\n\n")

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

cat("STEP 1: LOADING POLITICAL DONATIONS DATA\n")

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
    mutate(category = toupper(trimws(as.character(category_raw)))) %>%
    select(-category_raw) %>%
    mutate(party = file_info$party)

  all_data <- bind_rows(all_data, df)
  cat("✓ Loaded:", file_info$party, "-", nrow(df), "records\n")
}

cat("\nTotal records loaded:", nrow(all_data), "\n\n")

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

# ============================================================================
# STEP 2: FILTER FOR SMALL DONOR CATEGORIES (1A, 1B, 1C)
# ============================================================================

cat("STEP 2: FILTERING FOR SMALL DONORS (Categories 1A, 1B, 1C)\n")
cat(strrep("-", 80), "\n")

# Filter for Categories 1A, 1B, 1C records only (small donor subcategories)
individual_donors <- all_data %>%
  filter(category %in% c("1A", "1B", "1C"))

cat("Total small donors found:", nrow(individual_donors), "\n")
cat("Total amount (1A+1B+1C):", round(sum(individual_donors$amount, na.rm = TRUE), 0), "\n")
cat("Average donation:", round(mean(individual_donors$amount, na.rm = TRUE), 2), "\n\n")

# Breakdown by category
cat("Breakdown by subcategory:\n")
print(
  individual_donors %>%
    group_by(category) %>%
    summarise(
      count = n(),
      total_amount = sum(amount, na.rm = TRUE),
      avg_amount = mean(amount, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(factor(category, levels = c("1A", "1B", "1C"))) %>%
    mutate(
      pct_of_all = round(count / sum(count) * 100, 1),
      total_amount = round(total_amount, 0),
      avg_amount = round(avg_amount, 0)
    ) %>%
    select(category, count, pct_of_all, total_amount, avg_amount)
)
cat("\n")

# ============================================================================
# STEP 3: COUNT DONORS BY PARTY TYPE AND SUBCATEGORY (KEY METRIC)
# ============================================================================

cat("STEP 3: COUNTING SMALL DONORS BY PARTY TYPE AND SUBCATEGORY\n")
cat(strrep("-", 80), "\n")

# Count number of donors per party type AND subcategory (for grouped bar chart)
donor_count_by_type_category <- individual_donors %>%
  group_by(party_type, category) %>%
  summarise(
    donor_count = n(),
    total_amount = sum(amount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    party_type = factor(party_type, levels = c("Major Party", "Minor Party", "Independent")),
    category = factor(category, levels = c("1A", "1B", "1C"))
  ) %>%
  arrange(party_type, category)

cat("Donor Count by Party Type and Subcategory:\n")
print(
  donor_count_by_type_category %>%
    mutate(total_amount = round(total_amount, 0))
)
cat("\n")

# Also aggregate by party type for summary statistics
donor_count_summary <- individual_donors %>%
  group_by(party_type) %>%
  summarise(
    total_donors = n(),
    total_amount = sum(amount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    total_all_donors = sum(total_donors),
    pct_of_all_donors = (total_donors / total_all_donors) * 100
  )

cat("Summary by Party Type:\n")
cat(strrep("-", 80), "\n")
print(
  donor_count_summary %>%
    mutate(
      pct_of_all_donors = round(pct_of_all_donors, 1),
      total_amount = round(total_amount, 0)
    )
)
cat("\n")

# ============================================================================
# STEP 4: PREPARE DATA FOR GROUPED BAR CHART
# ============================================================================

cat("STEP 4: PREPARING DATA FOR GROUPED BAR VISUALIZATION\n")
cat(strrep("-", 80), "\n")

# The grouped bar data is already prepared in donor_count_by_type_category
h1b_grouped_data <- donor_count_by_type_category

cat("Grouped bar chart data ready:\n")
print(h1b_grouped_data)
cat("\n")

# ============================================================================
#  STEP 5: RESEARCH-GRADE Intra-group Proportional Faceting CHART
# ============================================================================

# Calculate percentages within the plot data for labeling
h1b_plot_data <- h1b_grouped_data %>%
  group_by(party_type) %>%
  mutate(pct = (donor_count / sum(donor_count)) * 100) %>%
  ungroup()

p_h1b_elaborated <- ggplot(h1b_plot_data,
                           aes(x = category, y = donor_count, fill = category)) +

  # Professional bars with custom colors
  geom_bar(stat = "identity", width = 0.75, alpha = 0.9) +

  # Faceting with independent Y-scales (Normalization)
  facet_wrap(~party_type, scales = "free_y") +

  # Elaborated labels: Count + (Percentage)
  geom_text(aes(label = paste0(scales::comma(donor_count), "\n(", round(pct, 1), "%)")),
            vjust = -0.3,
            lineheight = 0.9,
            size = 3.5,
            fontface = "bold") +

  # Formatting
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  scale_fill_manual(values = subcategory_palette) +

  # Labels and Titles
  labs(
    title = "H1b: Small Donor Count & Share by Party Type",
    subtitle = "(Scales Normalized to Highlight Distribution)",
    x = "Donor Subcategory",
    y = "Count",
    caption = "Source: AEC Political Donations Data\nNote: Y-axes are scaled independently to allow comparison of group profiles."
  ) +

  # Minimalist Research Theme
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 14),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed"),
    panel.spacing = unit(2, "lines"),
    legend.position = "none"
  )

print(p_h1b_elaborated)

# Save for your paper
ggsave("H1b_Faceted_Bar_Chart.png", p_h1b_elaborated, width = 11, height = 6, dpi = 300)
ggsave("H1b_Faceted_Bar_Chart.pdf", p_h1b_elaborated, width = 11, height = 6, bg = "white")

# ============================================================================
# STEP 6: DETAILED BREAKDOWN BY SUBCATEGORY AND PARTY TYPE
# ============================================================================

cat("STEP 6: DETAILED BREAKDOWN BY SUBCATEGORY AND PARTY TYPE\n")
cat(strrep("-", 80), "\n")

# Wide format for easy comparison
wide_comparison <- h1b_grouped_data %>%
  tidyr::pivot_wider(
    names_from = category,
    values_from = c(donor_count, total_amount),
    values_fill = 0
  ) %>%
  mutate(
    total_donors = donor_count_1A + donor_count_1B + donor_count_1C
  ) %>%
  arrange(party_type)

print(wide_comparison)
cat("\n")

# ============================================================================
# STEP 7: HYPOTHESIS H1b TEST RESULTS
# ============================================================================

cat(strrep("=", 80), "\n")
cat("HYPOTHESIS H1b TEST RESULTS\n")
cat("(Small Donor COUNT - Categories 1A, 1B, 1C)\n")
cat(strrep("=", 80), "\n\n")

# Step 7a: Show breakdown by subcategory for each party type
cat("STEP 7a: RECEIPT COUNTS BY SUBCATEGORY AND PARTY TYPE\n")
cat(strrep("-", 80), "\n")
print(
  h1b_grouped_data %>%
    arrange(party_type, category) %>%
    select(party_type, category, donor_count)
)
cat("\n")

# Step 7b: Sum subcategory counts for each party type
cat("STEP 7b: SUM OF RECEIPTS (1A + 1B + 1C) BY PARTY TYPE\n")
cat(strrep("-", 80), "\n")

# Calculate sum of subcategory counts for each party type
party_type_totals <- h1b_grouped_data %>%
  group_by(party_type) %>%
  summarise(
    total_receipts = sum(donor_count),
    .groups = 'drop'
  ) %>%
  mutate(
    grand_total = sum(total_receipts),
    pct_of_total = (total_receipts / grand_total) * 100
  ) %>%
  arrange(party_type)

# Extract values for comparison
major_receipts <- party_type_totals %>% filter(party_type == "Major Party") %>% pull(total_receipts)
major_pct <- party_type_totals %>% filter(party_type == "Major Party") %>% pull(pct_of_total)

minor_receipts <- party_type_totals %>% filter(party_type == "Minor Party") %>% pull(total_receipts)
minor_pct <- party_type_totals %>% filter(party_type == "Minor Party") %>% pull(pct_of_total)

indep_receipts <- party_type_totals %>% filter(party_type == "Independent") %>% pull(total_receipts)
indep_pct <- party_type_totals %>% filter(party_type == "Independent") %>% pull(pct_of_total)

cat("Sum of small donor receipts (1A + 1B + 1C) by party type:\n\n")
cat(sprintf("  Major Parties:  1A + 1B + 1C = %d receipts (%.1f%%)\n", major_receipts, major_pct))
cat(sprintf("  Minor Parties:  1A + 1B + 1C = %d receipts (%.1f%%)\n", minor_receipts, minor_pct))
cat(sprintf("  Independents:   1A + 1B + 1C = %d receipts (%.1f%%)\n\n", indep_receipts, indep_pct))

# Step 7c: Hypothesis comparison
cat("STEP 7c: H1b HYPOTHESIS COMPARISON\n")
cat(strrep("-", 80), "\n")
cat("H1b: Minor Parties attract a higher share of small donor receipts than Independents\n\n")

cat(sprintf("  Minor Parties:  %d receipts (%.1f%%)\n", minor_receipts, minor_pct))
cat(sprintf("  Independents:   %d receipts (%.1f%%)\n\n", indep_receipts, indep_pct))

if (minor_pct > indep_pct) {
  diff <- minor_pct - indep_pct
  diff_count <- minor_receipts - indep_receipts
  cat(sprintf("✅ SUPPORTS H1b: Minor Parties (%.1f%%) > Independents (%.1f%%)\n", minor_pct, indep_pct))
  cat(sprintf("   Difference: +%.1f percentage points\n", diff))
  cat(sprintf("   Minor parties attract %d more small donor receipts than independents\n", diff_count))
} else {
  diff <- indep_pct - minor_pct
  diff_count <- indep_receipts - minor_receipts
  cat(sprintf("❌ CONTRADICTS H1b: Minor Parties (%.1f%%) ≤ Independents (%.1f%%)\n", minor_pct, indep_pct))
  cat(sprintf("   Difference: -%.1f percentage points (Independents higher)\n", diff))
  cat(sprintf("   Independents attract %d more small donor receipts than minor parties\n", diff_count))
}
cat("\n")

cat(strrep("=", 80), "\n")
cat("📊 H1b ANALYSIS COMPLETE!\n")
cat(strrep("=", 80), "\n")
cat("Key Finding: H1b focuses on SMALL DONOR COUNT (1A, 1B, 1C)\n")
cat("Question: Which party type attracts the most small individual donors?\n")
cat("\nOutput:\n")
cat("  📊 H1b_Faceted_Bar_Chart.png\n\n")
