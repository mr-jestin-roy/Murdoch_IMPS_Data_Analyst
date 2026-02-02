# ============================================================================
# H1b GROUPED BAR CHART: NORMALIZED INDIVIDUAL DONOR COUNTS
# ============================================================================
# H1b: Minor parties attract a higher SHARE OF SMALL DONORS (by count)
#      than independents
#
# Focus: Categories 1, 1A, 1B, 1C combined
# Visualization: Grouped (dodged) bar chart with NORMALIZED counts
# Normalization: Scale each party type to 0-100 to avoid skew from large counts
# ============================================================================

# Load required packages
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

cat("\n")
cat(strrep("=", 80), "\n")
cat("H1b: GROUPED BAR CHART (NORMALIZED DONOR COUNTS)\n")
cat("Individual Donors (Categories 1, 1A, 1B, 1C Combined)\n")
cat(strrep("=", 80), "\n\n")

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

cat("STEP 1: LOADING POLITICAL DONATIONS DATA\n")

excel_files <- list(
  list(file = "NORMALISED_HEADER_FILES/ALP_Combined.xlsx", sheet = "ALP (Combined)", party = "ALP"),
  list(file = "NORMALISED_HEADER_FILES/LPA_Combined.xlsx", sheet = "LPA (Combined)", party = "LPA"),
  list(file = "NORMALISED_HEADER_FILES/Nationals_Combined.xlsx", sheet = "Nationals (Combined)", party = "Nationals"),
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
      party %in% c("ONP", "AJP", "ACP", "KAP", "Shooters") ~ "Minor Party",
      party %in% c("Wilkie", "Haines McGowan") ~ "Independent",
      TRUE ~ "Unknown"
    )
  )

# ============================================================================
# STEP 2: FILTER FOR INDIVIDUAL DONOR CATEGORIES (1, 1A, 1B, 1C)
# ============================================================================

cat("STEP 2: FILTERING FOR INDIVIDUAL DONORS (Categories 1, 1A, 1B, 1C)\n")
cat(strrep("-", 80), "\n")

# Filter for Categories 1, 1A, 1B, 1C records only
individual_donors <- all_data %>%
  filter(category %in% c("1", "1A", "1B", "1C"))

cat("Total individual donors found:", nrow(individual_donors), "\n")
cat("Total amount (1+1A+1B+1C):", round(sum(individual_donors$amount, na.rm = TRUE), 0), "\n")
cat("Average donation:", round(mean(individual_donors$amount, na.rm = TRUE), 2), "\n\n")

# ============================================================================
# STEP 3: COUNT DONORS BY PARTY TYPE
# ============================================================================

cat("STEP 3: COUNTING INDIVIDUAL DONORS BY PARTY TYPE\n")
cat(strrep("-", 80), "\n")

# Count number of individual donors per party type
donor_count_by_party_type <- individual_donors %>%
  group_by(party_type, party) %>%
  summarise(
    donor_count = n(),
    total_amount = sum(amount, na.rm = TRUE),
    avg_donation = mean(amount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(party_type, desc(donor_count))

cat("Breakdown by Party (Count of Individual Donors):\n")
print(
  donor_count_by_party_type %>%
    mutate(
      total_amount = round(total_amount, 0),
      avg_donation = round(avg_donation, 0)
    )
)
cat("\n")

# Aggregate by party type
donor_count_summary <- donor_count_by_party_type %>%
  group_by(party_type) %>%
  summarise(
    total_donors = sum(donor_count),
    n_parties = n(),
    total_amount = sum(total_amount),
    avg_donors_per_party = round(mean(donor_count), 1),
    .groups = 'drop'
  ) %>%
  mutate(
    # Calculate percentage of ALL individual donors
    total_all_donors = sum(total_donors),
    pct_of_all_donors = (total_donors / total_all_donors) * 100
  ) %>%
  select(party_type, total_donors, pct_of_all_donors, n_parties, 
         avg_donors_per_party, total_amount)

cat("Aggregated by Party Type (INDIVIDUAL DONOR COUNT):\n")
cat(strrep("-", 80), "\n")
print(
  donor_count_summary %>%
    mutate(
      pct_of_all_donors = round(pct_of_all_donors, 1),
      avg_donors_per_party = round(avg_donors_per_party, 1),
      total_amount = round(total_amount, 0)
    ) %>%
    select(party_type, total_donors, pct_of_all_donors, n_parties, avg_donors_per_party, total_amount)
)
cat("\n")

# ============================================================================
# STEP 4: PREPARE DATA FOR GROUPED BAR CHART WITH NORMALIZATION
# ============================================================================

cat("STEP 4: PREPARING DATA FOR GROUPED BAR CHART (WITH NORMALIZATION)\n")
cat(strrep("-", 80), "\n")

# Create visualization data - normalize counts to 0-100 scale within each party type
h1b_viz_data_grouped <- donor_count_by_party_type %>%
  group_by(party_type) %>%
  # Normalize: scale to 0-100 within each party type
  mutate(
    normalized_count = (donor_count / max(donor_count)) * 100,
    party_type = factor(party_type, 
                        levels = c("Major Party", "Minor Party", "Independent"))
  ) %>%
  ungroup() %>%
  arrange(party_type, desc(normalized_count)) %>%
  mutate(party = factor(party, levels = unique(party)))

cat("Visualization data ready (normalized):\n")
print(
  h1b_viz_data_grouped %>%
    select(party_type, party, donor_count, normalized_count) %>%
    mutate(normalized_count = round(normalized_count, 1))
)
cat("\n")

# ============================================================================
# STEP 5: CREATE GROUPED BAR CHART
# ============================================================================

cat("STEP 5: CREATING GROUPED BAR CHART\n")
cat(strrep("-", 80), "\n")

# Color palette for party types
party_type_palette <- c(
  "Major Party" = "#1B9E77",       # Teal
  "Minor Party" = "#D95F02",       # Orange
  "Independent" = "#7570B3"        # Purple
)

# Create the grouped bar chart
p_h1b_grouped <- ggplot(h1b_viz_data_grouped, 
                        aes(x = party, y = normalized_count, fill = party_type)) +
  
  # Grouped (dodged) bars
  geom_bar(stat = "identity", position = "dodge", width = 0.7,
           color = "white", linewidth = 0.4, alpha = 0.95) +
  
  # ADD TEXT LABELS showing ACTUAL donor counts on top of bars
  geom_text(aes(label = donor_count),
            position = position_dodge(width = 0.7),
            vjust = -0.3,
            size = 3.2,
            fontface = "bold",
            color = "#333333")
  
  # Scale formatting
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05)),
    breaks = seq(0, 100, by = 20),
    limits = c(0, 110)
  ) +
  
  # Color palette
  scale_fill_manual(
    values = party_type_palette,
    name = "Party Type",
    guide = guide_legend(
      ncol = 1,
      title.position = "top",
      label.position = "right",
      keywidth = 0.8,
      keyheight = 0.6,
      reverse = FALSE
    )
  ) +
  
  # Labels
  labs(
    title = "H1b: Individual Donor Count by Political Actor (Normalized)",
    subtitle = "Grouped bar chart | Normalized to 0-100 scale within each party type to prevent skew",
    x = "Political Party / Candidate",
    y = "Normalized Count (0-100 scale within party type)",
    caption = "Source: AEC Political Donations Data | Metric: COUNT of individual donor records (1, 1A, 1B, 1C)\nNormalization: Within each party type, the party with most donors = 100%, others scaled proportionally\nH1b Test: Do Minor Parties attract higher share of individual donors than Independents?"
  ) +
  
  # Theme
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#333333", margin = margin(b = 12)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 10, color = "#333333", angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 10, color = "#333333"),
    axis.line = element_line(color = "#333333", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "#EBEBEB", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10, color = "#333333"),
    legend.margin = margin(l = 20),
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
  )

print(p_h1b_grouped)

# Save chart
ggsave("H1b_Individual_Donors_Grouped_Normalized.png", 
       p_h1b_grouped, 
       width = 14, 
       height = 8, 
       dpi = 300,
       bg = "white")

ggsave("H1b_Individual_Donors_Grouped_Normalized.pdf", 
       p_h1b_grouped, 
       width = 14, 
       height = 8,
       bg = "white")

cat("✅ Grouped bar chart saved!\n")
cat("   📊 H1b_Individual_Donors_Grouped_Normalized.png (300 DPI)\n")
cat("   📄 H1b_Individual_Donors_Grouped_Normalized.pdf\n\n")

# ============================================================================
# STEP 6: DETAILED BREAKDOWN BY INDIVIDUAL ACTOR
# ============================================================================

cat("STEP 6: DETAILED BREAKDOWN BY INDIVIDUAL ACTOR\n")
cat(strrep("-", 80), "\n")

actor_detail <- donor_count_by_party_type %>%
  arrange(party_type, desc(donor_count)) %>%
  mutate(
    party_type_order = case_when(
      party_type == "Major Party" ~ 1,
      party_type == "Minor Party" ~ 2,
      party_type == "Independent" ~ 3
    )
  ) %>%
  arrange(party_type_order, desc(donor_count))

print(
  actor_detail %>%
    mutate(
      total_amount = round(total_amount, 0),
      avg_donation = round(avg_donation, 0)
    ) %>%
    select(party_type, party, donor_count, total_amount, avg_donation)
)
cat("\n")

# ============================================================================
# STEP 7: EXPORT TO EXCEL
# ============================================================================

cat("STEP 7: EXPORTING RESULTS TO EXCEL\n")
cat(strrep("-", 80), "\n")

write_xlsx(
  list(
    "H1b_Summary_by_Type" = donor_count_summary %>%
      mutate(
        pct_of_all_donors = round(pct_of_all_donors, 1),
        total_amount = round(total_amount, 0)
      ) %>%
      select(party_type, total_donors, pct_of_all_donors, n_parties, 
             avg_donors_per_party, total_amount) %>%
      rename(
        "Party Type" = party_type,
        "Count of Individual Donors" = total_donors,
        "% of All Donors" = pct_of_all_donors,
        "# of Parties" = n_parties,
        "Avg Donors/Party" = avg_donors_per_party,
        "Total Amount ($)" = total_amount
      ),
    
    "Detail_by_Actor" = actor_detail %>%
      mutate(
        total_amount = round(total_amount, 0),
        avg_donation = round(avg_donation, 0)
      ) %>%
      select(party_type, party, donor_count, total_amount, avg_donation) %>%
      rename(
        "Party Type" = party_type,
        "Actor" = party,
        "Donor Count" = donor_count,
        "Total Amount ($)" = total_amount,
        "Avg Donation ($)" = avg_donation
      ),
    
    "Grouped_Chart_Data" = h1b_viz_data_grouped %>%
      select(party_type, party, donor_count, normalized_count) %>%
      mutate(normalized_count = round(normalized_count, 1)) %>%
      rename(
        "Party Type" = party_type,
        "Actor" = party,
        "Donor Count (Actual)" = donor_count,
        "Normalized Count (0-100)" = normalized_count
      )
  ),
  "H1b_Grouped_Normalized_Analysis.xlsx"
)

cat("✅ Results exported: H1b_Grouped_Normalized_Analysis.xlsx\n\n")

# ============================================================================
# STEP 8: HYPOTHESIS H1b TEST RESULTS
# ============================================================================

cat(strrep("=", 80), "\n")
cat("HYPOTHESIS H1b TEST RESULTS\n")
cat("(Donor COUNT - Categories 1, 1A, 1B, 1C Combined)\n")
cat(strrep("=", 80), "\n\n")

major_donors <- donor_count_summary %>% filter(party_type == "Major Party") %>% pull(total_donors)
major_pct <- donor_count_summary %>% filter(party_type == "Major Party") %>% pull(pct_of_all_donors)

minor_donors <- donor_count_summary %>% filter(party_type == "Minor Party") %>% pull(total_donors)
minor_pct <- donor_count_summary %>% filter(party_type == "Minor Party") %>% pull(pct_of_all_donors)

indep_donors <- donor_count_summary %>% filter(party_type == "Independent") %>% pull(total_donors)
indep_pct <- donor_count_summary %>% filter(party_type == "Independent") %>% pull(pct_of_all_donors)

cat("INDIVIDUAL DONOR COUNT (Categories 1, 1A, 1B, 1C) - Distribution Across Party Types:\n")
cat(strrep("-", 80), "\n")
cat(sprintf("Major Parties:    %d donors (%.1f%% of all individual donors)\n", major_donors, major_pct))
cat(sprintf("Minor Parties:    %d donors (%.1f%% of all individual donors)\n", minor_donors, minor_pct))
cat(sprintf("Independents:     %d donors (%.1f%% of all individual donors)\n\n", indep_donors, indep_pct))

cat("H1b HYPOTHESIS: Minor Parties > Independents on Individual Donor Share\n")
cat(strrep("-", 80), "\n")

if (minor_pct > indep_pct) {
  diff <- minor_pct - indep_pct
  diff_count <- minor_donors - indep_donors
  cat(sprintf("✅ SUPPORTS H1b: Minor Parties (%.1f%%) > Independents (%.1f%%)\n", minor_pct, indep_pct))
  cat(sprintf("   Difference: +%.1f percentage points\n", diff))
  cat(sprintf("   Minor parties attract %.0f more individual donors than independents\n", diff_count))
} else {
  diff <- indep_pct - minor_pct
  diff_count <- indep_donors - minor_donors
  cat(sprintf("❌ CONTRADICTS H1b: Minor Parties (%.1f%%) ≤ Independents (%.1f%%)\n", minor_pct, indep_pct))
  cat(sprintf("   Difference: -%.1f percentage points (Independents higher)\n", diff))
  cat(sprintf("   Independents attract %.0f more individual donors than minor parties\n", diff_count))
}
cat("\n")

# Breakdown by individual party
cat("BREAKDOWN BY INDIVIDUAL PARTY/CANDIDATE (Actual Counts & Normalized):\n")
cat(strrep("-", 80), "\n")
print(
  h1b_viz_data_grouped %>%
    arrange(party_type, desc(donor_count)) %>%
    mutate(
      rank = row_number(),
      normalized_count = round(normalized_count, 1)
    ) %>%
    select(party_type, party, donor_count, normalized_count) %>%
    rename("Type" = party_type, "Actor" = party, 
           "Actual Count" = donor_count, "Normalized (0-100)" = normalized_count)
)
cat("\n")

cat(strrep("=", 80), "\n")
cat("📊 H1b GROUPED BAR CHART ANALYSIS COMPLETE!\n")
cat(strrep("=", 80), "\n")
cat("Key Finding: Grouped (dodged) bars with normalization prevent skewing\n")
cat("Normalization: Within each party type, largest = 100%, others scaled proportionally\n")
cat("\nOutputs:\n")
cat("  1. H1b_Individual_Donors_Grouped_Normalized.png\n")
cat("  2. H1b_Individual_Donors_Grouped_Normalized.pdf\n")
cat("  3. H1b_Grouped_Normalized_Analysis.xlsx\n\n")