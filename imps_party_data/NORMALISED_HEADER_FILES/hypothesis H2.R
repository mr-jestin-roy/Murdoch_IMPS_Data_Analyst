# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# ============================================================================
# STEP 1: READ DATA (Unchanged)
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
    mutate(category_clean = toupper(trimws(as.character(category_raw)))) %>%
    mutate(party = file_info$party)
  all_data <- bind_rows(all_data, df)
  cat("✓ Loaded:", file_info$party, "\n")
}

# ============================================================================
# STEP 2: CLASSIFY AND MAP CODES
# ============================================================================
cat("STEP 2: MAPPING CODES FOR AXIS AND LEGEND\n")

# Revised Function: Returns Short CODES for the Axis (Space Saving)
categorize_code <- function(category) {
  if (is.na(category) || category == "" || category == "NA") return("Cat 10")
  cat_str <- gsub("\\n| ", "", category)

  if (grepl("^1[A-F]?$", cat_str)) return("Cat 1")  # Individuals
  if (cat_str %in% c("2", "2.0"))   return("Cat 2")  # For-Profit
  if (cat_str %in% c("3", "3.0"))   return("Cat 3")  # Civil Society
  if (grepl("^4", cat_str))         return("Cat 4")  # Party/Corp/Self
  if (cat_str %in% c("5", "5.0"))   return("Cat 5")  # Subventions
  if (cat_str %in% c("7", "7.0"))   return("Cat 7")  # Associations
  if (cat_str == "8")              return("Cat 8")  # Fundraising
  return("Cat 10")                                  # Other
}

# Apply classifications
all_data <- all_data %>%
  mutate(
    party_type = case_when(
      party %in% c("ALP", "LPA", "Nationals") ~ "Major Party",
      party %in% c("Greens", "ONP", "AJP", "ACP", "KAP", "Shooters") ~ "Minor Party",
      party %in% c("Wilkie", "Haines McGowan") ~ "Independent",
      TRUE ~ "Unknown"
    ),
    donor_code = sapply(category_clean, categorize_code)
  )

# ============================================================================
# STEP 3: PROFESSIONAL VISUALIZATION
# ============================================================================
library(scales)

# 1. Define Color & Label Mappings (The Legend Logic)
# ---------------------------------------------------
# Colors match your schema
colors_grouped <- c(
  "Cat 1" = "#1f77b4", "Cat 2" = "#ff7f0e", "Cat 3" = "#2ca02c",
  "Cat 4" = "#d62728", "Cat 5" = "#9467bd", "Cat 7" = "#8c564b",
  "Cat 8" = "#e377c2", "Cat 10" = "#7f7f7f"
)

# Detailed Labels for the Legend
labels_grouped <- c(
  "Cat 1" = "Cat 1: Individual Donors",
  "Cat 2" = "Cat 2: For-Profit Entities",
  "Cat 3" = "Cat 3: Civil Society Orgs",
  "Cat 4" = "Cat 4: Party/Corporate/Self",
  "Cat 5" = "Cat 5: Govt Subventions",
  "Cat 7" = "Cat 7: Associations",
  "Cat 8" = "Cat 8: Fundraising Vehicles",
  "Cat 10" = "Cat 10: Other/Unclear"
)

# 2. Aggregation & Label Formatting
# ---------------------------------------------------
currency_format <- function(x) {
  case_when(
    x >= 1e6 ~ paste0("$", round(x / 1e6, 1), "M"),
    x >= 1e3 ~ paste0("$", round(x / 1e3, 0), "k"),
    TRUE ~ paste0("$", format(x, big.mark = ","))
  )
}

plot_data_facet <- all_data %>%
  group_by(party_type, donor_code) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  filter(total_amount > 0) %>%
  mutate(label_text = currency_format(total_amount))

# 3. Generate Plot
# ---------------------------------------------------
p_final <- ggplot(plot_data_facet, aes(x = donor_code, y = total_amount, fill = donor_code)) +
  geom_bar(stat = "identity", width = 0.7) +

  # Data Labels (Raw Values)
  geom_text(
    aes(label = label_text),
    hjust = -0.2,
    size = 2.8,
    fontface = "bold"
  ) +

  # Faceting
  facet_wrap(~party_type, scales = "free_y", ncol = 1) +

  # Colors & Legend
  scale_fill_manual(
    values = colors_grouped,
    labels = labels_grouped,  # Maps codes to full descriptions in legend
    name = "Donor Category Definitions"
  ) +

  # Log Scale Y-Axis
  scale_y_log10(
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0, 0.15))
  ) +

  # Horizontal Bars for readability
  coord_flip() +

  # Professional Titles
  labs(
    title = "Figure 2: Structural Composition of Political Funding Portfolios by Party Classification",
    subtitle = "Log-scaled distribution of donor categories illustrating varying degrees of revenue concentration.",
    x = "Donor Category Code",
    y = "Aggregate Financial Receipts ($AUD, Log10 Scale)",
    caption = "Source: Australian Electoral Commission Periodic Disclosures. \nNote: 'Major Parties' includes ALP/LPA/Nationals; 'Minor Parties' includes Greens/ONP/KAP/Shooters/AJP/ACP. Axis ranges differ by facet."
  ) +

  # Professional Theme
  theme_bw() +
  theme(
    legend.position = "bottom",              # Legend at bottom to save side space
    legend.direction = "horizontal",         # Horizontal layout
    legend.box = "horizontal",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "gray95"),
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.text.y = element_text(size = 9, face = "bold"), # The Category Codes
    panel.grid.minor = element_blank()
  ) +
  # Force legend to multiple rows if needed
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

print(p_final)
# 4. Save High-Res Output
ggsave("H2_Funding_Diversification_Analysis.png", plot = p_final, width = 10, height = 12, dpi = 300)
ggsave("H2_Funding_Diversification_Analysis.pdf", plot = p_final, width = 10, height = 12, bg = "white")

print(p_final)
