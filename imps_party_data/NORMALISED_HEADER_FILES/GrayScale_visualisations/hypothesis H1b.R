# ============================================================================
# H1B: DONOR SIZE DIVERSIFICATION - PATTERNED GRAYSCALE (AJPS COMPLIANT)
# ============================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggpattern)

# --- STEP 1: LOAD AND COMBINE DATA ---
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
    mutate(category = toupper(trimws(as.character(category_raw))), party = file_info$party)
  all_data <- bind_rows(all_data, df)
}

# --- STEP 2: CLASSIFY AND PREPARE PLOT DATA ---
h1b_plot_data <- all_data %>%
  # 1. Create party_type classification
  mutate(
    party_type = case_when(
      party %in% c("ALP", "LPA", "Nationals") ~ "Major Party",
      party %in% c("Greens", "ONP", "AJP", "ACP", "KAP", "Shooters") ~ "Minor Party",
      party %in% c("Wilkie", "Haines McGowan") ~ "Independent",
      TRUE ~ "Unknown"
    )
  ) %>%
  # 2. Filter for specific Individual Donor sizes
  filter(category %in% c("1A", "1B", "1C")) %>%
  
  # 3. Calculate donor counts
  group_by(party_type, category) %>%
  summarise(donor_count = n(), .groups = 'drop') %>%
  
  # 4. Alias Category Names and Set Factor Order
  group_by(party_type) %>%
  mutate(
    pct = (donor_count / sum(donor_count)) * 100,
    # Direct Aliasing: 1A->Small, 1B->Medium, 1C->Large
    donor_size = factor(category, 
                        levels = c("1A", "1B", "1C"), 
                        labels = c("Small", "Medium", "Large")),
    party_type = factor(party_type, 
                        levels = c("Major Party", "Minor Party", "Independent"))
  ) %>%
  ungroup()

# --- STEP 3: DEFINE PATTERNS ---
h1b_pattern_map <- c(
  "Small"  = "none",      
  "Medium" = "stripe",    
  "Large"  = "crosshatch" 
)

# --- STEP 4: VISUALIZATION ---
# Filter out 0 counts to prevent ggpattern calculation errors
h1b_plot_filtered <- h1b_plot_data %>% filter(donor_count > 0)
p_h1b_final <- ggplot(h1b_plot_filtered, aes(x = donor_size, y = donor_count, fill = donor_size)) +
  
  # Patterned Column Layer
  geom_col_pattern(
    aes(pattern = donor_size),
    position = "dodge",
    width = 0.75,
    color = "black",           
    pattern_fill = "black",    
    pattern_color = "black", 
    pattern_density = 0.1,     
    pattern_spacing = 0.05,
    linewidth = 0.3            
  ) +
  
  # Independent scales to allow comparison of group profiles
  facet_wrap(~party_type, scales = "free_y") +
  
  # Intra-group labels: Count + (Percentage)
  geom_text(aes(label = paste0(scales::comma(donor_count), "\n(", round(pct, 1), "%)")),
            vjust = -0.3,
            lineheight = 0.85,
            size = 3.5,
            fontface = "bold") +
  
  # Grayscale and Pattern Scale formatting
  scale_fill_grey(start = 0.9, end = 0.4, name = "Donor Subcategory") +
  scale_pattern_manual(values = h1b_pattern_map, name = "Donor Subcategory") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) + 
  
  # Clean axis labels (Titles and Subtitles removed per feedback)
  labs(x = "Donor Subcategory", y = "Donor Count") +
  
  # Minimalist Research Theme
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 14),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed"),
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom"
  )

# --- STEP 5: EXPORT ---
print(p_h1b_final)
ggsave("Figure_H1B_Patterned_Grayscale.png", p_h1b_final, width = 11, height = 7, dpi = 300)