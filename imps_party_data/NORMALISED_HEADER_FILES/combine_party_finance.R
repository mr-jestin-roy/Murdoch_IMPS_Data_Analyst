# R Script to Combine ALP and LPA Financial Data
# This script reads the Major-Parties-Finance-Worksheet.xlsx file,
# combines ALP and LPA data vertically across years, and exports to separate XLSX files

# Install required packages if not already installed
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("dplyr")) install.packages("dplyr")

library(readxl)
library(writexl)
library(dplyr)

# Step 1: Read all sheet names
excel_file <- "./NORMALISED_HEADER_FILES/Major Parties Finance Worksheet.xlsx"
sheet_names <- excel_sheets(excel_file)

cat("Available sheets:\n")
print(sheet_names)
cat("\n")

# Step 2: Combine ALP data across years
# Filter for sheets starting with "ALP " and exclude "ALP totals"
alp_sheets <- sheet_names[grepl("^ALP ", sheet_names) & !grepl("totals", sheet_names)]

cat("ALP sheets to combine:\n")
print(alp_sheets)
cat("\n")

# Read and combine all ALP sheets
alp_combined <- data.frame()

for (sheet in alp_sheets) {
  df <- read_excel(excel_file, sheet = sheet)
  alp_combined <- bind_rows(alp_combined, df)
}

cat("ALP Combined Data Summary:\n")
cat("  Rows:", nrow(alp_combined), "\n")
cat("  Columns:", colnames(alp_combined), "\n")
cat("  Years:", unique(alp_combined$YEAR), "\n\n")

# Step 3: Combine LPA data across years
# Filter for sheets starting with "LPA"
lpa_sheets <- sheet_names[grepl("^LPA", sheet_names)]

cat("LPA sheets to combine:\n")
print(lpa_sheets)
cat("\n")

# Read and combine all LPA sheets
lpa_combined <- data.frame()

for (sheet in lpa_sheets) {
  df <- read_excel(excel_file, sheet = sheet)
  lpa_combined <- bind_rows(lpa_combined, df)
}

cat("LPA Combined Data Summary:\n")
cat("  Rows:", nrow(lpa_combined), "\n")
cat("  Columns:", colnames(lpa_combined), "\n")
cat("  Years:", unique(lpa_combined$YEAR), "\n\n")

# Step 4: Combine Nationals data across years
# Filter for sheets starting with "Nationals" and exclude "Nationals totals"
nationals_sheets <- sheet_names[grepl("^Nationals", sheet_names) & !grepl("totals", sheet_names)]

cat("Nationals sheets to combine:\n")
print(nationals_sheets)
cat("\n")

# Read and combine all Nationals sheets
nationals_combined <- data.frame()

for (sheet in nationals_sheets) {
  df <- read_excel(excel_file, sheet = sheet)
  nationals_combined <- bind_rows(nationals_combined, df)
}

cat("Nationals Combined Data Summary:\n")
cat("  Rows:", nrow(nationals_combined), "\n")
cat("  Columns:", colnames(nationals_combined), "\n")
cat("  Years:", unique(nationals_combined$YEAR), "\n\n")

# Step 5: Export to separate XLSX files
write_xlsx(alp_combined, "./NORMALISED_HEADER_FILES/ALP_Combined.xlsx")
write_xlsx(lpa_combined, "./NORMALISED_HEADER_FILES/LPA_Combined.xlsx")
write_xlsx(nationals_combined, "./NORMALISED_HEADER_FILES/Nationals_Combined.xlsx")


cat("✓ All files successfully created!\n")
cat("1. ALP_Combined.xlsx - ", nrow(alp_combined), " rows\n")
cat("2. LPA_Combined.xlsx - ", nrow(lpa_combined), " rows\n")
cat("3. Nationals_Combined.xlsx - ", nrow(nationals_combined), " rows\n")
cat("\nAll files are ready in your working directory!\n")

# Optional: Display first few rows of each combined dataset
cat("\n" , "="*80, "\n")
cat("ALP Combined - First 5 rows:\n")
cat("="*80, "\n")
print(head(alp_combined, 5))

cat("\n", "="*80, "\n")
cat("LPA Combined - First 5 rows:\n")
cat("="*80, "\n")
print(head(lpa_combined, 5))

cat("\n", "="*80, "\n")
cat("Nationals Combined - First 5 rows:\n")
cat("="*80, "\n")
print(head(nationals_combined, 5))