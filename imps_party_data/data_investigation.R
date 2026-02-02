# VERSION 1:
# install.packages("readxl")
# library(readxl)
# 
# path <- "Major Parties Finance Worksheet.xlsx"
# print(excel_sheets(path))
# # Use lapply to read each sheet into a list
# all_sheets <- lapply(excel_sheets(path), function(x) read_excel(path, sheet = x))
# 
# # Name the list elements so you know which data frame is which
# names(all_sheets) <- excel_sheets(path)
# print(all_sheets)



# VERSION 2: UNDERSTAND THE FILE STRUCTURE AND SHEET STRUCTURE
library(readxl)

# 1. Get the list of all Excel files
file_list <- list.files(pattern = "*.xlsx")

# 2. Iterate through files and extract headers for valid sheets
all_headers <- lapply(file_list, function(f) {

  # Get all sheet names in this file
  sheets <- excel_sheets(f)

  # Filter out "Sheet1" and "Code"
  valid_sheets <- sheets[!(sheets %in% c("Sheet1", "Code"))]

  # For each valid sheet, read only the header row
  headers_list <- lapply(valid_sheets, function(s) {
    df_headers <- read_excel(f, sheet = s, n_max = 0)
    return(colnames(df_headers))
  })

  # Name the list by sheet name
  names(headers_list) <- valid_sheets
  return(headers_list)
})

# 3. Name the top-level list by filename
names(all_headers) <- file_list

# 4. Print the result to inspect the column names
print(all_headers)


# VERSION 3: CLASSIFICATION OF SHEETS INTO BUCKETS
# ==============================================================================
# FINAL CONSOLIDATED SCRIPT: POLITICAL FINANCE DATA
# ==============================================================================

# #Load Libraries
# library(readxl)
# 
# # 1. Get the list of all .xlsx files (excluding hidden temp files starting with ~)
# file_list <- list.files(pattern = "^[^~].*\\.xlsx$")
# 
# # 2. Generate the formatted names for all valid sheets
# all_formatted_names <- unlist(lapply(file_list, function(f) {
#   
#   # Get all sheet names in this file
#   sheets <- excel_sheets(f)
#   
#   # Filter out "Sheet1" and "Code"
#   valid_sheets <- sheets[!(sheets %in% c("Sheet1", "Code"))]
#   
#   # Format each as $filename$sheetname
#   # We use backticks in the string to match R's list-access syntax
#   paste0("$`", f, "`$", "$`", valid_sheets, "`")
# }))
# 
# # 3. Categorize these names based on your criteria
# totals_names  <- all_formatted_names[grepl("totals", all_formatted_names, ignore.case = TRUE)]
# public_names  <- all_formatted_names[grepl("public receipts", all_formatted_names, ignore.case = TRUE)]
# detail_names  <- all_formatted_names[!grepl("totals|public receipts", all_formatted_names, ignore.case = TRUE)]
# 
# # 4. Print results and counts
# cat("--- SUMMARY OF SHEETS FOUND ---\n")
# cat("Totals Schema:      ", length(totals_names), "\n")
# cat("Public Receipts:    ", length(public_names), "\n")
# cat("Transaction Detail: ", length(detail_names), "\n\n")
# 
# # View the details list
# print(detail_names)

# VERSION 4: Extract and Summarize Unique Header Sets
# # 1. Get the column names for every sheet in your detail_list
# detail_headers <- lapply(detail_list, colnames)
# 
# # 2. Find unique combinations of headers
# unique_schemas <- unique(detail_headers)
# 
# # 3. Print the unique schemas found
# cat("Found", length(unique_schemas), "unique column structures in the Transaction Detail category.\n")
# print(unique_schemas)
# 
# 
# ####################### 1. Extract and Count Distinct Donors
# library(dplyr)
# library(purrr)
# 
# # Function to extract names from any column containing "Name"
# extract_donor_names <- function(df) {
#   # Find the first column name that contains the word "Name" (case-insensitive)
#   name_col <- grep("Name", colnames(df), ignore.case = TRUE, value = TRUE)[1]
#   
#   if (!is.na(name_col)) {
#     return(df[[name_col]])
#   } else {
#     return(NULL)
#   }
# }
# 
# # Get all donor names from every sheet in detail_list
# all_donations <- unlist(lapply(detail_list, extract_donor_names))
# 
# # Clean: Remove NA values and empty strings
# all_donations <- all_donations[!is.na(all_donations) & all_donations != ""]
# 
# # Create a distinct set of donors
# distinct_donors <- unique(all_donations)
# 
# # Print Summary
# cat("Total donation records found: ", length(all_donations), "\n")
# cat("Total distinct donors identified: ", length(distinct_donors), "\n")
# 
# ######################### 2. Export the Distinct Donor List
# # Create a data frame of the 2,202 distinct donors
# # Sorting them alphabetically makes it much easier to spot fuzzy matches
# donor_audit_list <- data.frame(Donor_Name = sort(distinct_donors))
# 
# # Save to CSV for your investigation
# write.csv(donor_audit_list, "distinct_donors_for_matching.csv", row.names = FALSE)
# 
# cat("Success: The list of 2,202 donors has been saved to 'distinct_donors_for_matching.csv'.")
# 
# 
# 
# # VERSION 5: YEAR RANGE FOR THE SET
# library(stringr)
# 
# # 1. Extract all year strings from the "Year/Yr" columns across your detail_list
# raw_year_strings <- unlist(lapply(detail_list, function(df) {
#   col <- grep("Year|Yr", colnames(df), ignore.case = TRUE, value = TRUE)[1]
#   as.character(df[[col]])
# }))
# 
# # 2. Extract all 4-digit numbers and filter out obvious typos (like '1019')
# # This handles "2022-2023", "2022/2023", and "2023" simultaneously
# all_years_numeric <- as.numeric(unlist(str_extract_all(raw_year_strings, "\\d{4}")))
# valid_years <- all_years_numeric[all_years_numeric > 1900 & !is.na(all_years_numeric)]
# 
# # 3. Calculate distinct set and range
# distinct_years <- sort(unique(valid_years))
# year_range <- range(valid_years)
# 
# # Output Results
# cat("Total Unique Years Identified:", length(distinct_years), "\n")
# cat("Distinct Years:", paste(distinct_years, collapse = ", "), "\n")
# cat("Year Range:", year_range[1], "to", year_range[2], "\n")


## VERSION 5: write normalised-header XLSX copies (same sheets, same sheet names)
suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(stringr)
  library(purrr)
})

# -----------------------------
# Config
# -----------------------------
source_dir <- "."
out_dir <- file.path(source_dir, "NORMALISED_HEADER_FILES")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Only normalise THESE headers (your mapping)
rename_map <- c(
  # Amount
  "Amount"          = "AMOUNT",
  "Donation Amount" = "AMOUNT",
  
  # Year
  "Year"            = "YEAR",
  "Yr"              = "YEAR",
  
  # Donor name
  "Name"            = "DONOR_NAME",
  "Donor"           = "DONOR_NAME",
  
  # Donation count
  "No. Donations"       = "DONATION_COUNT",
  "Number_of_Donations" = "DONATION_COUNT",
  
  # Donor type
  "Type"            = "DONOR_TYPE"
)

# Sheets to ignore everywhere
skip_sheets <- c("Sheet1", "Code")

# -----------------------------
# Helpers
# -----------------------------
normalise_headers <- function(df, rename_map) {
  if (is.null(df) || ncol(df) == 0) return(df)
  
  current <- names(df)
  
  # Rename only exact matches from your mapping (leave the rest unchanged)
  new_names <- ifelse(current %in% names(rename_map), rename_map[current], current)
  
  # If a sheet contains BOTH "Amount" and "Donation Amount" they will collide after renaming.
  # Make them unique but keep meaning (rare, but safe).
  new_names <- make.unique(new_names, sep = "_DUP_")
  
  names(df) <- new_names
  df
}

# -----------------------------
# Main: create normalised-copy XLSX files
# -----------------------------
xlsx_files <- list.files(source_dir, pattern = "\\.xlsx$", full.names = TRUE)

for (f in xlsx_files) {
  sheets <- excel_sheets(f)
  valid_sheets <- sheets[!(sheets %in% skip_sheets)]
  
  # Create output workbook
  wb <- createWorkbook()
  
  # For each sheet: read entire sheet, rename headers, write back
  for (s in valid_sheets) {
    df <- read_excel(f, sheet = s)
    
    df2 <- normalise_headers(df, rename_map)
    
    addWorksheet(wb, s)
    writeData(wb, sheet = s, x = df2)
  }
  
  out_file <- file.path(out_dir, basename(f))
  saveWorkbook(wb, out_file, overwrite = TRUE)
}

message("Done. Normalised XLSX copies written to: ", normalizePath(out_dir))

