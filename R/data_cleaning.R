# ==============================================================================
# File: R/data_cleaning.R
# Project: Predicting Customer Churn (College-Level Probability & Statistics)
# Description: Data ingestion, missing value treatment, type conversion, and
#              feature preparation for statistical and probability analysis.
# ==============================================================================

#' Load Raw Telco Customer Churn Dataset
#'
#' Reads the customer churn CSV file, ensuring blank strings are treated as NA.
#'
#' @param file_path Character path to the CSV file.
#' @return A data.frame containing the raw dataset.
load_raw_data <- function(file_path = "data/customer_churn.csv") {
  if (!file.exists(file_path)) {
    stop(paste("Dataset file not found at:", file_path))
  }

  # Read CSV and interpret blank strings or whitespace as NA
  raw_df <- read.csv(
    file = file_path,
    header = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c("NA", "", " ", "  ")
  )

  message(sprintf("Successfully loaded raw dataset: %d rows, %d columns.", nrow(raw_df), ncol(raw_df)))
  return(raw_df)
}

#' Assign Unique Random Human Names to Customers
#'
#' Generates 100% unique full names (First + Last) deterministically for every
#' customerID in the dataset, ensuring no two customers share the same name.
#'
#' @param df Data frame containing customerID.
#' @param seed Integer seed for deterministic generation. Default 42.
#' @return Data frame with CustomerName column added.
assign_unique_customer_names <- function(df, seed = 42) {
  if (!"customerID" %in% names(df)) {
    return(df)
  }

  first_names <- c(
    "James", "Mary", "John", "Patricia", "Robert", "Jennifer", "Michael", "Linda", "William", "Elizabeth",
    "David", "Barbara", "Richard", "Susan", "Joseph", "Jessica", "Thomas", "Sarah", "Charles", "Karen",
    "Christopher", "Nancy", "Daniel", "Lisa", "Matthew", "Betty", "Anthony", "Margaret", "Mark", "Sandra",
    "Donald", "Ashley", "Steven", "Kimberly", "Paul", "Emily", "Andrew", "Donna", "Joshua", "Michelle",
    "Kenneth", "Dorothy", "Kevin", "Carol", "Brian", "Amanda", "George", "Melissa", "Edward", "Deborah",
    "Ronald", "Stephanie", "Timothy", "Rebecca", "Jason", "Sharon", "Jeffrey", "Laura", "Ryan", "Cynthia",
    "Jacob", "Kathleen", "Gary", "Amy", "Nicholas", "Shirley", "Eric", "Angela", "Jonathan", "Helen",
    "Stephen", "Anna", "Larry", "Brenda", "Justin", "Pamela", "Scott", "Nicole", "Brandon", "Emma",
    "Benjamin", "Samantha", "Samuel", "Katherine", "Gregory", "Christine", "Frank", "Debra", "Alexander", "Rachel",
    "Raymond", "Catherine", "Patrick", "Carolyn", "Jack", "Janet", "Dennis", "Ruth", "Jerry", "Maria",
    "Tyler", "Heather", "Aaron", "Diane", "Jose", "Virginia", "Adam", "Julie", "Henry", "Joyce",
    "Nathan", "Victoria", "Douglas", "Olivia", "Zachary", "Kelly", "Peter", "Christina", "Kyle", "Lauren"
  )

  last_names <- c(
    "Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis", "Rodriguez", "Martinez",
    "Hernandez", "Lopez", "Gonzalez", "Wilson", "Anderson", "Thomas", "Taylor", "Moore", "Jackson", "Martin",
    "Lee", "Perez", "Thompson", "White", "Harris", "Sanchez", "Clark", "Ramirez", "Lewis", "Robinson",
    "Walker", "Young", "Allen", "King", "Wright", "Scott", "Torres", "Nguyen", "Hill", "Flores",
    "Green", "Adams", "Nelson", "Baker", "Hall", "Rivera", "Campbell", "Mitchell", "Carter", "Roberts",
    "Gomez", "Phillips", "Evans", "Turner", "Diaz", "Parker", "Cruz", "Edwards", "Collins", "Reyes",
    "Stewart", "Morris", "Morales", "Murphy", "Cook", "Rogers", "Gutierrez", "Ortiz", "Morgan", "Cooper",
    "Peterson", "Bailey", "Reed", "Kelly", "Howard", "Ramos", "Kim", "Cox", "Ward", "Richardson",
    "Watson", "Brooks", "Chavez", "Wood", "James", "Bennett", "Gray", "Mendoza", "Ruiz", "Hughes",
    "Price", "Alvarez", "Castillo", "Sanders", "Patel", "Myers", "Long", "Ross", "Foster", "Jimenez",
    "Powell", "Jenkins", "Perry", "Russell", "Sullivan", "Bell", "Coleman", "Butler", "Henderson", "Barnes",
    "Gonzales", "Fisher", "Vasquez", "Simmons", "Romero", "Jordan", "Patterson", "Alexander", "Hamilton", "Graham"
  )

  all_combos <- as.vector(outer(first_names, last_names, function(f, l) paste(f, l)))
  unique_ids <- unique(df$customerID)

  set.seed(seed)
  unique_assigned <- sample(all_combos, size = length(unique_ids), replace = FALSE)
  id_to_name <- setNames(unique_assigned, unique_ids)
  df$CustomerName <- id_to_name[df$customerID]

  other_cols <- names(df)[!names(df) %in% c("customerID", "CustomerName")]
  df <- df[, c("customerID", "CustomerName", other_cols)]
  return(df)
}

#' Clean and Preprocess Customer Churn Dataset
#'
#' Performs essential data hygiene operations:
#' 1. Identifies and removes duplicate records.
#' 2. Ensures each customer has a unique human name.
#' 3. Converts TotalCharges to numeric and handles missing values.
#' 4. Recodes SeniorCitizen from binary integers (0/1) to readable factors ("No"/"Yes").
#' 5. Converts categorical character columns to factors.
#' 6. Adds derived grouping factors for probability and statistical stratification.
#'
#' @param df Raw data.frame.
#' @param drop_id Logical, whether to drop customerID column. Default FALSE.
#' @return Cleaned data.frame ready for statistical analysis and modeling.
clean_churn_data <- function(df, drop_id = FALSE) {
  # 1. Check and report duplicate customer IDs
  if ("customerID" %in% names(df)) {
    dup_count <- sum(duplicated(df$customerID))
    if (dup_count > 0) {
      warning(sprintf("Found %d duplicate customerID records. Removing duplicates.", dup_count))
      df <- df[!duplicated(df$customerID), ]
    }
  }

  # Ensure each customer has a unique human name
  if (!"CustomerName" %in% names(df) && "customerID" %in% names(df)) {
    df <- assign_unique_customer_names(df)
  }

  # 2. Convert TotalCharges to numeric (handles any remaining character conversions)
  df$TotalCharges <- as.numeric(df$TotalCharges)

  # Check missing values in TotalCharges
  na_total_charges <- sum(is.na(df$TotalCharges))
  if (na_total_charges > 0) {
    # In Telco dataset, NAs in TotalCharges occur where tenure == 0 (brand new customers with no bill yet)
    # Dropping these 11 rows ensures complete cases for statistical analysis
    message(sprintf(
      "Handling missing values: Removed %d rows where TotalCharges was NA (tenure == 0).",
      na_total_charges
    ))
    df <- df[!is.na(df$TotalCharges), ]
  }

  # Ensure tenure and MonthlyCharges are strictly numeric
  df$tenure <- as.numeric(df$tenure)
  df$MonthlyCharges <- as.numeric(df$MonthlyCharges)

  # 3. Recode SeniorCitizen from 0/1 to Factor "No"/"Yes"
  if ("SeniorCitizen" %in% names(df)) {
    df$SeniorCitizen <- factor(
      ifelse(df$SeniorCitizen == 1, "Yes", "No"),
      levels = c("No", "Yes")
    )
  }

  # 4. List of categorical columns to convert to factors
  categorical_cols <- c(
    "gender", "Partner", "Dependents", "PhoneService", "MultipleLines",
    "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection",
    "TechSupport", "StreamingTV", "StreamingMovies", "Contract",
    "PaperlessBilling", "PaymentMethod", "Churn"
  )

  for (col in categorical_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.factor(df[[col]])
    }
  }

  # Ensure Churn has "No" as the reference level
  if ("Churn" %in% names(df)) {
    df$Churn <- factor(df$Churn, levels = c("No", "Yes"))
  }

  # 5. Create derived statistical grouping variables for discrete probability analysis
  # Tenure Groups (Quarterly/Yearly lifecycle stages)
  df$TenureGroup <- cut(
    df$tenure,
    breaks = c(-Inf, 12, 24, 48, Inf),
    labels = c("0-12 Months (New)", "13-24 Months", "25-48 Months", "49-72 Months (Loyal)"),
    right = TRUE
  )

  # Monthly Charges Groups (Tiered spending)
  df$MonthlyChargeGroup <- cut(
    df$MonthlyCharges,
    breaks = c(-Inf, 35, 70, 90, Inf),
    labels = c("Low (<= $35)", "Medium ($35 - $70)", "High ($70 - $90)", "Very High (> $90)"),
    right = TRUE
  )

  # 6. Remove unnecessary customerID column
  if (drop_id && "customerID" %in% names(df)) {
    df$customerID <- NULL
  }

  message(sprintf("Data cleaning complete. Cleaned dataset has %d observations and %d variables.", nrow(df), ncol(df)))
  return(df)
}

#' Display Dataset Summary and Quality Metrics
#'
#' Prints a structured, student-friendly statistical summary of the dataset.
#'
#' @param df Cleaned data.frame.
#' @return A list containing summary metadata.
get_dataset_info <- function(df) {
  total_obs <- nrow(df)
  total_vars <- ncol(df)
  total_na <- sum(is.na(df))

  churn_counts <- table(df$Churn)
  churn_prop <- prop.table(churn_counts)

  cat("\n========================================================\n")
  cat("          TELCO CUSTOMER CHURN DATASET SUMMARY          \n")
  cat("========================================================\n")
  cat(sprintf("Total Observations (Customers) : %d\n", total_obs))
  cat(sprintf("Total Variables (Features)     : %d\n", total_vars))
  cat(sprintf("Total Missing Values (NAs)     : %d\n", total_na))
  cat("--------------------------------------------------------\n")
  cat("Target Variable (Churn) Distribution:\n")
  cat(sprintf("  - Retained (No) : %d (%.2f%%)\n", churn_counts["No"], churn_prop["No"] * 100))
  cat(sprintf("  - Churned (Yes) : %d (%.2f%%)\n", churn_counts["Yes"], churn_prop["Yes"] * 100))
  cat("--------------------------------------------------------\n")
  cat("Key Numerical Features Range & Medians:\n")
  cat(sprintf("  - Tenure (Months)    : Min = %.1f, Median = %.1f, Max = %.1f\n",
              min(df$tenure), median(df$tenure), max(df$tenure)))
  cat(sprintf("  - Monthly Charges ($): Min = %.2f, Median = %.2f, Max = %.2f\n",
              min(df$MonthlyCharges), median(df$MonthlyCharges), max(df$MonthlyCharges)))
  cat(sprintf("  - Total Charges ($)  : Min = %.2f, Median = %.2f, Max = %.2f\n",
              min(df$TotalCharges), median(df$TotalCharges), max(df$TotalCharges)))
  cat("========================================================\n\n")

  invisible(list(
    n_rows = total_obs,
    n_cols = total_vars,
    na_count = total_na,
    churn_counts = churn_counts,
    churn_prop = churn_prop
  ))
}

# Standalone execution check when run directly via Rscript
if (sys.nframe() == 0L) {
  raw_data <- load_raw_data()
  clean_data <- clean_churn_data(raw_data)
  get_dataset_info(clean_data)
}
