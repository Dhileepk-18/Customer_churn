# ==============================================================================
# File: R/probability.R
# Project: Predicting Customer Churn (College-Level Probability & Statistics)
# Description: Marginal, joint, and conditional probability calculations with
#              mathematical definitions and step-by-step explanations.
# ==============================================================================

#' Calculate Overall (Marginal) Probability of Churn
#'
#' Computes the unconditional probability:
#' P(Churn = Yes) = n(Churn = Yes) / N
#' P(Churn = No)  = n(Churn = No)  / N = 1 - P(Churn = Yes)
#'
#' @param df Cleaned data.frame.
#' @return A list containing probabilities, counts, and mathematical explanation.
calc_overall_churn_probability <- function(df) {
  n_total <- nrow(df)
  n_churn <- sum(df$Churn == "Yes")
  n_retain <- sum(df$Churn == "No")

  p_churn <- n_churn / n_total
  p_retain <- n_retain / n_total

  explanation <- sprintf(
    paste0(
      "Overall Probability Calculation:\n",
      "  P(Churn = Yes) = n(Churn = Yes) / N = %d / %d = %.4f (%.2f%%)\n",
      "  P(Churn = No)  = 1 - P(Churn = Yes) = %.4f (%.2f%%)\n",
      "Interpretation: In the entire customer base, any randomly selected customer ",
      "has a %.2f%% likelihood of leaving."
    ),
    n_churn, n_total, p_churn, p_churn * 100,
    p_retain, p_retain * 100,
    p_churn * 100
  )

  list(
    TotalCustomers = n_total,
    ChurnedCount = n_churn,
    RetainedCount = n_retain,
    P_Churn = round(p_churn, 4),
    P_Retain = round(p_retain, 4),
    Percentage_Churn = round(p_churn * 100, 2),
    Percentage_Retain = round(p_retain * 100, 2),
    Formula = "P(A) = n(A) / N",
    Explanation = explanation
  )
}


calc_conditional_probability <- function(df, condition_expr, condition_name = "Given Condition") {
  n_total <- nrow(df)
  n_condition <- sum(condition_expr)

  if (n_condition == 0) {
    stop("Condition B matches 0 observations. Conditional probability is undefined.")
  }

  # Intersection: Condition B AND Churn == Yes
  n_intersection <- sum(condition_expr & df$Churn == "Yes")

  p_b <- n_condition / n_total
  p_a_and_b <- n_intersection / n_total
  p_a_given_b <- n_intersection / n_condition

  # Overall baseline churn probability for comparison
  p_baseline <- sum(df$Churn == "Yes") / n_total
  relative_risk <- p_a_given_b / p_baseline

  explanation <- sprintf(
    "Conditional Probability: P(Churn = Yes | %s)\n  Formula: P(A | B) = n(A and B) / n(B)\n  Subset size n(B): %d customers\n  Joint occurrences n(A and B): %d churned\n  P(Churn | %s) = %d / %d = %.4f (%.2f%%)\n  Baseline Churn: %.2f%%\n  Relative Risk (Lift): %.2fx compared to overall customer population.",
    condition_name, n_condition, n_intersection,
    condition_name, n_intersection, n_condition,
    p_a_given_b, p_a_given_b * 100,
    p_baseline * 100, relative_risk
  )

  list(
    Condition = condition_name,
    n_B = n_condition,
    n_A_and_B = n_intersection,
    P_Condition = round(p_b, 4),
    P_Joint = round(p_a_and_b, 4),
    P_Conditional = round(p_a_given_b, 4),
    Percentage = round(p_a_given_b * 100, 2),
    BaselinePercentage = round(p_baseline * 100, 2),
    RelativeRisk = round(relative_risk, 2),
    Explanation = explanation
  )
}

calc_churn_by_categorical_probability <- function(df, cat_var) {
  baseline_p <- sum(df$Churn == "Yes") / nrow(df)
  levels_vec <- levels(as.factor(df[[cat_var]]))

  rows <- lapply(levels_vec, function(lvl) {
    cond <- df[[cat_var]] == lvl
    n_sub <- sum(cond)
    n_churn_sub <- sum(cond & df$Churn == "Yes")
    p_cond <- if (n_sub > 0) n_churn_sub / n_sub else 0
    rel_risk <- p_cond / baseline_p

    data.frame(
      Category = lvl,
      Total_Customers = n_sub,
      Churned_Customers = n_churn_sub,
      Retained_Customers = n_sub - n_churn_sub,
      P_Churn = round(p_cond, 4),
      Churn_Rate_Pct = round(p_cond * 100, 2),
      Relative_Risk = round(rel_risk, 2),
      stringsAsFactors = FALSE
    )
  })

  res_df <- do.call(rbind, rows)
  return(res_df)
}


get_key_conditional_probabilities <- function(df) {
  median_charges <- median(df$MonthlyCharges)

  # 1. Contract conditional probabilities
  p_m2m <- calc_conditional_probability(df, df$Contract == "Month-to-month", "Month-to-month Contract")
  p_one_yr <- calc_conditional_probability(df, df$Contract == "One year", "One year Contract")
  p_two_yr <- calc_conditional_probability(df, df$Contract == "Two year", "Two year Contract")

  # 2. Tenure conditional probabilities (Long tenure > 48 months vs New <= 12 months)
  p_new <- calc_conditional_probability(df, df$tenure <= 12, "Tenure <= 12 Months (New Customers)")
  p_loyal <- calc_conditional_probability(df, df$tenure > 48, "Tenure > 48 Months (Long-Term Customers)")

  # 3. Monthly charges conditional probabilities (High vs Low)
  p_high_charge <- calc_conditional_probability(
    df, df$MonthlyCharges > median_charges,
    sprintf("High Monthly Charges (> Median $%.2f)", median_charges)
  )
  p_low_charge <- calc_conditional_probability(
    df, df$MonthlyCharges <= median_charges,
    sprintf("Low/Moderate Monthly Charges (<= Median $%.2f)", median_charges)
  )

  # 4. Payment Method: Electronic check
  p_echeck <- calc_conditional_probability(df, df$PaymentMethod == "Electronic check", "Electronic Check Payment")

  # 5. Internet Service: Fiber optic
  p_fiber <- calc_conditional_probability(df, df$InternetService == "Fiber optic", "Fiber Optic Internet")

  list(
    Overall = calc_overall_churn_probability(df),
    MonthToMonth = p_m2m,
    OneYear = p_one_yr,
    TwoYear = p_two_yr,
    NewTenure = p_new,
    LongTenure = p_loyal,
    HighCharges = p_high_charge,
    LowCharges = p_low_charge,
    ElectronicCheck = p_echeck,
    FiberOptic = p_fiber
  )
}

# Standalone execution check when run directly via Rscript
if (sys.nframe() == 0L) {
  source("R/data_cleaning.R")
  data_clean <- clean_churn_data(load_raw_data())

  cat("\n--- OVERALL CHURN PROBABILITY ---\n")
  overall <- calc_overall_churn_probability(data_clean)
  cat(overall$Explanation, "\n\n")

  cat("--- CONDITIONAL PROBABILITIES BY CONTRACT ---\n")
  print(calc_churn_by_categorical_probability(data_clean, "Contract"))

  cat("\n--- CONDITIONAL PROBABILITIES BY INTERNET SERVICE ---\n")
  print(calc_churn_by_categorical_probability(data_clean, "InternetService"))

  cat("\n--- CONDITIONAL PROBABILITIES BY PAYMENT METHOD ---\n")
  print(calc_churn_by_categorical_probability(data_clean, "PaymentMethod"))

  cat("\n--- KEY LABORATORY COMPARISONS ---\n")
  key_probs <- get_key_conditional_probabilities(data_clean)
  cat(key_probs$MonthToMonth$Explanation, "\n\n")
  cat(key_probs$LongTenure$Explanation, "\n\n")
  cat(key_probs$HighCharges$Explanation, "\n")
}
