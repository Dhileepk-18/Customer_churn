# ==============================================================================
# File: R/statistics.R
# Project: Predicting Customer Churn (College-Level Probability & Statistics)
# Description: Descriptive statistics, frequency distributions, and inferential
#              hypothesis tests (Chi-Square test, Two-sample t-test, Correlation).
# ==============================================================================

#' Statistical Mode Calculation
#'
#' Base R provides `mode()` for data storage type, not the statistical mode.
#' This helper calculates the most frequently occurring value in a vector.
#'
#' @param x Numeric or character vector.
#' @param na.rm Logical, whether to remove NA values.
#' @return The most frequent value (or first mode if multimodal).
calc_mode <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }

  # For continuous numeric data, mode of rounded values
  if (is.numeric(x) && length(unique(x)) > 50) {
    tbl <- table(round(x, 1))
    mode_val <- as.numeric(names(tbl)[which.max(tbl)])
  } else {
    tbl <- table(x)
    mode_val <- names(tbl)[which.max(tbl)]
    if (is.numeric(x)) mode_val <- as.numeric(mode_val)
  }
  return(mode_val)
}


calc_descriptive_stats <- function(x, var_name = "Variable") {
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)

  if (n == 0) {
    stop("Input vector contains no valid numeric values.")
  }

  # Measures of Central Tendency
  mean_val <- mean(x_clean)
  median_val <- median(x_clean)
  mode_val <- calc_mode(x_clean)

  # Measures of Dispersion (Spread)
  var_val <- var(x_clean)
  sd_val <- sd(x_clean)
  min_val <- min(x_clean)
  max_val <- max(x_clean)
  range_val <- max_val - min_val

  # Percentiles / Quartiles
  quants <- quantile(x_clean, probs = c(0.25, 0.75))
  q1 <- as.numeric(quants[1])
  q3 <- as.numeric(quants[2])
  iqr_val <- q3 - q1

  # Skewness Indicator (Pearson's median skewness: 3 * (Mean - Median) / SD)
  skew_val <- if (sd_val > 0) 3 * (mean_val - median_val) / sd_val else 0

  list(
    Variable = var_name,
    N = n,
    Mean = round(mean_val, 2),
    Median = round(median_val, 2),
    Mode = round(mode_val, 2),
    SD = round(sd_val, 2),
    Variance = round(var_val, 2),
    Min = round(min_val, 2),
    Q1 = round(q1, 2),
    Q3 = round(q3, 2),
    Max = round(max_val, 2),
    Range = round(range_val, 2),
    IQR = round(iqr_val, 2),
    Skewness = round(skew_val, 3)
  )
}


calc_numerical_summary_table <- function(df, num_vars = c("tenure", "MonthlyCharges", "TotalCharges")) {
  stats_list <- lapply(num_vars, function(v) {
    if (v %in% names(df) && is.numeric(df[[v]])) {
      as.data.frame(calc_descriptive_stats(df[[v]], var_name = v), stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })

  summary_df <- do.call(rbind, stats_list)
  rownames(summary_df) <- NULL
  return(summary_df)
}


calc_grouped_descriptive_stats <- function(df, num_var, group_var = "Churn") {
  groups <- levels(as.factor(df[[group_var]]))

  res <- lapply(groups, function(grp) {
    sub_data <- df[[num_var]][df[[group_var]] == grp]
    s <- calc_descriptive_stats(sub_data, var_name = sprintf("%s (%s = %s)", num_var, group_var, grp))
    as.data.frame(s, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, res)
  rownames(out) <- NULL
  return(out)
}


calc_frequency_table <- function(df, cat_var) {
  freq <- table(df[[cat_var]])
  rel_freq <- prop.table(freq) * 100
  cum_freq <- cumsum(rel_freq)

  out <- data.frame(
    Category = names(freq),
    Count = as.numeric(freq),
    Percentage = round(as.numeric(rel_freq), 2),
    CumulativePercentage = round(as.numeric(cum_freq), 2),
    stringsAsFactors = FALSE
  )
  return(out)
}


calc_contingency_table <- function(df, cat_var1, cat_var2 = "Churn") {
  counts <- table(df[[cat_var1]], df[[cat_var2]])
  row_props <- round(prop.table(counts, margin = 1) * 100, 2)

  list(
    Counts = addmargins(counts),
    RowPercentages = row_props
  )
}


perform_chisq_test <- function(df, cat_var, target_var = "Churn", alpha = 0.05) {
  tbl <- table(df[[cat_var]], df[[target_var]])
  test_res <- chisq.test(tbl, correct = FALSE)

  stat_val <- round(as.numeric(test_res$statistic), 4)
  p_val <- test_res$p.value
  df_val <- as.numeric(test_res$parameter)

  is_significant <- p_val < alpha

  conclusion <- if (is_significant) {
    sprintf(
      "Reject H0 (p-value = %.4e < %.2f). There is a statistically significant association between '%s' and '%s'.",
      p_val, alpha, cat_var, target_var
    )
  } else {
    sprintf(
      "Fail to reject H0 (p-value = %.4f >= %.2f). No statistically significant association was found between '%s' and '%s'.",
      p_val, alpha, cat_var, target_var
    )
  }

  list(
    TestName = "Pearson's Chi-Square Test of Independence",
    Feature = cat_var,
    Target = target_var,
    H0 = sprintf("Null Hypothesis (H0): '%s' and '%s' are independent.", cat_var, target_var),
    H1 = sprintf("Alternative Hypothesis (H1): '%s' and '%s' are dependent (statistically associated).", cat_var, target_var),
    ChiSquareStatistic = stat_val,
    DegreesOfFreedom = df_val,
    PValue = p_val,
    SignificanceLevel = alpha,
    IsSignificant = is_significant,
    Conclusion = conclusion,
    ContingencyTable = tbl
  )
}


perform_two_sample_ttest <- function(df, num_var, group_var = "Churn", alpha = 0.05) {
  groups <- levels(as.factor(df[[group_var]]))
  group_no_data <- df[[num_var]][df[[group_var]] == groups[1]]
  group_yes_data <- df[[num_var]][df[[group_var]] == groups[2]]

  mean_no <- mean(group_no_data, na.rm = TRUE)
  mean_yes <- mean(group_yes_data, na.rm = TRUE)

  # Welch Two-Sample t-test (does not assume equal population variances)
  test_res <- t.test(group_no_data, group_yes_data, alternative = "two.sided", conf.level = 1 - alpha)

  t_stat <- round(as.numeric(test_res$statistic), 4)
  p_val <- test_res$p.value
  df_val <- round(as.numeric(test_res$parameter), 2)
  ci_low <- round(test_res$conf.int[1], 3)
  ci_high <- round(test_res$conf.int[2], 3)

  is_significant <- p_val < alpha

  conclusion <- if (is_significant) {
    sprintf(
      "Reject H0 (p-value = %.4e < %.2f). The difference in mean %s between Retained (%.2f) and Churned (%.2f) customers is statistically significant.",
      p_val, alpha, num_var, mean_no, mean_yes
    )
  } else {
    sprintf(
      "Fail to reject H0 (p-value = %.4f >= %.2f). There is no statistically significant difference in mean %s between Retained (%.2f) and Churned (%.2f).",
      p_val, alpha, num_var, mean_no, mean_yes
    )
  }

  list(
    TestName = "Welch Two-Sample t-Test (Difference in Population Means)",
    Variable = num_var,
    GroupingVariable = group_var,
    Mean_Retained_No = round(mean_no, 2),
    Mean_Churned_Yes = round(mean_yes, 2),
    DifferenceInMeans = round(mean_no - mean_yes, 2),
    H0 = sprintf("Null Hypothesis (H0): mu(Retained) == mu(Churned) for %s.", num_var),
    H1 = sprintf("Alternative Hypothesis (H1): mu(Retained) != mu(Churned) for %s.", num_var),
    TStatistic = t_stat,
    DegreesOfFreedom = df_val,
    PValue = p_val,
    ConfidenceInterval95 = c(ci_low, ci_high),
    SignificanceLevel = alpha,
    IsSignificant = is_significant,
    Conclusion = conclusion
  )
}

perform_correlation_analysis <- function(df, var1, var2, alpha = 0.05) {
  test_res <- cor.test(df[[var1]], df[[var2]], method = "pearson", conf.level = 1 - alpha)

  r_val <- round(as.numeric(test_res$estimate), 4)
  t_stat <- round(as.numeric(test_res$statistic), 4)
  p_val <- test_res$p.value
  df_val <- as.numeric(test_res$parameter)
  ci_low <- round(test_res$conf.int[1], 4)
  ci_high <- round(test_res$conf.int[2], 4)

  # Strength description
  abs_r <- abs(r_val)
  strength <- if (abs_r < 0.2) {
    "very weak"
  } else if (abs_r < 0.4) {
    "weak"
  } else if (abs_r < 0.6) {
    "moderate"
  } else if (abs_r < 0.8) {
    "strong"
  } else {
    "very strong"
  }

  direction <- if (r_val > 0) "positive" else "negative"

  is_significant <- p_val < alpha
  conclusion <- if (is_significant) {
    sprintf(
      "Reject H0 (p-value = %.4e < %.2f). There is a statistically significant %s %s linear relationship between '%s' and '%s' (r = %.4f).",
      p_val, alpha, strength, direction, var1, var2, r_val
    )
  } else {
    sprintf(
      "Fail to reject H0 (p-value = %.4f >= %.2f). No statistically significant linear correlation found between '%s' and '%s'.",
      p_val, alpha, var1, var2
    )
  }

  list(
    TestName = "Pearson Product-Moment Correlation Test",
    Variable1 = var1,
    Variable2 = var2,
    Pearson_r = r_val,
    Strength = strength,
    Direction = direction,
    H0 = sprintf("Null Hypothesis (H0): True correlation rho between %s and %s is 0.", var1, var2),
    H1 = sprintf("Alternative Hypothesis (H1): True correlation rho between %s and %s is not 0.", var1, var2),
    TStatistic = t_stat,
    DegreesOfFreedom = df_val,
    PValue = p_val,
    ConfidenceInterval95 = c(ci_low, ci_high),
    SignificanceLevel = alpha,
    IsSignificant = is_significant,
    Conclusion = conclusion
  )
}

# Standalone execution check when run directly via Rscript
if (sys.nframe() == 0L) {
  source("R/data_cleaning.R")
  data_clean <- clean_churn_data(load_raw_data())

  cat("\n--- NUMERICAL SUMMARY TABLE ---\n")
  print(calc_numerical_summary_table(data_clean))

  cat("\n--- TWO-SAMPLE T-TEST (Tenure by Churn) ---\n")
  ttest_tenure <- perform_two_sample_ttest(data_clean, "tenure")
  print(ttest_tenure$Conclusion)

  cat("\n--- CHI-SQUARE TEST (Contract vs Churn) ---\n")
  chisq_contract <- perform_chisq_test(data_clean, "Contract")
  print(chisq_contract$Conclusion)

  cat("\n--- CORRELATION (Tenure vs TotalCharges) ---\n")
  cor_res <- perform_correlation_analysis(data_clean, "tenure", "TotalCharges")
  print(cor_res$Conclusion)
}
