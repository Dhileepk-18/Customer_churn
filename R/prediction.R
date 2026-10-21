# ==============================================================================
# File: R/prediction.R
# Project: Predicting Customer Churn (College-Level Probability & Statistics)
# Description: Binary Logistic Regression model training, evaluation metrics
#              (Confusion Matrix, Accuracy, Precision, Recall, F1, ROC/AUC),
#              odds ratio interpretations, and individual customer risk scorer.
# ==============================================================================

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("FPR", "TPR"))
}

split_data <- function(df, train_ratio = 0.80, seed = 42) {
  set.seed(seed)
  n <- nrow(df)
  train_indices <- sample(seq_len(n), size = floor(train_ratio * n), replace = FALSE)

  train_data <- df[train_indices, ]
  test_data <- df[-train_indices, ]

  message(sprintf(
    "Data partitioned: %d train rows (%.1f%%), %d test rows (%.1f%%).",
    nrow(train_data), train_ratio * 100, nrow(test_data), (1 - train_ratio) * 100
  ))

  list(train = train_data, test = test_data)
}

#' Train Binary Logistic Regression Model
#'
#' Fits a generalized linear model with binomial family and logit link:
#' ln(p / (1 - p)) = beta_0 + beta_1 * X_1 + ... + beta_k * X_k
#'
#' @param train_df Training data.frame.
#' @return Fitted `glm` object.
train_logistic_regression <- function(train_df) {
  # Core lab formula utilizing key predictors
  model_formula <- Churn ~ tenure + MonthlyCharges + TotalCharges +
    Contract + InternetService + PaymentMethod + SeniorCitizen

  model <- glm(
    formula = model_formula,
    data = train_df,
    family = binomial(link = "logit")
  )

  message("Logistic regression model trained successfully.")
  return(model)
}

get_model_coefficients <- function(model) {
  s <- summary(model)
  coef_mat <- s$coefficients

  est <- coef_mat[, "Estimate"]
  se <- coef_mat[, "Std. Error"]
  z <- coef_mat[, "z value"]
  p <- coef_mat[, "Pr(>|z|)"]

  odds_ratio <- exp(est)
  ci_lower <- exp(est - 1.96 * se)
  ci_upper <- exp(est + 1.96 * se)

  interpretation <- sapply(seq_along(est), function(i) {
    term <- rownames(coef_mat)[i]
    if (term == "(Intercept)") {
      return("Baseline log-odds when all continuous vars are 0 and factors are reference levels.")
    }

    or <- odds_ratio[i]
    if (p[i] >= 0.05) {
      return("Not statistically significant at alpha = 0.05 level.")
    } else if (or > 1) {
      pct_inc <- (or - 1) * 100
      return(sprintf("Increases odds of churn by %.1f%% (Risk Factor).", pct_inc))
    } else {
      pct_dec <- (1 - or) * 100
      return(sprintf("Decreases odds of churn by %.1f%% (Protective Factor).", pct_dec))
    }
  })

  out <- data.frame(
    Feature = rownames(coef_mat),
    Coefficient_Beta = round(est, 4),
    Std_Error = round(se, 4),
    Z_Statistic = round(z, 3),
    P_Value = format.pval(p, digits = 3, eps = 0.0001),
    Odds_Ratio = round(odds_ratio, 3),
    OR_95_CI_Low = round(ci_lower, 3),
    OR_95_CI_High = round(ci_upper, 3),
    Statistical_Interpretation = interpretation,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  return(out)
}


calculate_roc_auc <- function(true_labels, predicted_probs) {
  y_true <- ifelse(true_labels == "Yes", 1, 0)

  # Calculate Wilcoxon/Mann-Whitney rank-sum AUC
  n_pos <- sum(y_true == 1)
  n_neg <- sum(y_true == 0)

  if (n_pos == 0 || n_neg == 0) {
    return(list(AUC = 0.5, ROC_Data = data.frame(FPR = c(0, 1), TPR = c(0, 1))))
  }

  ranks <- rank(predicted_probs)
  sum_ranks_pos <- sum(ranks[y_true == 1])
  auc_val <- (sum_ranks_pos - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)

  # Generate ROC curve coordinates across thresholds
  thresholds <- seq(0, 1, by = 0.01)
  roc_points <- lapply(thresholds, function(th) {
    y_pred <- ifelse(predicted_probs >= th, 1, 0)
    tp <- sum(y_pred == 1 & y_true == 1)
    fp <- sum(y_pred == 1 & y_true == 0)
    fn <- sum(y_pred == 0 & y_true == 1)
    tn <- sum(y_pred == 0 & y_true == 0)

    tpr <- if (tp + fn > 0) tp / (tp + fn) else 0 # Sensitivity
    fpr <- if (fp + tn > 0) fp / (fp + tn) else 0 # 1 - Specificity

    data.frame(Threshold = th, FPR = fpr, TPR = tpr)
  })

  roc_df <- do.call(rbind, roc_points)
  roc_df <- roc_df[order(roc_df$FPR, roc_df$TPR), ]

  list(
    AUC = round(auc_val, 4),
    ROC_Data = roc_df
  )
}


evaluate_model <- function(model, test_df, threshold = 0.50) {
  probs <- predict(model, newdata = test_df, type = "response")
  pred_class <- factor(ifelse(probs >= threshold, "Yes", "No"), levels = c("No", "Yes"))
  actual_class <- factor(test_df$Churn, levels = c("No", "Yes"))

  # Confusion Matrix elements
  # Reference: Actual is columns, Predicted is rows
  cm <- table(Predicted = pred_class, Actual = actual_class)

  tn <- cm["No", "No"]
  fp <- cm["Yes", "No"]
  fn <- cm["No", "Yes"]
  tp <- cm["Yes", "Yes"]
  total <- tn + fp + fn + tp

  # Performance Metrics
  accuracy <- (tp + tn) / total
  precision <- if (tp + fp > 0) tp / (tp + fp) else 0
  recall <- if (tp + fn > 0) tp / (tp + fn) else 0 # Sensitivity
  specificity <- if (tn + fp > 0) tn / (tn + fp) else 0
  f1_score <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0

  # Compute ROC and AUC curve metrics
  roc_res <- calculate_roc_auc(actual_class, probs)

  list(
    Threshold = threshold,
    ConfusionMatrix = cm,
    Metrics = data.frame(
      Metric = c("Accuracy", "Precision", "Recall (Sensitivity)", "Specificity", "F1-Score", "ROC-AUC"),
      Formula = c(
        "(TP + TN) / Total",
        "TP / (TP + FP)",
        "TP / (TP + FN)",
        "TN / (TN + FP)",
        "2 * (Prec * Rec) / (Prec + Rec)",
        "Area Under ROC Curve"
      ),
      Value = c(
        round(accuracy, 4),
        round(precision, 4),
        round(recall, 4),
        round(specificity, 4),
        round(f1_score, 4),
        roc_res$AUC
      ),
      Percentage = c(
        sprintf("%.2f%%", accuracy * 100),
        sprintf("%.2f%%", precision * 100),
        sprintf("%.2f%%", recall * 100),
        sprintf("%.2f%%", specificity * 100),
        sprintf("%.2f%%", f1_score * 100),
        sprintf("%.2f%%", roc_res$AUC * 100)
      ),
      stringsAsFactors = FALSE
    ),
    AUC = roc_res$AUC,
    ROC_Data = roc_res$ROC_Data,
    PredictedProbabilities = probs
  )
}

#' Plot ROC Curve using ggplot2
#'
#' @param eval_res Result object returned by `evaluate_model()`.
#' @return ggplot object.
plot_roc_curve <- function(eval_res) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")

  roc_df <- eval_res$ROC_Data
  auc_val <- eval_res$AUC

  ggplot2::ggplot(roc_df, ggplot2::aes(x = FPR, y = TPR)) +
    ggplot2::geom_line(color = "#2563eb", linewidth = 1.3) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#94a3b8", linewidth = 0.9) +
    ggplot2::annotate(
      "text",
      x = 0.65, y = 0.25,
      label = sprintf(
        "AUC = %.4f\n(Discriminative Power: %s)",
        auc_val,
        ifelse(auc_val > 0.8, "Good", ifelse(auc_val > 0.7, "Acceptable", "Poor"))
      ),
      color = "#1e293b", fontface = "bold", size = 4.5,
      hjust = 0
    ) +
    ggplot2::labs(
      title = "Receiver Operating Characteristic (ROC) Curve",
      subtitle = "Evaluates True Positive Rate (Sensitivity) vs False Positive Rate (1 - Specificity)",
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity / Recall)"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      panel.grid.minor = ggplot2::element_blank()
    )
}

predict_individual_customer <- function(
  model,
  tenure,
  MonthlyCharges,
  TotalCharges,
  Contract,
  InternetService,
  PaymentMethod,
  SeniorCitizen,
  threshold = 0.50
) {
  # Build 1-row data frame matching model structure
  customer_df <- data.frame(
    tenure = as.numeric(tenure),
    MonthlyCharges = as.numeric(MonthlyCharges),
    TotalCharges = as.numeric(TotalCharges),
    Contract = factor(Contract, levels = c("Month-to-month", "One year", "Two year")),
    InternetService = factor(InternetService, levels = c("DSL", "Fiber optic", "No")),
    PaymentMethod = factor(PaymentMethod, levels = c("Bank transfer (automatic)", "Credit card (automatic)", "Electronic check", "Mailed check")),
    SeniorCitizen = factor(SeniorCitizen, levels = c("No", "Yes")),
    stringsAsFactors = FALSE
  )

  # Calculate predicted probability
  prob <- as.numeric(predict(model, newdata = customer_df, type = "response"))
  prob_pct <- round(prob * 100, 1)

  is_churn <- prob >= threshold
  result_label <- if (is_churn) "Likely to Churn" else "Likely to Stay"
  risk_tier <- if (prob >= 0.70) "High Risk" else if (prob >= 0.40) "Moderate Risk" else "Low Risk"

  # Identify key risk drivers based on inputs
  drivers <- c()
  if (Contract == "Month-to-month") drivers <- c(drivers, "Month-to-month contract carries highest churn hazard.")
  if (Contract == "Two year") drivers <- c(drivers, "Two-year contract strongly protects against churn.")
  if (tenure <= 6) drivers <- c(drivers, "Short tenure indicates customer is still in vulnerable onboarding window.")
  if (tenure > 36) drivers <- c(drivers, "Long tenure demonstrates high historical brand loyalty.")
  if (InternetService == "Fiber optic") drivers <- c(drivers, "Fiber optic customers exhibit statistically higher churn rates.")
  if (PaymentMethod == "Electronic check") drivers <- c(drivers, "Electronic check payment method is correlated with elevated churn.")
  if (MonthlyCharges > 80) drivers <- c(drivers, "High monthly charges (> $80) increase price sensitivity.")

  explanation <- sprintf(
    "Estimated Chance of Leaving: %.1f%%\nVerdict: Customer is %s (Risk Level: %s)\n\nMain Factors Driving This Prediction:\n%s\n\n[Notice]: This estimate is calculated from patterns in historical customer data. It is an empirical statistical prediction to guide retention, not an absolute guarantee.",
    prob_pct, result_label, risk_tier,
    paste(" •", drivers, collapse = "\n")
  )

  list(
    PredictedStatus = result_label,
    ChurnProbability = prob,
    ProbabilityPercent = prob_pct,
    RiskTier = risk_tier,
    KeyDrivers = drivers,
    Explanation = explanation,
    Disclaimer = "Statistical prediction based on dataset sample; not guaranteed."
  )
}

# Standalone execution check when run directly via Rscript
if (sys.nframe() == 0L) {
  source("R/data_cleaning.R")
  data_clean <- clean_churn_data(load_raw_data())
  splits <- split_data(data_clean, train_ratio = 0.8, seed = 42)

  model <- train_logistic_regression(splits$train)

  cat("\n--- MODEL COEFFICIENTS AND ODDS RATIOS ---\n")
  print(head(get_model_coefficients(model), 8))

  cat("\n--- MODEL EVALUATION ON TEST SET ---\n")
  eval_res <- evaluate_model(model, splits$test)
  print(eval_res$ConfusionMatrix)
  print(eval_res$Metrics)

  cat("\n--- SAMPLE INDIVIDUAL PREDICTION ---\n")
  sample_pred <- predict_individual_customer(
    model,
    tenure = 2,
    MonthlyCharges = 85.50,
    TotalCharges = 171.0,
    Contract = "Month-to-month",
    InternetService = "Fiber optic",
    PaymentMethod = "Electronic check",
    SeniorCitizen = "No"
  )
  cat(sample_pred$Explanation, "\n")
}
