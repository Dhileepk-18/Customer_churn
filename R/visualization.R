# ==============================================================================
# File: R/visualization.R
# Project: Predicting Customer Churn (College-Level Probability & Statistics)
# Description: Modular ggplot2 statistical graphics for exploratory data analysis,
#              probability distributions, and hypothesis test visualizations.
# ==============================================================================

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Churn", "Freq", "Label", "tenure", "MonthlyCharges",
    "Category", "Prop", "PctLabel", ".data", "Var1", "Var2", "Correlation"
  ))
}

# Common aesthetic palette and theme for publication-quality lab figures
theme_college_stats <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, color = "#1e293b", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#64748b", margin = ggplot2::margin(b = 10)),
      axis.title = ggplot2::element_text(face = "bold", size = 11, color = "#334155"),
      axis.text = ggplot2::element_text(color = "#475569"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#e2e8f0", linewidth = 0.5),
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 10),
      plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
      panel.background = ggplot2::element_rect(fill = "#ffffff", color = NA)
    )
}

# Color palette definition
PALETTE_CHURN <- c("No" = "#2563eb", "Yes" = "#ef4444")  # Retained: Blue, Churned: Red
PALETTE_PRIMARY <- "#3b82f6"
PALETTE_ACCENT <- "#10b981"

#' Plot Target Distribution (Churn Frequency & Proportion)
#'
#' @param df Cleaned customer data.frame.
#' @return ggplot object.
plot_churn_distribution <- function(df) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")

  churn_counts <- as.data.frame(table(Churn = df$Churn))
  churn_counts$Percent <- churn_counts$Freq / sum(churn_counts$Freq) * 100
  churn_counts$Label <- sprintf("%s\n(%d | %.1f%%)",
                                ifelse(churn_counts$Churn == "No", "Retained (No)", "Churned (Yes)"),
                                churn_counts$Freq, churn_counts$Percent)

  ggplot2::ggplot(churn_counts, ggplot2::aes(x = Churn, y = Freq, fill = Churn)) +
    ggplot2::geom_col(width = 0.55, alpha = 0.9, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = Label), vjust = 1.3, color = "white", fontface = "bold", size = 4.2) +
    ggplot2::scale_fill_manual(values = PALETTE_CHURN) +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(
      title = "Distribution of Customer Churn (Target Variable)",
      subtitle = sprintf("Total Sample: N = %d customers | Overall Churn Rate: %.1f%%", nrow(df), churn_counts$Percent[churn_counts$Churn == "Yes"]),
      x = "Churn Status",
      y = "Number of Customers (Frequency)"
    ) +
    theme_college_stats()
}

#' Plot Histogram and Density for Customer Tenure
#'
#' @param df Cleaned data.frame.
#' @return ggplot object.
plot_tenure_distribution <- function(df) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")

  median_val <- median(df$tenure)
  mean_val <- mean(df$tenure)

  ggplot2::ggplot(df, ggplot2::aes(x = tenure)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 30, fill = "#38bdf8", color = "#0284c7", alpha = 0.7) +
    ggplot2::geom_density(color = "#0369a1", linewidth = 1.1) +
    ggplot2::geom_vline(xintercept = mean_val, color = "#ef4444", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_vline(xintercept = median_val, color = "#16a34a", linetype = "dotted", linewidth = 1) +
    ggplot2::annotate("text", x = mean_val + 2, y = 0.022, label = sprintf("Mean: %.1f mos", mean_val), color = "#ef4444", hjust = 0, fontface = "bold") +
    ggplot2::annotate("text", x = median_val + 2, y = 0.018, label = sprintf("Median: %.1f mos", median_val), color = "#16a34a", hjust = 0, fontface = "bold") +
    ggplot2::labs(
      title = "Distribution of Customer Tenure (Months with Company)",
      subtitle = "Shows high concentration of new sign-ups (tenure < 5) and long-term loyalists (tenure > 60)",
      x = "Tenure (Months)",
      y = "Probability Density"
    ) +
    theme_college_stats()
}

#' Plot Histogram and Density for Monthly Charges
#'
#' @param df Cleaned data.frame.
#' @return ggplot object.
plot_monthly_charges_distribution <- function(df) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")

  median_val <- median(df$MonthlyCharges)
  mean_val <- mean(df$MonthlyCharges)

  ggplot2::ggplot(df, ggplot2::aes(x = MonthlyCharges)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 30, fill = "#a78bfa", color = "#7c3aed", alpha = 0.7) +
    ggplot2::geom_density(color = "#5b21b6", linewidth = 1.1) +
    ggplot2::geom_vline(xintercept = mean_val, color = "#ef4444", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_vline(xintercept = median_val, color = "#16a34a", linetype = "dotted", linewidth = 1) +
    ggplot2::annotate("text", x = mean_val + 2, y = 0.013, label = sprintf("Mean: $%.2f", mean_val), color = "#ef4444", hjust = 0, fontface = "bold") +
    ggplot2::annotate("text", x = median_val + 2, y = 0.010, label = sprintf("Median: $%.2f", median_val), color = "#16a34a", hjust = 0, fontface = "bold") +
    ggplot2::labs(
      title = "Distribution of Monthly Charges ($)",
      subtitle = "Bimodal distribution reflecting basic economy tiers (~$20) vs high-speed/bundled packages ($70-$100)",
      x = "Monthly Charges ($)",
      y = "Probability Density"
    ) +
    theme_college_stats()
}

#' Plot Churn Proportion across Categorical Levels
#'
#' @param df Cleaned data.frame.
#' @param cat_var Name of the categorical column.
#' @param title_label Title description for the plot.
#' @return ggplot object.
plot_churn_vs_categorical <- function(df, cat_var, title_label = cat_var) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")

  # Calculate proportions per group
  grouped <- as.data.frame(table(Category = df[[cat_var]], Churn = df$Churn))
  totals <- aggregate(Freq ~ Category, data = grouped, sum)
  grouped <- merge(grouped, totals, by = "Category", suffixes = c("", "_Total"))
  grouped$Prop <- grouped$Freq / grouped$Freq_Total
  grouped$PctLabel <- sprintf("%.1f%%\n(%d)", grouped$Prop * 100, grouped$Freq)

  ggplot2::ggplot(grouped, ggplot2::aes(x = Category, y = Prop, fill = Churn)) +
    ggplot2::geom_col(position = "fill", alpha = 0.9, width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = PctLabel),
      position = ggplot2::position_fill(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 3.8
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_fill_manual(
      values = PALETTE_CHURN,
      labels = c("Retained (No)", "Churned (Yes)")
    ) +
    ggplot2::labs(
      title = sprintf("Customer Churn Proportion by %s", title_label),
      subtitle = "Visualizes conditional churn probabilities across subgroups",
      x = title_label,
      y = "Proportion of Customers",
      fill = "Status:"
    ) +
    theme_college_stats() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))
}

#' Plot Comparative Boxplot of Continuous Metric by Churn Status
#'
#' @param df Cleaned data.frame.
#' @param num_var Continuous column name.
#' @param y_label Human-readable axis label.
#' @return ggplot object.
plot_boxplot_by_churn <- function(df, num_var, y_label = num_var) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")

  ggplot2::ggplot(df, ggplot2::aes(x = Churn, y = .data[[num_var]], fill = Churn)) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.color = "#94a3b8", outlier.size = 1.5, width = 0.5) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "yellow", color = "black") +
    ggplot2::scale_fill_manual(values = PALETTE_CHURN, guide = "none") +
    ggplot2::labs(
      title = sprintf("%s by Churn Status (Boxplot & Quartiles)", y_label),
      subtitle = "Solid lines indicate Medians (Q2); Yellow diamonds denote Group Means",
      x = "Churn Status",
      y = y_label
    ) +
    theme_college_stats()
}

#' Plot Correlation Heatmap for Numerical Features
#'
#' @param df Cleaned data.frame.
#' @param num_vars Vector of numeric column names.
#' @return ggplot object.
plot_correlation_matrix <- function(df, num_vars = c("tenure", "MonthlyCharges", "TotalCharges")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")

  sub_df <- df[, num_vars]
  cor_mat <- cor(sub_df, use = "complete.obs", method = "pearson")

  cor_df <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("Var1", "Var2", "Correlation")
  cor_df$Label <- sprintf("%.3f", cor_df$Correlation)

  ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = Label), color = ifelse(abs(cor_df$Correlation) > 0.6, "white", "black"), fontface = "bold", size = 4.5) +
    ggplot2::scale_fill_gradient2(low = "#3b82f6", mid = "#f8fafc", high = "#ef4444", midpoint = 0, limit = c(-1, 1), name = "Pearson r") +
    ggplot2::labs(
      title = "Pearson Correlation Matrix (Continuous Variables)",
      subtitle = "Shows strong collinearity between Tenure and Total Charges (r = 0.826)",
      x = "",
      y = ""
    ) +
    theme_college_stats() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
}
