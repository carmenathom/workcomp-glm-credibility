library(ggplot2)
library(gridExtra)
library(scales)
library(tidyverse)

setwd("/Users/carmenthom/Desktop/workcomp-glm-credibility/analysis/figures")

p_exposure_dist <- ggplot(df, aes(x = exposure)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  scale_x_log10(labels = comma) +
  labs(
    title = "Distribution of Payroll Exposures",
    subtitle = "Workers' Compensation Analysis",
    x = "Payroll Exposure (Millions, log scale)",
    y = "Frequency"
  ) +
  theme_minimal()

p_loss_ratio <- ggplot(df %>% filter(loss_ratio < 2), aes(x = loss_ratio)) +
  geom_histogram(bins = 50, fill = "coral", alpha = 0.7) +
  geom_vline(aes(xintercept = median(loss_ratio, na.rm = TRUE)), 
             linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Distribution of Loss Ratios",
    subtitle = "Excluding extreme outliers (>2.0)",
    x = "Loss Ratio (Loss / Payroll)",
    y = "Frequency"
  ) +
  theme_minimal()

top_classes <- class_summary %>%
  arrange(desc(pure_premium)) %>%
  head(20)

p_pure_prem_top <- ggplot(top_classes, aes(x = reorder(factor(CL), pure_premium), 
                                           y = pure_premium)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Occupation Classes by Pure Premium",
    x = "Occupation Class",
    y = "Pure Premium (per 1,000 payroll)"
  ) +
  theme_minimal()

yearly_trend <- df %>%
  group_by(YR) %>%
  summarise(
    total_loss = sum(LOSS),
    total_exposure = sum(PR),
    loss_ratio = total_loss / total_exposure,
    pure_premium = (total_loss / total_exposure) * 1000,
    claim_freq = sum(freq) / n()
  )

p_temporal <- ggplot(yearly_trend, aes(x = YR)) +
  geom_line(aes(y = pure_premium), color = "blue", size = 1.2) +
  geom_point(aes(y = pure_premium), color = "blue", size = 3) +
  labs(
    title = "Pure Premium Trend Over Time",
    x = "Year",
    y = "Pure Premium (per 1,000 payroll)"
  ) +
  theme_minimal()

p_exposure_loss <- ggplot(df %>% filter(LOSS > 0), 
                          aes(x = exposure, y = LOSS)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Relationship Between Exposure and Loss",
    subtitle = "Log-log scale, claims only",
    x = "Payroll Exposure (Millions, log scale)",
    y = "Loss Amount ($, log scale)"
  ) +
  theme_minimal()

freq_by_exposure <- df %>%
  group_by(exposure_category) %>%
  summarise(
    claim_freq = mean(freq),
    n = n()
  )

p_freq_exposure <- ggplot(freq_by_exposure, 
                          aes(x = exposure_category, y = claim_freq)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = sprintf("%.3f", claim_freq)), 
            vjust = -0.5, size = 4) +
  labs(
    title = "Claim Frequency by Exposure Category",
    x = "Exposure Category",
    y = "Claim Frequency (proportion)",
    caption = paste0("Sample sizes - Low: ", freq_by_exposure$n[1], 
                     ", Medium: ", freq_by_exposure$n[2], 
                     ", High: ", freq_by_exposure$n[3])
  ) +
  theme_minimal()

freq_data_pred <- freq_data %>%
  mutate(
    fitted = predict(freq_model, type = "response"),
    residuals = residuals(freq_model, type = "deviance")
  )

p_freq_resid <- ggplot(freq_data_pred, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = TRUE, color = "blue") +
  labs(
    title = "Frequency Model: Residual Plot",
    subtitle = "Poisson GLM with offset",
    x = "Fitted Values",
    y = "Deviance Residuals"
  ) +
  theme_minimal()

p_freq_qq <- ggplot(freq_data_pred, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    title = "Frequency Model: Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

sev_data_pred <- sev_data %>%
  mutate(
    fitted = predict(sev_model, type = "response"),
    residuals = residuals(sev_model, type = "deviance")
  )

p_sev_resid <- ggplot(sev_data_pred, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = TRUE, color = "blue") +
  scale_x_log10(labels = comma) +
  labs(
    title = "Severity Model: Residual Plot",
    subtitle = "Gamma GLM",
    x = "Fitted Values (log scale)",
    y = "Deviance Residuals"
  ) +
  theme_minimal()

p_sev_qq <- ggplot(sev_data_pred, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    title = "Severity Model: Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

freq_coefs <- data.frame(
  class = names(coef(freq_model))[grep("^factor\\(CL\\)", names(coef(freq_model)))],
  estimate = coef(freq_model)[grep("^factor\\(CL\\)", names(coef(freq_model)))]
) %>%
  mutate(
    CL = as.numeric(gsub("factor\\(CL\\)", "", class)),
    exp_estimate = exp(estimate)
  ) %>%
  arrange(estimate)

p_freq_coefs <- ggplot(freq_coefs %>% head(30), 
                       aes(x = reorder(factor(CL), estimate), y = exp_estimate)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Frequency Model: Class Relativities (Top 30)",
    subtitle = "Relative to baseline class",
    x = "Occupation Class",
    y = "Frequency Relativity"
  ) +
  theme_minimal()

p_actual_pred <- ggplot(final_model, aes(x = actual_pure_prem, y = prediction)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95, color = exposure_group), 
                alpha = 0.4, width = 0) +
  geom_point(aes(color = exposure_group, size = exposure_millions), alpha = 0.7) +
  scale_size_continuous(range = c(2, 8), trans = "log10", labels = comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "GLM Predictions vs Actual Pure Premium",
    subtitle = "Error bars show 95% confidence intervals (wider for low-exposure classes)",
    x = "Actual Pure Premium (per 1,000 payroll)",
    y = "Predicted Pure Premium (per 1,000 payroll)",
    color = "Exposure Group",
    size = "Exposure ($M)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

p_error_boxplot <- ggplot(final_model, aes(x = exposure_group, y = rel_error)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(alpha = 0.3, width = 0.2) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Relative Prediction Errors by Exposure Group",
    x = "Exposure Group",
    y = "Relative Error (|Predicted - Actual| / Actual)"
  ) +
  theme_minimal()

coverage_data <- final_model %>%
  group_by(exposure_group) %>%
  summarise(
    coverage_95 = mean(actual_in_95_band) * 100,
    coverage_80 = mean(actual_in_80_band) * 100,
    n = n()
  ) %>%
  pivot_longer(cols = c(coverage_95, coverage_80), 
               names_to = "interval", 
               values_to = "coverage")

p_coverage <- ggplot(coverage_data, aes(x = exposure_group, y = coverage, fill = interval)) +
  geom_col(position = "dodge", alpha = 0.7) +
  geom_hline(yintercept = c(80, 95), linetype = "dashed", color = "red") +
  geom_text(aes(label = sprintf("%.1f%%", coverage)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("coverage_80" = "orange", "coverage_95" = "steelblue"),
                    labels = c("80% Interval", "95% Interval")) +
  labs(
    title = "Confidence Interval Coverage by Exposure Group",
    subtitle = "Target coverage shown by dashed lines",
    x = "Exposure Group",
    y = "Coverage (%)",
    fill = "Confidence Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_interval_width <- ggplot(final_model, aes(x = exposure_millions, 
                                            y = upper_95 - lower_95)) +
  geom_point(aes(color = exposure_group), alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  scale_x_log10(labels = comma) +
  labs(
    title = "95% Confidence Interval Width vs Exposure",
    subtitle = "Width decreases with higher exposure (credibility effect)",
    x = "Exposure ($M, log scale)",
    y = "95% Interval Width",
    color = "Exposure Group"
  ) +
  theme_minimal()

final_model <- final_model %>%
  mutate(
    implied_credibility = 1 / uncertainty_multiplier,
    credibility_group = case_when(
      implied_credibility >= 0.8 ~ "High (≥80%)",
      implied_credibility >= 0.5 ~ "Medium (50-80%)",
      TRUE ~ "Low (<50%)"
    )
  )

p_credibility <- ggplot(final_model, aes(x = exposure_millions, y = implied_credibility)) +
  geom_point(aes(color = credibility_group), size = 3, alpha = 0.7) +
  geom_hline(yintercept = c(0.5, 0.8), linetype = "dashed", color = "gray50") +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "Implied Credibility by Exposure Level",
    subtitle = "Credibility inversely related to uncertainty multiplier",
    x = "Exposure ($M, log scale)",
    y = "Credibility Weight",
    color = "Credibility Level"
  ) +
  theme_minimal()

p_credibility_blend <- ggplot(final_model, 
                              aes(x = actual_pure_prem, y = prediction, 
                                  color = implied_credibility)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "yellow", high = "red", 
                        midpoint = 0.5, labels = percent) +
  labs(
    title = "Prediction Blend: Model vs Class Experience",
    subtitle = "Color indicates credibility given to class experience",
    x = "Actual Pure Premium (Class Experience)",
    y = "Predicted Pure Premium (Blended)",
    color = "Credibility\nWeight"
  ) +
  theme_minimal()

error_summary <- final_model %>%
  group_by(exposure_group) %>%
  summarise(
    n = n(),
    MAE = mean(abs_error),
    RMSE = sqrt(mean(abs_error^2)),
    MAPE = mean(rel_error) * 100,
    median_error = median(abs_error)
  ) %>%
  pivot_longer(cols = c(MAE, RMSE, MAPE, median_error), 
               names_to = "metric", 
               values_to = "value")

p_error_metrics <- ggplot(error_summary, 
                          aes(x = exposure_group, y = value, fill = metric)) +
  geom_col(position = "dodge", alpha = 0.7) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Prediction Error Metrics by Exposure Group",
    x = "Exposure Group",
    y = "Error Value",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p_error_cdf <- ggplot(final_model, aes(x = rel_error, color = exposure_group)) +
  stat_ecdf(size = 1.2) +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Cumulative Distribution of Relative Errors",
    x = "Relative Error (|Predicted - Actual| / Actual)",
    y = "Cumulative Probability",
    color = "Exposure Group"
  ) +
  theme_minimal()

final_model <- final_model %>%
  mutate(
    premium_gap = prediction - actual_pure_prem,
    premium_gap_pct = premium_gap / actual_pure_prem
  )

top_gaps <- final_model %>%
  arrange(desc(abs(premium_gap))) %>%
  head(20)

p_premium_gaps <- ggplot(top_gaps, 
                         aes(x = reorder(factor(CL), premium_gap), 
                             y = premium_gap,
                             fill = premium_gap > 0)) +
  geom_col(alpha = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                    labels = c("Underpredicted", "Overpredicted")) +
  labs(
    title = "Top 20 Classes by Premium Adequacy Gap",
    subtitle = "Positive = Model overestimates, Negative = Model underestimates",
    x = "Occupation Class",
    y = "Premium Gap (Predicted - Actual)",
    fill = "Direction"
  ) +
  theme_minimal()

portfolio_summary <- final_model %>%
  summarise(
    total_classes = n(),
    total_exposure = sum(total_exposure),
    total_actual_loss = sum(actual_pure_prem * total_exposure / 1000),
    total_predicted_loss = sum(prediction * total_exposure / 1000),
    overall_loss_ratio = total_actual_loss / total_exposure
  )

class_year_matrix <- df %>%
  group_by(CL, YR) %>%
  summarise(
    pure_premium = sum(LOSS) / sum(PR) * 1000,
    .groups = "drop"
  ) %>%
  filter(CL %in% head(unique(class_summary$CL[order(-class_summary$pure_premium)]), 30))

p_heatmap <- ggplot(class_year_matrix, aes(x = factor(YR), y = factor(CL), fill = pure_premium)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = median(class_year_matrix$pure_premium),
                       labels = comma) +
  labs(
    title = "Pure Premium Heatmap: Top 30 Classes by Year",
    x = "Year",
    y = "Occupation Class",
    fill = "Pure Premium"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))

dir.create("plots", showWarnings = FALSE)

ggsave("plots/01_exposure_distribution.png", p_exposure_dist, width = 10, height = 6)
ggsave("plots/02_loss_ratio_distribution.png", p_loss_ratio, width = 10, height = 6)
ggsave("plots/03_pure_premium_top20.png", p_pure_prem_top, width = 10, height = 8)
ggsave("plots/04_temporal_trend.png", p_temporal, width = 10, height = 6)
ggsave("plots/05_exposure_vs_loss.png", p_exposure_loss, width = 10, height = 6)
ggsave("plots/06_freq_by_exposure.png", p_freq_exposure, width = 10, height = 6)

ggsave("plots/07_freq_residuals.png", p_freq_resid, width = 10, height = 6)
ggsave("plots/08_freq_qq.png", p_freq_qq, width = 10, height = 6)
ggsave("plots/09_sev_residuals.png", p_sev_resid, width = 10, height = 6)
ggsave("plots/10_sev_qq.png", p_sev_qq, width = 10, height = 6)
ggsave("plots/11_freq_coefficients.png", p_freq_coefs, width = 10, height = 8)

ggsave("plots/12_actual_vs_predicted.png", p_actual_pred, width = 12, height = 8)
ggsave("plots/13_error_boxplot.png", p_error_boxplot, width = 10, height = 6)
ggsave("plots/14_coverage_analysis.png", p_coverage, width = 10, height = 6)
ggsave("plots/15_interval_width.png", p_interval_width, width = 10, height = 6)

ggsave("plots/16_credibility_by_exposure.png", p_credibility, width = 10, height = 6)
ggsave("plots/17_credibility_blend.png", p_credibility_blend, width = 10, height = 6)

ggsave("plots/18_error_metrics.png", p_error_metrics, width = 12, height = 8)
ggsave("plots/19_error_cdf.png", p_error_cdf, width = 10, height = 6)

ggsave("plots/20_premium_gaps.png", p_premium_gaps, width = 10, height = 8)
ggsave("plots/21_heatmap.png", p_heatmap, width = 10, height = 10)

pdf("plots/22_model_diagnostics_panel.pdf", width = 14, height = 10)
grid.arrange(p_freq_resid, p_freq_qq, p_sev_resid, p_sev_qq, ncol = 2)
dev.off()

pdf("plots/23_performance_summary_panel.pdf", width = 14, height = 10)
grid.arrange(p_actual_pred, p_coverage, p_error_boxplot, p_credibility, ncol = 2)
dev.off()

print("All visualizations saved to 'plots/' directory!")
print(paste0("Total plots created: 23 individual plots + 2 multi-panel PDFs"))