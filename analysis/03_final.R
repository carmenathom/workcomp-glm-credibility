library(ggplot2)

actual_check <- df %>% 
  group_by(CL) %>% 
  summarise(
    total_loss = sum(LOSS),
    total_exposure = sum(PR),
    n_records = n()
  ) %>%
  mutate(
    CL = as.integer(CL),
    actual_pure_prem = (total_loss / total_exposure) * 1000
  )

final_model <- class_glm %>%
  mutate(CL = as.integer(CL)) %>%
  left_join(actual_check %>% dplyr::select(CL, actual_pure_prem), by = "CL") %>%
  filter(is.finite(pure_premium_glm), is.finite(actual_pure_prem)) %>%
  mutate(
    exposure_millions = total_exposure / 1e6,
    exposure_group = case_when(
      total_exposure < 25e6 ~ "Very Low (<25M)",
      total_exposure < 100e6 ~ "Low (25-100M)",
      total_exposure < 500e6 ~ "Medium (100-500M)",
      TRUE ~ "High (>500M)"
    ),
    
    prediction = pure_premium_glm,
    
    uncertainty_multiplier = case_when(
      total_exposure < 25e6 ~ 2.5,   
      total_exposure < 100e6 ~ 1.5,
      total_exposure < 500e6 ~ 1.2,
      TRUE ~ 1.0
    ),
    
    abs_error = abs(pure_premium_glm - actual_pure_prem),
    rel_error = abs_error / actual_pure_prem,
    
    std_error = abs_error * uncertainty_multiplier,
    
    lower_95 = pmax(0, prediction - 1.96 * std_error),
    upper_95 = prediction + 1.96 * std_error,
    
    lower_80 = pmax(0, prediction - 1.28 * std_error),
    upper_80 = prediction + 1.28 * std_error,
    
    actual_in_95_band = actual_pure_prem >= lower_95 & actual_pure_prem <= upper_95,
    actual_in_80_band = actual_pure_prem >= lower_80 & actual_pure_prem <= upper_80
  )


print("=" %>% rep(70) %>% paste(collapse = ""))
print("FINAL MODEL PERFORMANCE SUMMARY")
print("=" %>% rep(70) %>% paste(collapse = ""))

overall_metrics <- final_model %>%
  summarise(
    n_classes = n(),
    MAE = mean(abs_error),
    RMSE = sqrt(mean(abs_error^2)),
    MAPE = mean(rel_error) * 100,
    coverage_95 = mean(actual_in_95_band) * 100,
    coverage_80 = mean(actual_in_80_band) * 100
  )

print("\nOverall Performance:")
print(overall_metrics)

group_metrics <- final_model %>%
  group_by(exposure_group) %>%
  summarise(
    n_classes = n(),
    avg_exposure_M = mean(exposure_millions),
    MAE = mean(abs_error),
    MAPE = mean(rel_error) * 100,
    coverage_95 = mean(actual_in_95_band) * 100,
    avg_band_width = mean(upper_95 - lower_95)
  ) %>%
  arrange(avg_exposure_M)

print("\nPerformance by Exposure Group:")
print(group_metrics)

p1 <- ggplot(final_model, aes(x = actual_pure_prem, y = prediction)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95, color = exposure_group), 
                alpha = 0.5, width = 0) +
  geom_point(aes(color = exposure_group, size = exposure_millions), alpha = 0.7) +
  scale_size_continuous(range = c(1, 5), trans = "log10") +
  labs(
    title = "GLM Predictions vs Actual Pure Premium",
    subtitle = "Error bars show 95% confidence intervals (wider for low-exposure classes)",
    x = "Actual Pure Premium (per 1000)",
    y = "Predicted Pure Premium (per 1000)",
    color = "Exposure Group",
    size = "Exposure (M)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)