freq_data <- df %>%
  mutate(claim_count = ifelse(LOSS > 0, 1, 0)) %>%
  filter(exposure > 0)

freq_model <- glm(
  claim_count ~ factor(CL) + factor(YR),
  family = poisson(link = "log"),
  offset = log(exposure),
  data = freq_data
)

print(summary(freq_model))
deviance_ratio <- freq_model$deviance / freq_model$df.residual
print(paste("Dispersion parameter:", round(deviance_ratio, 3)))

sev_data <- df %>%
  filter(LOSS > 0) %>%
  mutate(
    CL = factor(CL),
    avg_severity = LOSS
  )

sev_model <- glm(
  avg_severity ~ factor(CL) + factor(YR),
  family = Gamma(link = "log"),
  data = sev_data
)

summary(sev_model)
plot(sev_model, which = 1:2)

sev_train_levels <- levels(sev_data$CL)

glm_predictions <- df %>%
  filter(CL %in% sev_train_levels) %>%
  mutate(
    freq_pred = predict(freq_model, newdata = ., type = "response"),
    sev_pred = predict(sev_model, newdata = ., type = "response"),
    
    expected_loss = freq_pred * sev_pred,
    pure_premium_glm = (expected_loss / PR) * 1000
  )

class_glm <- glm_predictions %>%
  group_by(CL) %>%
  summarise(
    pure_premium_glm = weighted.mean(pure_premium_glm, PR),  
    total_exposure = sum(PR)  
  )
