library(CASdatasets)
library(tidyverse)
library(MASS) 

data(usworkcomp)
glimpse(usworkcomp)

# Variables:
# CL: Occupation class (1-121)
# YR: Year (1-7)
# PR: Payroll exposure (in dollars)
# LOSS: Total losses (in dollars)

df <- usworkcomp %>%
  mutate(
    freq = ifelse(LOSS > 0, 1, 0), 
    
    loss_ratio = LOSS / PR,
    
    exposure = PR / 1000000,  
    
    pure_premium = LOSS / PR * 1000,  
    
    log_exposure = log(exposure + 1),
    
    exposure_category = case_when(
      exposure < quantile(exposure, 0.33) ~ "Low",
      exposure < quantile(exposure, 0.67) ~ "Medium",
      TRUE ~ "High"
    )
  )

class_summary <- df %>%
  group_by(CL) %>%
  summarise(
    n_years = n(),
    total_exposure = sum(exposure),
    total_loss = sum(LOSS),
    avg_loss_ratio = mean(loss_ratio, na.rm = TRUE),
    sd_loss_ratio = sd(loss_ratio, na.rm = TRUE),
    pure_premium = sum(LOSS) / sum(PR) * 1000
  )


