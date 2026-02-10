# Credibility-Weighted Pure Premium Modeling for Workers’ Compensation


## Overview
This project develops an actuarial pure premium model for workers’ compensation insurance using NCCI industry data across 121 occupation classes and 7 accident years. The analysis applies a frequency–severity Generalized Linear Model (GLM) framework to estimate baseline loss costs and incorporates exposure-based credibility principles to account for varying levels of class experience. 

The modeling approach reflects standard CAS ratemaking practice by separating claim frequency and claim severity, using payroll as an exposure offset, and evaluating predictive uncertainty and credibility as a function of exposure volume. The resulting framework produces class-level pure premium estimates with uncertainty bands that widen appropriately for low-exposure classes and stabilize for high-exposure classes.


## Methodology
### Data:
- Source: NCCI workers' compensation (121 classes, 7 years, 847 observations)
- Structure:
  - 121 occupation classes
  - 7 years of experience
  - 847 class–year observations
- Key variables:
  - Payroll exposure (PR)
  - Total losses (LOSS)
    
Pure premium is defined as losses per $1,000 of payroll.

### Frequency Modeling:
- Claim frequency is modeled using a Poisson GLM
- Payroll exposure enters the model as a log offset
- Rating variables:
  - Occupation class
  - Accident year
 
### Severity Modeling
- Claim severity (conditional on positive loss) is modeled using a Gamma GLM with a log link
- Occupation class and accident year effects capture heterogeneity in loss severity
- Distributional assumptions are validated using residual and Q–Q diagnostics

### Pure Premium Estimation
- Expected losses are calculated as the product of predicted frequency and predicted severity
- Pure premiums are aggregated to the class level using payroll-weighted averages
- Model estimates are compared against observed class experience

### Credibility and Uncertainty Treatment
Rather than applying a closed-form Bühlmann formula, the project incorporates credibility concepts implicitly through exposure-based uncertainty adjustments:
- Classes are grouped by total payroll exposure
- Lower-exposure classes receive higher uncertainty multipliers
- Prediction intervals widen as exposure decreases and narrow as exposure increases

### Model Evaluation
- Performance is assessed using:
  - Mean Absolute Error (MAE)
  - Root Mean Squared Error (RMSE)
  - Mean Absolute Percentage Error (MAPE)
  - Coverage of 80% and 95% prediction intervals

## Files
- `/analysis`: R scripts for each modeling phase
- `/analysis/figures`: Visualizations of results
