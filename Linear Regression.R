library(tidyverse)

# linear Regression
combined <- readRDS("combined.rds")


linear_model <- lm(sales_value ~ dead_value, data = combined)
summary(linear_model)

# Call:
#   lm(formula = sales_value ~ dead_value, data = combined)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -9651  -3372  -1200   1191  19074 
# 
# Coefficients:
#   Estimate Std. Error
# (Intercept) 1405.6213  1695.3839
# dead_value     1.2749     0.0854
# t value Pr(>|t|)    
# (Intercept)   0.829    0.413    
# dead_value   14.929   <2e-16 ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05
# ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5827 on 34 degrees of freedom
# Multiple R-squared:  0.8676,	Adjusted R-squared:  0.8637 
# F-statistic: 222.9 on 1 and 34 DF,  p-value: < 2.2e-16



# Logistic Regression

combined <- combined %>% 
  mutate(
    loss_ratio_pct = round((dead_value / sales_value)* 100, 2),
    high_risk_flag = ifelse(loss_ratio_pct > 50, 1, 0)
        )

str(combined)
combined %>% filter(is.na(high_risk_flag) | sales_value == 0)

model_logit_real <- glm(high_risk_flag ~ sales_value, data = combined, family = "binomial")
summary(model_logit_real)
 # Call:
#   glm(formula = high_risk_flag ~ sales_value, family = "binomial", 
#       data = combined)
# 
# Coefficients:
#   Estimate Std. Error
# (Intercept) 4.578e-01  7.428e-01
# sales_value 4.719e-05  3.560e-05
# z value Pr(>|z|)
# (Intercept)   0.616    0.538
# sales_value   1.326    0.185
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 35.028  on 34  degrees of freedom
# Residual deviance: 32.867  on 33  degrees of freedom
# (1 observation deleted due to missingness)
# AIC: 36.867
# 
# Number of Fisher Scoring iterations: 5

exp(coef(model_logit_real)["sales_value"]* 1000)
# (Intercept) sales_value 
# 1.580572    1.000047

# sales_value 
# 1.048324 