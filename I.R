install.packages('faraway')
library(faraway)
data(savings)

# (1) Hypothesis testing for beta_pop15 = beta_pop75 = beta_dpi = beta_ddpi = 0

# Fit the multiple linear regression model
model <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

# Obtain the residuals
residuals <- residuals(model)

# Calculate the Total Sum of Squares (SST), Residual Sum of Squares (SSE) and
# Regression Sum of Squares (SSR)
SST <- sum((savings$sr - mean(savings$sr))^2)
SSE <- sum(residuals^2)
SSR <- SST - SSE

# Degrees of freedom for each component
df_total <- length(residuals) - 1  # Total sample size minus 1 (for intercept)
df_reg <- 4  # Number of predictors (including intercept)
df_error <- df_total - df_reg

# Calculate Mean Square Total (MST), Mean Square Regression (MSR) and
# Mean Square Error (MSE)
MST <- SST / df_total
MSR <- SSR / df_reg
MSE <- SSE / df_error

# Calculate F-statistic
F_statistic <- MSR / MSE

# alpha for the first test
alpha_1 <- 0.1

# Calculate the critical value from F-distribution for the first test
critical_value_1 <- qf(1 - alpha_1, df_reg, df_error)

#F-statistic and critical value for the first test
cat("F-Statistic for the first test:", F_statistic, "\n")
cat("Critical Value for the first test:", critical_value_1, "\n")

#Perform the F-test for all predictors
if (F_statistic > critical_value_1) {
  cat("Reject the null hypothesis for the first test: At least one coefficient is not equal to zero\n")
} else {
  cat("Fail to reject the null hypothesis for the first test: All coefficients are equal to zero\n")
}

# ANOVA table
anova_table <- data.frame(Source = c("Regression", "Error", "Total"),
                          DF = c(df_reg, df_error, df_total),
                          SS = c(SSR, SSE, SST),
                          MS = c(MSR, MSE, MST),
                          F = c(F_statistic, NA, NA),
                          p_value = c(pf(F_statistic, df_reg, df_error, lower.tail = FALSE), NA, NA))

print(anova_table)


# (2) Hypothesis testing for beta_pop15 = beta_pop75 

# Drop 'pop75' from the model and re-calculate SSR, df_reg, MSR for the second test
model_2 <- lm(sr ~ pop15 + dpi + ddpi, data = savings)
residuals_2 <- residuals(model_2)
SSE_2 <- sum(residuals_2^2)
SSR_2 <- SST - SSE_2
df_reg_2 <- 3  # Since we have one less predictor
MSR_2 <- SSR_2 / df_reg_2

# Calculate F-statistic for the second test
F_statistic_2 <- (MSR - MSR_2) / MSE

#alpha for the second test
alpha_2 <- 0.05

# Calculate the critical value for the second test
critical_value_2 <- qf(1 - alpha_2, 1, df_error)

# Print F-statistic and critical value for the second test
cat("F-Statistic for the second test:", F_statistic_2, "\n")
cat("Critical Value for the second test:", critical_value_2, "\n")

# Perform the F-test for 'pop15' and 'pop75'
if (F_statistic_2 > critical_value_2) {
  cat("Reject the null hypothesis for the second test: beta_pop15 is not equal to beta_pop75\n")
} else {
  cat("Fail to reject the null hypothesis for the second test: beta_pop15 is equal to beta_pop75\n")
}
