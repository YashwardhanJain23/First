# Given Data
X1 <- c(-1, -1, 0, 1, 1)
X2 <- c(-1, 0, 0, 0, 1)
Y <- c(7.2, 8.1, 9.8, 12.3, 12.9)
n <- length(X1)

# Null Hypothesis: beta1 = 2*beta2 vs Alternative Hypothesis: beta1 != 2*beta2 

# Define the Z matrix
Z <- matrix(c(1, 1, 1, 1, 1, -3, -2, 0, 2, 3), ncol = 2)

Z_t <- t(Z)
Z_t_Z <- Z_t %*% Z
Z_t_Z_inv <- solve(Z_t_Z)
Z_t_Y <- Z_t %*% Y

# Calculate alpha
alpha <- Z_t_Z_inv %*% Z_t_Y

# Calculate SSE_new
SSE_new <- t(Y) %*% Y - t(alpha) %*% Z_t_Y
SSE <- 0.107 

# Degrees of freedom
q <- 1
p <- 2

# Calculate F-test statistic
F_test_stat <- ((SSE_new - SSE) / q) / (SSE / (n - p - 1))

# Set significance level
alpha_level <- 0.05

# Calculate critical value
critical_value <- qf(1 - alpha_level, q, n - p - 1, lower.tail = FALSE)

# Perform hypothesis test
if (F_test_stat > critical_value) {
  cat("Reject the null hypothesis")
} else {
  cat("Do not reject the null hypothesis")
}
