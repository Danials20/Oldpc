## Prac test 1 W4
#Q1
blood_data <- read.table("blood.txt", header = TRUE)
selenium_levels <- blood_data$selen
t_test_result <- t.test(selenium_levels, mu = 29.78, alternative = "greater")
test_statistic <- t_test_result$statistic
print(test_statistic)  # This will display the t-statistic to 3 decimal places
p_value <- t_test_result$p.value
print(p_value)  # This will display the p-value to 3 decimal places
if (p_value < 0.05) {
  print("Reject H0")
} else {
  print("Do not reject H0")
}
if (p_value < 0.01) {
  print("Reject H0")
} else {
  print("Do not reject H0")
}

#Q2
petrol_data <- read.table("petrol.txt", header = TRUE)
melbourne_prices <- petrol_data$Melbourne
brisbane_prices <- petrol_data$Brisbane
t_test_result <- t.test(melbourne_prices, brisbane_prices, alternative = "two.sided")
test_statistic <- abs(t_test_result$statistic)
print(round(test_statistic, 3))

p_value <- t_test_result$p.value
print(p_value)  # This will display the p-value to 3 decimal places
if (p_value < 0.05) {
  print("Reject H0")
} else {
  print("Do not reject H0")
}
if (p_value < 0.01) {
  print("Reject H0")
} else {
  print("Do not reject H0")
}

t_critical_95 <- qt(1 - 0.05/2, df = 48)
print(round(abs(t_critical_95), 3))
# Sample sizes
n1 <- length(melbourne_prices)
n2 <- length(brisbane_prices)

# Sample means and variances
mean_melbourne <- mean(melbourne_prices)
mean_brisbane <- mean(brisbane_prices)
var_melbourne <- var(melbourne_prices)
var_brisbane <- var(brisbane_prices)

# Difference in means
mean_diff <- mean_melbourne - mean_brisbane

# Standard error of the difference in means
se_diff <- sqrt(var_melbourne/n1 + var_brisbane/n2)

# 95% CI lower and upper bounds
ci_lower_95 <- mean_diff - t_critical_95 * se_diff
ci_upper_95 <- mean_diff + t_critical_95 * se_diff

print(paste("95% CI: (", round(ci_lower_95, 3), ",", round(ci_upper_95, 3), ")"))

t_critical_89 <- qt(1 - 0.11/2, df = 48)
print(round(abs(t_critical_89), 3))

# 89% CI lower and upper bounds
ci_lower_89 <- mean_diff - t_critical_89 * se_diff
ci_upper_89 <- mean_diff + t_critical_89 * se_diff

print(paste("89% CI: (", round(ci_lower_89, 3), ",", round(ci_upper_89, 3), ")"))
