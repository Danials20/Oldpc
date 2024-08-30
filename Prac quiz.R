## Prac test 1 W4
#Q1a
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


###New Quiz

#Q1a
# Load the data
getwd() ##check directory


data <- read.table("blood2.txt", header = TRUE)
selenium_levels <- data$selen
t_test_result <- t.test(selenium_levels, mu = 15.76, alternative = "greater")
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

###Q1b
# Degrees of freedom
df <- 21 - 1
# Critical value for a two-sided 95% CI
critical_value_95 <- qt(0.975, df)  # 0.975 because it's two-sided, and we want the upper tail probability
abs_critical_value_95 <- abs(critical_value_95)
# Print the result rounded to 3 decimal places
print(paste("Critical value for 95% CI: ", round(abs_critical_value_95, 3)))
#
# Calculate the sample mean and standard deviation
selenium_data <- data
mean_selenium <- mean(selenium_data$selen)
std_dev <- sd(selenium_data$selen)
n <- length(selenium_data$selen)
# Compute the margin of error
margin_error_95 <- abs_critical_value_95 * (std_dev / sqrt(n))
# Compute the lower and upper bounds
lower_bound_95 <- mean_selenium - margin_error_95
upper_bound_95 <- mean_selenium + margin_error_95
# Print the results rounded to 3 decimal places
print(paste("Lower bound for 95% CI: ", round(lower_bound_95, 3)))
print(paste("Upper bound for 95% CI: ", round(upper_bound_95, 3)))
# Critical value for a two-sided 98% CI
critical_value_98 <- qt(0.99, df)  # 0.99 because it's two-sided, and we want the upper tail probability
abs_critical_value_98 <- abs(critical_value_98)
# Print the result rounded to 3 decimal places
print(paste("Critical value for 98% CI: ", round(abs_critical_value_98, 3)))
#
# Compute the margin of error for 98% CI
margin_error_98 <- abs_critical_value_98 * (std_dev / sqrt(n))
# Compute the lower and upper bounds for 98% CI
lower_bound_98 <- mean_selenium - margin_error_98
upper_bound_98 <- mean_selenium + margin_error_98
# Print the results rounded to 3 decimal places
print(paste("Lower bound for 98% CI: ", round(lower_bound_98, 3)))
print(paste("Upper bound for 98% CI: ", round(upper_bound_98, 3)))

##########################################################################################

#Q2
petrol_data2 <- read.table("petrol.txt", header = TRUE)
melbourne_prices <- petrol_data2$Melbourne
perth_prices <- petrol_data2$Perth
t_test_result <- t.test(melbourne_prices, perth_prices, alternative = "two.sided")
test_statistic <- abs(t_test_result$statistic)
print(test_statistic)

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
n2 <- length(perth_prices)

# Sample means and variances
mean_melbourne <- mean(melbourne_prices)
mean_perth <- mean(perth_prices)
var_melbourne <- var(melbourne_prices)
var_perth <- var(perth_prices)

# Difference in means
mean_diff <- mean_melbourne - mean_perth

# Standard error of the difference in means
se_diff <- sqrt(var_melbourne/n1 + var_perth/n2)

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

#####Q2b
critical_value_95 <- qnorm(0.975) # Two-tailed test: 1 - (1 - 0.95) / 2
abs_critical_value_95 <- abs(critical_value_95)
round(abs_critical_value_95, 3)

##_______________________
mean_diff <- # your mean difference
  sd_diff <- # your standard deviation of the difference
  n1 <- # sample size from Perth
  n2 <- # sample size from Melbourne
  
  # Standard error of the mean difference
  se_diff <- sqrt((sd_diff^2 / n1) + (sd_diff^2 / n2))
# Margin of error for 95% CI
margin_error_95 <- critical_value_95 * se_diff
# Bounds for 95% CI
lower_bound_95 <- mean_diff - margin_error_95
upper_bound_95 <- mean_diff + margin_error_95
round(lower_bound_95, 3)
round(upper_bound_95, 3)

critical_value_94 <- qnorm(0.97) # Two-tailed test: 1 - (1 - 0.94) / 2
abs_critical_value_94 <- abs(critical_value_94)
round(abs_critical_value_94, 3)
##
# Margin of error for 94% CI
margin_error_94 <- critical_value_94 * se_diff
# Bounds for 94% CI
lower_bound_94 <- mean_diff - margin_error_94
upper_bound_94 <- mean_diff + margin_error_94
round(lower_bound_94, 3)
round(upper_bound_94, 3)

#########################################
##Q5a
# Load data
Gdata <- read.table("grocery.txt", header = TRUE)  # Adjust as needed
monday_prices <- Gdata$Monday  # Adjust column name as needed
friday_prices <- Gdata$Friday  # Adjust column name as needed

# Perform paired t-test
t_test_result <- t.test(monday_prices, friday_prices, paired = TRUE)

# Absolute value of the test statistic
abs_test_statistic <- abs(t_test_result$statistic)
round(abs_test_statistic, 3)

# P-Value
p_value <- t_test_result$p.value
print(p_value, 3)


# Extract 95% CI limits
ci_95 <- t_test_result$conf.int
# Compute the length of the 95% CI
length_ci_95 <- ci_95[2] - ci_95[1]
# Print the result rounded to 3 decimal places
round(length_ci_95, 3)

#____________________
# Define confidence level and alpha
alpha_75 <- 1 - 0.75
critical_value_75 <- qt(1 - alpha_75 / 2, df = length(monday_prices) - 1)

# Compute the standard error of the difference
mean_diff <- mean(monday_prices) - mean(friday_prices)
sd_diff <- sd(monday_prices - friday_prices)
se_diff <- sd_diff / sqrt(length(monday_prices))

# Compute margin of error for 75% CI
margin_error_75 <- critical_value_75 * se_diff

# Compute the 75% CI limits
lower_ci_75 <- mean_diff - margin_error_75
upper_ci_75 <- mean_diff + margin_error_75
# Compute the length of the 75% CI
length_ci_75 <- upper_ci_75 - lower_ci_75
# Print the result rounded to 3 decimal places
round(length_ci_75, 3)



#####REAL DEAL
#Q1a
# Load the data
getwd() ##check directory

# Load the data
stall <- read.table("stall.txt", header = TRUE)
stall_angle <- stall$angle_of_attack

# Perform a one-sample t-test
t_test_result <- t.test(stall_angle, mu = 20, alternative = "less")

# Extract test statistic and p-value
test_statistic <- t_test_result$statistic
p_value <- t_test_result$p.value

# Print test statistic and p-value rounded to 3 decimal places
print(round(test_statistic, 3))
print(round(p_value, 3))

# Determine statistical outcome for ?? = 0.05
alpha_05 <- 0.05
if (p_value < alpha_05) {
  print("Reject H0")
} else {
  print("Do not reject H0")
}

# Determine statistical outcome for ?? = 0.005
alpha_005 <- 0.005
if (p_value < alpha_005) {
  print("Reject H0")
} else {
  print("Do not reject H0")
}

# Compute the critical value for a two-sided 95% CI
critical_value_95 <- qt(0.975, df = length(stall_angle) - 1)
print(round(critical_value_95, 3))

# Sample mean and standard deviation
mean_angle <- mean(stall_angle)
sd_angle <- sd(stall_angle)

# Compute the standard error
SE <- sd_angle / sqrt(length(stall_angle))

# Compute the lower and upper bounds for a two-sided 95% CI
lower_bound_95 <- mean_angle - critical_value_95 * SE
upper_bound_95 <- mean_angle + critical_value_95 * SE

# Print the bounds rounded to 3 decimal places
print(round(lower_bound_95, 3))
print(round(upper_bound_95, 3))

# Compute the critical value for a two-sided 75% CI
critical_value_75 <- qt(0.875, df = length(stall_angle) - 1)
print(round(critical_value_75, 3))

# Compute the lower and upper bounds for a two-sided 75% CI
lower_bound_75 <- mean_angle - critical_value_75 * SE
upper_bound_75 <- mean_angle + critical_value_75 * SE

# Print the bounds rounded to 3 decimal places
print(round(lower_bound_75, 3))
print(round(upper_bound_75, 3))



# Critical value for 75% CI
critical_value_75 <- qt(0.875, df)
abs_critical_value_75 <- abs(critical_value_75)

# Print result rounded to 3 decimal places
print(round(abs_critical_value_75, 3))

# CI bounds for 75% CI
ci_lower_75 <- mean_angle - critical_value_75 * se
ci_upper_75 <- mean_angle + critical_value_75 * se

# Print results rounded to 3 decimal places
print(round(ci_lower_75, 3))
print(round(ci_upper_75, 3))






# Load the data
time <- read.table("time.txt", header = TRUE)
max_sidepods <- data$max_sidepods
min_sidepods <- data$min_sidepods

# Perform the two-sample t-test
t_test_result <- t.test(max_sidepods, min_sidepods, var.equal = TRUE)

# Absolute value of the test statistic
test_statistic <- abs(t_test_result$statistic)
print(round(test_statistic, 3))

# P-value
p_value <- t_test_result$p.value
print(round(p_value, 3))


# Compute sample means and variances
mean_max <- mean(max_sidepods)
mean_min <- mean(min_sidepods)
var_max <- var(max_sidepods)
var_min <- var(min_sidepods)

# Difference in means
mean_diff <- mean_max - mean_min

# Standard error of the difference
se_diff <- sqrt(var_max/length(max_sidepods) + var_min/length(min_sidepods))

# Critical value for 95% CI
t_critical_95 <- qt(0.975, df = 76)

# 95% CI bounds
ci_lower_95 <- mean_diff - t_critical_95 * se_diff
ci_upper_95 <- mean_diff + t_critical_95 * se_diff

# Length of the 95% CI
length_ci_95 <- ci_upper_95 - ci_lower_95
print(round(length_ci_95, 3))

# Critical value for 99% CI
t_critical_99 <- qt(0.995, df = 76)

# 99% CI bounds
ci_lower_99 <- mean_diff - t_critical_99 * se_diff
ci_upper_99 <- mean_diff + t_critical_99 * se_diff

# Length of the 99% CI
length_ci_99 <- ci_upper_99 - ci_lower_99
print(round(length_ci_99, 3))

#Q5a
# Load the data
data <- read.table("injury.txt", header = TRUE)

# Extract the proportions for Victoria and Australian Capital Territory
victoria_proportions <- data$Victoria
act_proportions <- data$Australian_Capital_Territory

# Perform a two-sample t-test
t_test_result <- t.test(victoria_proportions, act_proportions, var.equal = TRUE)

# Compute the absolute value of the test statistic
test_statistic <- abs(t_test_result$statistic)

# Print the test statistic rounded to 3 decimal places
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


# Compute the critical value for a two-sided 95% CI
critical_value_95 <- qt(0.975, df = 46)
print(round(critical_value_95, 3))

# Example values for mean difference and SE
mean_diff <- 4 # Example mean difference
SE <- 1 # Example standard error

# Compute the CI bounds
lower_bound_95 <- mean_diff - critical_value_95 * SE
upper_bound_95 <- mean_diff + critical_value_95 * SE
print(round(lower_bound_95, 3))
print(round(upper_bound_95, 3))

# Compute the critical value for a two-sided 82% CI
critical_value_82 <- qt(0.91, df = 46)
print(round(critical_value_82, 3))


# Compute the CI bounds
lower_bound_82 <- mean_diff - critical_value_82 * SE
upper_bound_82 <- mean_diff + critical_value_82 * SE
print(round(lower_bound_82, 3))
print(round(upper_bound_82, 3))

