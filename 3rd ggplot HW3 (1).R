# Set seed for reproducibility
set.seed(123)

# Create a simulated dataset with income and marriage status
n <- 1000  # Number of observations

# Marriage status categories
marriage_status <- sample(c("Single", "Married", "Divorced", "Widowed"), 
                          n, replace = TRUE, 
                          prob = c(0.3, 0.4, 0.2, 0.1))

# Generate income based on marriage status (with some variation)
income <- numeric(n)
for (i in 1:n) {
  if (marriage_status[i] == "Single") {
    income[i] <- rnorm(1, mean = 45000, sd = 10000)
  } else if (marriage_status[i] == "Married") {
    income[i] <- rnorm(1, mean = 75000, sd = 15000)
  } else if (marriage_status[i] == "Divorced") {
    income[i] <- rnorm(1, mean = 50000, sd = 12000)
  } else {  # Widowed
    income[i] <- rnorm(1, mean = 40000, sd = 10000)
  }
}

# Ensure income is positive
income <- abs(income)

# Create a data frame
income_data <- data.frame(mar_stat = marriage_status, income = income)

# View summary statistics by marriage status
summary_stats <- aggregate(income ~ mar_stat, data = income_data, 
                           FUN = function(x) c(mean = mean(x), 
                                               sd = sd(x), 
                                               min = min(x), 
                                               max = max(x)))
print(summary_stats)

# Create a boxplot of income by marriage status
boxplot(income ~ mar_stat, data = income_data,
        main = "Income Distribution by Marriage Status",
        xlab = "Marriage Status",
        ylab = "Income ($)",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
        border = "darkblue")

# Add means to the plot
means <- tapply(income_data$income, income_data$mar_stat, mean)
points(1:length(means), means, pch = 18, col = "red", cex = 1.5)
text(1:length(means), means, labels = round(means), pos = 3, col = "red")

# Perform ANOVA to test if differences are statistically significant
anova_result <- aov(income ~ mar_stat, data = income_data)
summary(anova_result)

# If ANOVA is significant, perform post-hoc tests
if (summary(anova_result)[[1]]$'Pr(>F)'[1] < 0.05) {
  print("Performing Tukey's HSD post-hoc test:")
  print(TukeyHSD(anova_result))
}