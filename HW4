# HW4
Group members: Bamba Cisse, Jason Seda, Maurice Agonsi, Romulo Jimenez

library(tidyverse)
library(ggplot2)


setwd("..")
setwd("HPS_2020-24")

load("d_HHP2020_24.Rdata")

setwd("..")
setwd("ecob2000_lab3")

d_HHP2020_24$partnered <- (d_HHP2020_24$Mar_Stat == "Married") | 
  (d_HHP2020_24$Mar_Stat == "widowed") | 
  (d_HHP2020_24$Mar_Stat == "divorced") |
  (d_HHP2020_24$Mar_Stat == "separated")

table(d_HHP2020_24$partnered, useNA = "ifany")


table(d_HHP2020_24$Mar_Stat, d_HHP2020_24$partnered)


library(dplyr)
library(ggplot2)

# Summarise share partnered by Age
partner_rate_by_age <- d_HHP2020_24 %>%
  group_by(Age) %>%
  summarise(share_partnered = mean(partnered, na.rm = TRUE))

# Plot it
ggplot(partner_rate_by_age, aes(x = Age, y = share_partnered)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Share Ever Partnered by Age",
       x = "Age",
       y = "Percent partnered (current or past)") +
  theme_minimal()


# If your data has a "cohabiting" or similar category:
d_HHP2020_24$current_partnered <- 
  (d_HHP2020_24$Mar_Stat == "Married") |
  (d_HHP2020_24$Mar_Stat == "Living with partner")

d_HHP2020_24$current_partnered <- d_HHP2020_24$Mar_Stat == "Married


current_partner_rate_by_age <- d_HHP2020_24 %>%
  group_by(Age) %>%
  summarise(share_current_partnered = mean(current_partnered, na.rm = TRUE))


ggplot(current_partner_rate_by_age, aes(x = Age, y = share_current_partnered)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Currently Partnered by Age",
       x = "Age",
       y = "Percent currently partnered") +
  theme_minimal()


library(dplyr)
library(ggplot2)
library(tidyr)  # for pivot_longer

# 1. Create both variables
d_HHP2020_24 <- d_HHP2020_24 %>%
  mutate(
    ever_partnered = (Mar_Stat == "Married") |
      (Mar_Stat == "widowed") |
      (Mar_Stat == "divorced") |
      (Mar_Stat == "separated"),
    current_partnered = (Mar_Stat == "Married") |
      (Mar_Stat == "Living with partner")
  )

# 2. Summarise both by Age
partner_rates_by_age <- d_HHP2020_24 %>%
  group_by(Age) %>%
  summarise(
    ever_partnered = mean(ever_partnered, na.rm = TRUE),
    current_partnered = mean(current_partnered, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(ever_partnered, current_partnered),
               names_to = "partner_type",
               values_to = "share")

# 3. Plot both lines on the same graph
ggplot(partner_rates_by_age, aes(x = Age, y = share, color = partner_type)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("ever_partnered" = "steelblue",
                                "current_partnered" = "darkgreen"),
                     labels = c("Ever partnered", "Currently partnered")) +
  labs(title = "Partnering by Age",
       x = "Age",
       y = "Percent",
       color = "Partnership status") +
  theme_minimal()


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

# Create sample data for income by marital status
set.seed(123)  # For reproducibility

# Define marital status categories
marital_status <- c("Single", "Married", "Widowed", "Divorced", "Separated")

# Generate sample data - number of individuals in each marital status category
num_individuals <- c(
  Single = sample(200:500, 1),
  Married = sample(600:900, 1),
  Widowed = sample(100:300, 1),
  Divorced = sample(150:400, 1),
  Separated = sample(50:200, 1)
)

# Generate average income for each marital status (in thousands)
avg_income <- c(
  Single = sample(35:55, 1),
  Married = sample(65:95, 1),
  Widowed = sample(25:45, 1),
  Divorced = sample(40:60, 1),
  Separated = sample(30:50, 1)
)

# Calculate total income for each marital status
total_income <- num_individuals * avg_income

# Create line graph showing average income by marital status
plot(1:length(marital_status), avg_income, 
     type = "b",
     main = "Average Income by Marital Status",
     xlab = "Marital Status",
     ylab = "Average Income (in thousands)",
     xaxt = "n",
     col = "blue",
     lwd = 2,
     ylim = c(0, max(avg_income) * 1.1))
axis(1, at = 1:length(marital_status), labels = marital_status)
