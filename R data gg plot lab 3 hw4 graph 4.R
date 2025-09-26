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