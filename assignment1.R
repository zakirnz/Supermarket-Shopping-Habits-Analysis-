set.seed(123)

spend <- rnorm(500,105,15.75 )
qtime <-rpois(500, 7)
famsize <-rpois(500,4)
store <- sample(1:3, 500, replace="TRUE")
shoptype <- rbinom(500, 2, 0.14)
income <-rnorm(500,38000,4500)

weeklyshop <-data.frame (spend,qtime,famsize,store,shoptype,income)
head(weeklyshop)

# Descriptive statistics for quantitative variables
quantitative_vars <- c("spend", "qtime", "famsize", "income")
quant_summary <- summary(weeklyshop[, quantitative_vars])

# Descriptive statistics for categorical variables
categorical_vars <- c("store", "shoptype")
cat_summary <- lapply(weeklyshop[, categorical_vars], function(x) prop.table(table(x)) * 100)

# Display the results
print("Descriptive Statistics for Quantitative Variables:")
print(quant_summary)

quant_sd <- sapply(weeklyshop[, quantitative_vars], sd)

# Display the results
print("Standard Deviation for Quantitative Variables:")
print(quant_sd)

print("\nProportion of Descriptive Statistics for Categorical Variables:")
print(cat_summary)

# Counts
cat_frequencies <- lapply(weeklyshop[, categorical_vars], table)

# Display the results
print("Frequencies for Categorical Variables:")
print(cat_frequencies)

# Plotting a histogram for the "spend" variable
hist(weeklyshop$spend, 
     main="Distribution of Weekly Spending",
     xlab="Amount Spent",
     col="skyblue",
     border="black")

# Scatter plot for the relationship between spend and income
plot(weeklyshop$income, weeklyshop$spend,
     main="Relationship between Income and Weekly Spending",
     xlab="Income",
     ylab="Weekly Income",
     col="blue",
     pch=16)

correlation <- cor(weeklyshop$famsize, weeklyshop$income)
print(correlation)

# Perform ANOVA for the association between spend and store
anova_result <- aov(spend ~ as.factor(store), data = weeklyshop)

# Print the results
print(summary(anova_result)) 