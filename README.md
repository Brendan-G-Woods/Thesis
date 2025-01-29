# Load required libraries
library(ggplot2)
library(hrbrthemes)
library(readr)
library(dplyr)
library(reshape2) # For correlation matrix visualization

# Load the dataset
data <- read.csv("~/Documents/data.csv")

# View the structure and summary of the dataset
str(data)
summary(data)

cat("\nSummary of the dataset:\n")
summary(data)

# Create a binary column for Insulin
data$binary_insulin <- 0
data$binary_insulin[data$InsulinYN == "Yes"] <- 1

# Proportion test for Insulin usage between groups
StudyInsulinTest <- prop.test(
  x = c(
    sum(data$InsulinYN[data$Group == "Metformin"] == "Yes", na.rm = TRUE),
    sum(data$InsulinYN[data$Group == "Placebo"] == "Yes", na.rm = TRUE)
  ),
  n = c(
    sum(data$Group == "Metformin"),
    sum(data$Group == "Placebo")
  )
)
print(StudyInsulinTest)

# Perform t-tests for binary_insulin, gw32, and gw38
insulin_t <- t.test(data$binary_insulin ~ data$Group)
gw32_t <- t.test(data$gw32 ~ data$Group)
gw38_t <- t.test(data$gw38 ~ data$Group)

# Print the t-test results
cat("T-test Results:\n")
print(insulin_t)
print(gw32_t)
print(gw38_t)

# Calculate composite scores
y_squared_composite <- (insulin_t$statistic^2) + (gw32_t$statistic^2) + (gw38_t$statistic^2)
study_non_squared_composite <- insulin_t$statistic + gw32_t$statistic + gw38_t$statistic

cat("\nComposite Scores:\n")
cat("Squared Composite:", y_squared_composite, "\n")
cat("Non-Squared Composite:", study_non_squared_composite, "\n")

# Visualization: Proportion of Insulin Usage by Group
ggplot(data, aes(x = Group, fill = InsulinYN)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Insulin Usage by Group",
    y = "Proportion",
    x = "Group"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# Visualization: Boxplots for gw32 and gw38 with cutoff line at 5.1 and p-values
ggplot(data, aes(x = Group, y = gw32, fill = Group)) +
  geom_boxplot() +
  geom_hline(yintercept = 5.1, linetype = "dashed", color = "red") +
  annotate("text", x = 1.5, y = max(data$gw32, na.rm = TRUE), 
           label = paste("p-value:", round(gw32_t$p.value, 4)), 
           color = "blue", size = 4) +
  labs(
    title = "Gestational Week 32 by Group",
    y = "Week 32",
    x = "Group",
    caption = "Dashed red line represents the cutoff at 5.1"
  ) +
  theme_minimal()

ggplot(data, aes(x = Group, y = gw38, fill = Group)) +
  geom_boxplot() +
  geom_hline(yintercept = 5.1, linetype = "dashed", color = "red") +
  annotate("text", x = 1.5, y = max(data$gw38, na.rm = TRUE), 
           label = paste("p-value:", round(gw38_t$p.value, 4)), 
           color = "blue", size = 4) +
  labs(
    title = "Gestational Week 38 by Group",
    y = "Week 38",
    x = "Group",
    caption = "Dashed red line represents the cutoff at 5.1"
  ) +
  theme_minimal()

# Calculate and display correlation matrix
correlation_vars <- data %>%
  select(gw32, gw38, binary_insulin) 
correlation_matrix <- cor(correlation_vars, use = "complete.obs", method = "pearson")

cat("\nCorrelation Matrix (Pearson):\n")
print(correlation_matrix)

# Plot correlation matrix as a heatmap
correlation_melted <- melt(correlation_matrix)

ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(
    title = "Correlation Matrix (Pearson)",
    x = "Variables",
    y = "Variables"
  ) +
  theme_minimal()
