#------------------------- Setting Working Directory ----------------------------------#
setwd("//Users//subashyadav//Documents//Practicum//Project 1//")

#------------------------- Installing and Loading Libraries --------------------------#
# Uncomment the below lines if these packages are not already installed
# install.packages("gbm")
# install.packages("randomForest")
# install.packages("ggcorrplot")

#-------------------------Importing All The Libraries------------------------------------#
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)
library(reshape2)
library(caret)
library(FactoMineR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(car)
library(cluster)
library(factoextra)
library(glmnet)
library(cluster)

#------------------------- Importing Dataset -----------------------------------------#
#imporing dataset
employee_data <- read.csv("../data/Employee.csv")

# Check dataset structure
names(employee_data)
str(employee_data)

#------------------------- Data Preprocessing ----------------------------------------#
# Convert binary variables to factors
employee_data$Work_accident <- as.factor(employee_data$Work_accident)
employee_data$left <- as.factor(employee_data$left)
employee_data$promotion_last_5years <- as.factor(employee_data$promotion_last_5years)

# Rename columns for consistency
colnames(employee_data)[which(names(employee_data) == "Work_accident")] <- "work_accident"


#------------------------- Exploratory Data Analysis ---------------------------------#

# Calculate the number of missing values for each variable
missing_data <- employee_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Variable", value = "MissingCount")

# Create a bar plot for missing values
ggplot(missing_data, aes(x = Variable, y = MissingCount, fill = Variable)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  ggtitle("Count of Missing Values in Each Variable") +
  xlab("Variable") +
  ylab("Number of Missing Values") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summary statistics for numerical variables
summary_stats <- summary(select(employee_data, satisfaction_level, last_evaluation, number_project, average_montly_hours, time_spend_company))
print("Summary Statistics:")
print(summary_stats)

#------------------------- Hypothesis Testing ---------------------------------------#

# Salary Distribution by Turnover
# Ensure salary levels are ordered as "low", "medium", "high"
employee_data$salary <- factor(employee_data$salary, levels = c("low", "medium", "high"))

# Plot Salary Distribution by Turnover
ggplot(employee_data, aes(x = salary, fill = left)) +
  geom_bar(position = "fill") +
  ggtitle("Salary Distribution by Turnover") +
  xlab("Salary Level") +
  ylab("Proportion") +
  scale_fill_manual(values = c("steelblue", "steelblue4"), name = "Turnover", labels = c("Stayed", "Left")) +
  theme_minimal()

# Summarize the proportion of employees who left by salary level
salary_turnover_summary <- employee_data %>%
  group_by(salary) %>%
  summarise(
    total_employees = n(),
    employees_left = sum(left == 1),
    proportion_left = employees_left / total_employees
  )

# Conduct a Chi-Square test to see if there is a statistically significant association between salary and employee turnover
salary_turnover_table <- table(employee_data$salary, employee_data$left)

# Perform Chi-Square test
chi_square_test <- chisq.test(salary_turnover_table)

# Display the result of the Chi-Square test
print(chi_square_test)




# Turnover by Work Accident
ggplot(employee_data, aes(x = work_accident, fill = left)) +
  geom_bar(position = "fill") +
  ggtitle("Turnover Rate by Work Accident") +
  xlab("Work Accident (0 = No, 1 = Yes)") +
  ylab("Proportion") +
  scale_fill_manual(values = c("steelblue", "steelblue4"), name = "Turnover") +
  theme_minimal()

# Summarize data by work accident and turnover
work_accident_summary <- employee_data %>%
  group_by(work_accident) %>%
  summarise(
    total_employees = n(),
    employees_left = sum(left == 1),
    employees_stayed = sum(left == 0),
    proportion_left = employees_left / total_employees,
    proportion_stayed = employees_stayed / total_employees
  )

# Print the summary table
print(work_accident_summary)



# Turnover Rate by Promotion in Last 5 Years
ggplot(employee_data, aes(x = promotion_last_5years, fill = left)) +
  geom_bar(position = "fill") +
  ggtitle("Turnover Rate by Promotion in Last 5 Years") +
  xlab("Promotion in Last 5 Years (0 = No, 1 = Yes)") +
  ylab("Proportion") +
  scale_fill_manual(values = c("steelblue", "steelblue4"), name = "Turnover") +
  theme_minimal()

# Summarize data by promotion status and turnover
promotion_summary <- employee_data %>%
  group_by(promotion_last_5years) %>%
  summarise(
    total_employees = n(),
    employees_left = sum(left == 1),
    employees_stayed = sum(left == 0),
    proportion_left = employees_left / total_employees
  )

# Print the summary table
print(promotion_summary)

#------------------------- Data Visualizations ---------------------------------------#

# 1. Distribution of Satisfaction Level
ggplot(employee_data, aes(x = satisfaction_level)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  ggtitle("Distribution of Satisfaction Level") +
  xlab("Satisfaction Level") +
  ylab("Count") +
  theme_minimal()

# 2. Average Monthly Hours Distribution
ggplot(employee_data, aes(x = average_montly_hours)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  ggtitle("Distribution of Average Monthly Hours") +
  xlab("Average Monthly Hours") +
  ylab("Count") +
  theme_minimal()

# 3. Boxplot of Satisfaction Level by Turnover
ggplot(employee_data, aes(x = left, y = satisfaction_level, fill = left)) +
  geom_boxplot() +
  ggtitle("Satisfaction Level by Turnover") +
  xlab("Turnover (0 = Stayed, 1 = Left)") +
  ylab("Satisfaction Level") +
  scale_fill_manual(values = c("steelblue", "steelblue4"), name = "Turnover") +
  theme_minimal()

# 4. Boxplot of Average Monthly Hours by Turnover
ggplot(employee_data, aes(x = left, y = average_montly_hours, fill = left)) +
  geom_boxplot() +
  ggtitle("Average Monthly Hours by Turnover") +
  xlab("Turnover (0 = Stayed, 1 = Left)") +
  ylab("Average Monthly Hours") +
  scale_fill_manual(values = c("steelblue", "steelblue4"), name = "Turnover") +
  theme_minimal()

# 5. Turnover Rate by Sales (Department)
employee_data %>%
  group_by(sales, left) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ggplot(aes(x = reorder(sales, -proportion), y = proportion, fill = left)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Turnover Rate by Department (Sales)") +
  xlab("Department") +
  ylab("Proportion") +
  scale_fill_manual(values = c("steelblue", "steelblue4"), name = "Turnover", labels = c("Stayed", "Left")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. Satisfaction vs. Average Monthly Hours (Scatterplot)
ggplot(employee_data, aes(x = satisfaction_level, y = average_montly_hours, color = left)) +
  geom_point(alpha = 0.6) +
  ggtitle("Satisfaction vs. Average Monthly Hours by Turnover") +
  xlab("Satisfaction Level") +
  ylab("Average Monthly Hours") +
  scale_color_manual(values = c("steelblue", "blue"), name = "Turnover") +
  theme_minimal()

# 7. Time Spent at Company Distribution
ggplot(employee_data, aes(x = time_spend_company)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  ggtitle("Distribution of Time Spent at Company") +
  xlab("Years at Company") +
  ylab("Count") +
  theme_minimal()

# 8. Time Spent at Company by Turnover (Bar Plot)
ggplot(employee_data, aes(x = as.factor(time_spend_company), fill = left)) +
  geom_bar(position = "fill") +
  ggtitle("Turnover Rate by Time Spent at Company") +
  xlab("Years at Company") +
  ylab("Proportion") +
  scale_fill_manual(values = c("steelblue", "steelblue4"), name = "Turnover") +
  theme_minimal()

  # 9. Turnover Rate by Last Evaluation Score
  ggplot(employee_data, aes(x = last_evaluation, fill = left)) +
    geom_density(alpha = 0.7) +
    ggtitle("Turnover Rate by Last Evaluation Score") +
    xlab("Last Evaluation Score") +
    ylab("Density") +
    scale_fill_manual(values = c("steelblue", "steelblue4"), name = "Turnover") +
    theme_minimal()


  
#------------------------- Correlation ---------------------------------------#
  
# Correlation Heatmap for Numerical Variables
# Select numerical variables for correlation analysis
# Compute the correlation matrix
numeric_data <- select(employee_data, satisfaction_level, last_evaluation, number_project, 
                       average_montly_hours, time_spend_company)

correlation_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")
correlation_matrix
melted_corr <- melt(correlation_matrix)

ggplot(data = melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "steelblue", high = "steelblue4", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  ggtitle("Correlation Heatmap") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)
  )



#------------------------- Data Transformation ---------------------------------------#

# Bin the average monthly hours to create ranges for analysis and remove the original column
employee_data <- employee_data %>%
  mutate(
    hours_category = case_when(
      average_montly_hours < 150 ~ "Low",
      average_montly_hours >= 150 & average_montly_hours <= 250 ~ "Moderate",
      average_montly_hours > 250 ~ "High"
    )
  ) %>%
  select(-average_montly_hours)  # Remove the original average monthly hours column

# Summarize data by average monthly hours category and turnover
hours_summary <- employee_data %>%
  group_by(hours_category) %>%
  summarise(
    total_employees = n(),
    employees_left = sum(left == 1),
    employees_stayed = sum(left == 0),
    proportion_left = employees_left / total_employees
  )

# Print the updated dataset and the summary table
print(employee_data)  # Updated dataset with hours_category column
print(hours_summary)  # Summary of turnover by hours category


# Ensure hours_category is sorted as Low, Moderate, High
hours_summary$hours_category <- factor(hours_summary$hours_category, levels = c("Low", "Moderate", "High"))

# Plot the proportion of employees who left by hours category
ggplot(hours_summary, aes(x = hours_category, y = proportion_left, fill = hours_category)) +
  geom_bar(stat = "identity", color = "black") +
  ggtitle("Proportion of Employees Who Left by Hours Category") +
  xlab("Hours Category") +
  ylab("Proportion of Employees Who Left") +
  scale_fill_manual(values = c("steelblue", "steelblue3", "steelblue4"), name = "Hours Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))
 

# Compute Weighted Difference for Satisfaction-to-Performance
employee_data <- employee_data %>%
  mutate(
    satisfaction_performance_score = 0.6 * satisfaction_level - 0.4 * last_evaluation
  )

# View the updated dataset
head(employee_data)

# Summarize the results by turnover (left)
satisfaction_performance_summary <- employee_data %>%
  group_by(left) %>%
  summarise(
    avg_satisfaction_performance = mean(satisfaction_performance_score, na.rm = TRUE),
    min_satisfaction_performance = min(satisfaction_performance_score, na.rm = TRUE),
    max_satisfaction_performance = max(satisfaction_performance_score, na.rm = TRUE)
  )

mean_scores <- employee_data %>%
  group_by(left) %>%
  summarise(avg_score = mean(satisfaction_performance_score, na.rm = TRUE))


ggplot(mean_scores, aes(x = left, y = avg_score, fill = left)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(avg_score, 2)), vjust = -0.5, size = 4) +
  ggtitle("Average Satisfaction-to-Performance Score by Turnover") +
  xlab("Turnover (0 = Stayed, 1 = Left)") +
  ylab("Average Satisfaction-to-Performance Score") +
  scale_fill_manual(values = c("steelblue", "steelblue4"), name = "Turnover", labels = c("Stayed", "Left")) +
  theme_minimal()


#------------------------- LASSO Implementation --------------------------------------#

# Convert factors to numeric where necessary
employee_data$left <- as.numeric(as.character(employee_data$left))  # Ensure target is numeric
employee_data$work_accident <- as.numeric(as.character(employee_data$work_accident))
employee_data$promotion_last_5years <- as.numeric(as.character(employee_data$promotion_last_5years))

# Remove non-numeric columns or encode them
employee_data$salary <- as.numeric(factor(employee_data$salary, levels = c("low", "medium", "high")))
employee_data$sales <- as.numeric(factor(employee_data$sales))

# Prepare predictor matrix (X) and target variable (Y)
X <- model.matrix(left ~ ., employee_data)[, -1]  # Remove intercept column
Y <- employee_data$left




# Fit LASSO model
lasso_model <- cv.glmnet(X, Y, alpha = 1, family = "binomial")  # alpha = 1 for LASSO

# Plot Cross-Validation Results
plot(lasso_model)

# Best Lambda Value
best_lambda <- lasso_model$lambda.min
print(paste("Best Lambda:", best_lambda))

# Coefficients of the LASSO Model
lasso_coefficients <- coef(lasso_model, s = best_lambda)
print(lasso_coefficients)



#------------------------- Modeling --------------------------------------------------#

# Step 1: Create a final dataset using the LASSO-selected predictors
final_data <- employee_data %>%
  select(left, satisfaction_level, satisfaction_performance_score, work_accident, 
         promotion_last_5years, salary, hours_category, time_spend_company, number_project)

# Convert `left` to a factor for classification tasks
final_data$left <- as.factor(final_data$left)

# Step 2: Partition the dataset into 70% training and 30% testing
set.seed(123)  # For reproducibility
partition <- createDataPartition(final_data$left, p = 0.7, list = FALSE)
train_data <- final_data[partition, ]
test_data <- final_data[-partition, ]


# Logistic Regression
logistic_model <- glm(left ~ ., data = train_data, family = "binomial")  # Fit the model
logistic_preds <- predict(logistic_model, test_data, type = "response")  # Predict on test data
logistic_class <- ifelse(logistic_preds > 0.5, 1, 0)  # Convert probabilities to classes
logistic_confusion <- confusionMatrix(as.factor(logistic_class), test_data$left)  # Evaluate

# Logistic Regression Metrics
cat("\nLogistic Regression Metrics:\n")
cat("Accuracy: ", logistic_confusion$overall["Accuracy"], "\n")
cat("Sensitivity (Recall): ", logistic_confusion$byClass["Sensitivity"], "\n")
cat("Specificity: ", logistic_confusion$byClass["Specificity"], "\n")

# Logistic Regression Plot (ROC Curve)
logistic_roc <- roc(as.numeric(test_data$left), logistic_preds)
plot(logistic_roc, col = "blue", main = "Logistic Regression ROC Curve")
auc_logistic <- auc(logistic_roc)
cat("Logistic Regression AUC: ", auc_logistic, "\n")


# Decision Tree
decision_tree <- rpart(left ~ ., data = train_data, method = "class")  # Fit the model
rpart.plot(decision_tree, main = "Decision Tree for Employee Turnover")  # Visualize
tree_preds <- predict(decision_tree, test_data, type = "class")  # Predict on test data
tree_confusion <- confusionMatrix(tree_preds, test_data$left)  # Evaluate

# Decision Tree Metrics
cat("\nDecision Tree Metrics:\n")
cat("Accuracy: ", tree_confusion$overall["Accuracy"], "\n")
cat("Sensitivity (Recall): ", tree_confusion$byClass["Sensitivity"], "\n")
cat("Specificity: ", tree_confusion$byClass["Specificity"], "\n")


# Random Forest
random_forest <- randomForest(left ~ ., data = train_data, ntree = 100, importance = TRUE)  # Fit the model
rf_preds <- predict(random_forest, test_data)  # Predict on test data
rf_confusion <- confusionMatrix(rf_preds, test_data$left)  # Evaluate

# Random Forest Metrics
cat("\nRandom Forest Metrics:\n")
cat("Accuracy: ", rf_confusion$overall["Accuracy"], "\n")
cat("Sensitivity (Recall): ", rf_confusion$byClass["Sensitivity"], "\n")
cat("Specificity: ", rf_confusion$byClass["Specificity"], "\n")

# Random Forest Feature Importance Plot
varImpPlot(random_forest, main = "Random Forest Feature Importance")



#------------------------- Clustering ------------------------------------------------#

# Step 1: Filter Data for Employees Who Stayed
stayed_data <- employee_data %>%
  filter(left == 0) %>%  # Filter only those who have stayed
  select(satisfaction_level, time_spend_company, number_project, satisfaction_performance_score)

# Standardize the data to ensure all variables are on the same scale
scaled_stayed_data <- scale(stayed_data)

# Step 2: Determine Optimal Number of Clusters

# Elbow Method
fviz_nbclust(scaled_stayed_data, kmeans, method = "wss") + 
  ggtitle("Elbow Method for Optimal Clusters (Stayed Employees)")


# Step 3: Perform K-Means Clustering
set.seed(123)  # For reproducibility
optimal_clusters <- 3  # Set the number of clusters based on the above methods
kmeans_model <- kmeans(scaled_stayed_data, centers = optimal_clusters, nstart = 25)

# Add cluster labels back to the original dataset for stayed employees
employee_data$Cluster <- NA  # Initialize column
employee_data$Cluster[employee_data$left == 0] <- as.factor(kmeans_model$cluster)

# Step 4: Analyze Segments for Stayed Employees
cluster_summary <- employee_data %>%
  filter(left == 0) %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Satisfaction = mean(satisfaction_level, na.rm = TRUE),
    Avg_Tenure = mean(time_spend_company, na.rm = TRUE),
    Avg_Projects = mean(number_project, na.rm = TRUE),
    Avg_Satisfaction_Performance = mean(satisfaction_performance_score, na.rm = TRUE)
  )

# Print the cluster summary
cat("Cluster Summary for Stayed Employees:\n")
print(cluster_summary)

# Step 5: Silhouette Plot for Stayed Employees
silhouette_info <- silhouette(kmeans_model$cluster, dist(scaled_stayed_data))

# Plot silhouette scores for each cluster
plot(
  silhouette_info,
  col = 1:optimal_clusters,
  border = NA,
  main = "Silhouette Plot for K-Means Clustering (Stayed Employees)"
)

# Step 6: PCA Plot with Cluster Centers
pca_result <- prcomp(scaled_stayed_data, center = TRUE, scale. = TRUE)

# Prepare data for PCA plot
pca_data <- as.data.frame(pca_result$x[, 1:2])  # Use the first two principal components
pca_data$Cluster <- as.factor(kmeans_model$cluster)  # Add cluster labels

# Map cluster centers to PCA space
cluster_centers <- as.data.frame(kmeans_model$centers)
cluster_centers_pca <- predict(pca_result, newdata = cluster_centers)

# PCA Plot with Cluster Centers
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_point(data = as.data.frame(cluster_centers_pca), aes(x = PC1, y = PC2), color = "black", size = 4, shape = 8) +
  ggtitle("PCA Plot with Cluster Centers (Stayed Employees)") +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  theme_minimal()

# Step 7: Cluster Center Plot
# Convert Cluster to a factor
cluster_summary$Cluster <- as.factor(cluster_summary$Cluster)

# Convert cluster summary to long format for better plotting
cluster_summary_long <- pivot_longer(cluster_summary, cols = -Cluster, names_to = "Feature", values_to = "Value")

# Plot cluster centers by feature
ggplot(cluster_summary_long, aes(x = Feature, y = Value, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  ggtitle("Cluster Centers by Feature (Stayed Employees)") +
  xlab("Feature") + ylab("Average Value") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

