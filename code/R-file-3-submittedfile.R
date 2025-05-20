#setting working directory

#setwd("C:\\Users\\subas\\OneDrive\\Desktop\\Practicum\\Project 1")

#install.packages("gbm")
#install.packages("randomForest")
#install.packages("ggcorrplot")

#-------------------------Importing All The Libraries------------------------------------#
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)
library(reshape2)
library(caret)
library(FactoMineR)
library(caret)
library(gbm)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(ggcorrplot)
library(corrplot)
library(car)

#-------------------------Importing Dataset -------------------------------------#
# Load the dataset
employee_data <- read.csv("../data/Employee.csv")

names(employee_data)

#-------------------------Converting Factors------------------------------------#
# Convert binary variables to factors
employee_data$Work_accident <- as.factor(employee_data$Work_accident)
employee_data$left <- as.factor(employee_data$left)
employee_data$promotion_last_5years <- as.factor(employee_data$promotion_last_5years)

colnames(employee_data)[which(names(employee_data) == "Work_accident")] <- "work_accident"


# EDA Start

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

#-------------------histogram of distribution of satisfaction level----------------#
# Histogram for Satisfaction Level
ggplot(employee_data, aes(x = satisfaction_level)) + 
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") + 
  theme_minimal() + 
  ggtitle("Distribution of Satisfaction Level") +
  xlab("Satisfaction Level") +
  ylab("Frequency")


#-----------------------Correlation plot----------------------#

# Select numerical variables for correlation analysis
numeric_data <- select(employee_data, satisfaction_level, last_evaluation, number_project, 
                       average_montly_hours, time_spend_company)

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")

# Print the correlation matrix
print("Correlation Matrix:")
print(cor_matrix)

# Visualize the correlation matrix using corrplot
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix of Numerical Variables", mar = c(0,0,1,0))

#-----------------------comparison plots-----------------------------#

#----------------Hypothesis 1 -------------------------------#
#reordering the salary levels in the dataset
employee_data$salary <- factor(employee_data$salary, levels = c("low", "medium", "high"))

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

# Bar Plot for Salary vs. Turnover
ggplot(employee_data, aes(x = salary, fill = left)) + 
  geom_bar(position = "fill") + 
  theme_minimal() + 
  ggtitle("Salary Level vs. Employee Turnover") +
  xlab("Salary Level") +
  ylab("Proportion of Employees") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Stayed", "Left"))



#----------------Hypothesis 2 -------------------------------#
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

# Bar Plot for Work Accident vs. Turnover
ggplot(employee_data, aes(x = work_accident, fill = left)) + 
  geom_bar(position = "fill") + 
  theme_minimal() + 
  ggtitle("Work Safety vs. Employee Turnover") +
  xlab("Work Accident (0 = No, 1 = Yes)") +
  ylab("Proportion of Employees") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Stayed", "Left"))


#----------------Hypothesis 3 -------------------------------#
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

# Bar Plot for Promotion Status vs. Turnover
ggplot(employee_data, aes(x = promotion_last_5years, fill = left)) + 
  geom_bar(position = "fill") + 
  theme_minimal() + 
  ggtitle("Promotion Status vs. Employee Turnover") +
  xlab("Promotion in Last 5 Years (0 = No, 1 = Yes)") +
  ylab("Proportion of Employees") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Stayed", "Left"))




#--------------------insights for initial exploration-----------------------#
# Bar Plot for Department vs. Turnover
ggplot(employee_data, aes(x = sales, fill = left)) + 
  geom_bar(position = "fill") + 
  theme_minimal() + 
  ggtitle("Department vs. Employee Turnover") +
  xlab("Department") +
  ylab("Proportion of Employees") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Stayed", "Left")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box Plot for Number of Projects vs. Turnover
ggplot(employee_data, aes(x = left, y = number_project, fill = left)) + 
  geom_boxplot() + 
  theme_minimal() + 
  ggtitle("Number of Projects vs. Employee Turnover") +
  xlab("Turnover (0 = Stayed, 1 = Left)") +
  ylab("Number of Projects") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Stayed", "Left")) +
  theme(legend.position = "none")


# Box Plot for Last Evaluation Score vs. Turnover
ggplot(employee_data, aes(x = left, y = last_evaluation, fill = left)) + 
  geom_boxplot() + 
  theme_minimal() + 
  ggtitle("Last Evaluation Score vs. Employee Turnover") +
  xlab("Turnover (0 = Stayed, 1 = Left)") +
  ylab("Last Evaluation Score") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Stayed", "Left")) +
  theme(legend.position = "none")

# Density Plot for Average Monthly Hours vs. Turnover
ggplot(employee_data, aes(x = average_montly_hours, fill = left)) + 
  geom_density(alpha = 0.5) + 
  theme_minimal() + 
  ggtitle("Average Monthly Hours vs. Employee Turnover") +
  xlab("Average Monthly Hours") +
  ylab("Density") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Stayed", "Left")) +
  theme(legend.position = "right")

# Bin the average monthly hours to create ranges for analysis
employee_data <- employee_data %>%
  mutate(
    hours_category = case_when(
      average_montly_hours < 150 ~ "Low (<150)",
      average_montly_hours >= 150 & average_montly_hours <= 250 ~ "Moderate (150-250)",
      average_montly_hours > 250 ~ "High (>250)"
    )
  )

# Summarize data by average monthly hours category and turnover
hours_summary <- employee_data %>%
  group_by(hours_category) %>%
  summarise(
    total_employees = n(),
    employees_left = sum(left == 1),
    employees_stayed = sum(left == 0),
    proportion_left = employees_left / total_employees
  )

# Print the summary table
print(hours_summary)

  #------------------- Data Transformation ------------------------#


# Principal Component Analysis (PCA)
# Selecting only numerical variables for PCA
numerical_vars <- employee_data[, sapply(employee_data, is.numeric)]
pca_result <- PCA(numerical_vars, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Display PCA results
print(pca_result$eig)
print(pca_result$var$coord)


#-------------transformation--------------#
# Data Transformation
# Feature Engineering
# 1. Workload Balance Score
employee_data$workload_balance <- employee_data$number_project * employee_data$average_montly_hours

# 2. Satisfaction-Evaluation Interaction
employee_data$satisfaction_evaluation_interaction <- employee_data$satisfaction_level * employee_data$last_evaluation

# Encoding Categorical Variables
# Ordinal Encoding for Salary
employee_data$salary <- as.numeric(factor(employee_data$salary, levels = c("low", "medium", "high"), ordered = TRUE))

#-------------correlation between new derived variable with rest------------#
# Ensure that the variables are numeric
#employee_data$workload_balance <- as.numeric(employee_data$workload_balance)
#employee_data$satisfaction_evaluation_interaction <- as.numeric(employee_data$satisfaction_evaluation_interaction)
#employee_data$left <- as.numeric(employee_data$left)

# Now calculate the correlation matrix
#correlation_matrix <- cor(employee_data[, c("workload_balance", "satisfaction_evaluation_interaction", "left")], use = "complete.obs")

# Print the correlation matrix
#print(correlation_matrix)
#plot(correlation_matrix)

#---------------------partitioning -------------------------#

employee_data$hours_category <- as.factor(employee_data$hours_category)
employee_data$sales <- as.factor(employee_data$sales)
employee_data$work_accident <- as.factor(employee_data$work_accident)
employee_data$promotion_last_5years <- as.factor(employee_data$promotion_last_5years)

# Retain only the necessary features
employee_data_refined <- employee_data[, c('satisfaction_level', 
                                           'last_evaluation', 
                                           'number_project', 
                                           'average_montly_hours', 
                                           'time_spend_company', 
                                           'work_accident', 
                                           'promotion_last_5years', 
                                           'salary', 
                                           'workload_balance', 
                                           'satisfaction_evaluation_interaction', 
                                           'left')]

# Convert 'left' to numeric for modeling purposes
employee_data_refined$left <- as.numeric(as.character(employee_data_refined$left))

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # For reproducibility
partition_index <- createDataPartition(employee_data$left, p = 0.7, list = FALSE)

# Create training and testing datasets
train_employee_data <- employee_data[partition_index, ]
test_employee_data <- employee_data[-partition_index, ]


# Check the dimensions of the split employee_datasets
print(dim(train_employee_data))
print(dim(test_employee_data))

names(train_employee_data)

#---------------------------- Model Building 1 ------------------------#

#-------------we are building the model with this original column left ~ satisfaction_level + last_evaluation + workload_balance + 
#time_spend_company + work_accident + promotion_last_5years + 
#  sales + salary  just to see how our model behaves -----------------------#

# Logistic Regression Model
logistic_model <- glm(left ~ satisfaction_level + last_evaluation  + 
                        time_spend_company + work_accident + promotion_last_5years + 
                        sales + salary, 
                      data = train_employee_data, family = binomial)
summary(logistic_model)

# Decision Tree Model
tree_model <- rpart(left ~ satisfaction_level + last_evaluation  + 
                      time_spend_company + work_accident + promotion_last_5years + 
                      sales + salary, 
                    data = train_employee_data, method = "class")
rpart.plot(tree_model)

# Random Forest Model
rf_model <- randomForest(left ~ satisfaction_level + last_evaluation + 
                           time_spend_company + work_accident + promotion_last_5years + 
                           sales + salary, 
                         data = train_employee_data, ntree = 500, mtry = 3, importance = TRUE)
print(rf_model)

# Ensure the target variable 'left' is in numeric format (0 or 1)
train_employee_data$left <- as.numeric(as.character(train_employee_data$left))
test_employee_data$left <- as.numeric(as.character(test_employee_data$left))

# Gradient Boosting Machine (GBM) Model
gbm_model <- gbm(left ~ satisfaction_level + last_evaluation + 
                   time_spend_company + work_accident + promotion_last_5years + 
                   sales + salary, 
                 data = train_employee_data, 
                 distribution = "bernoulli", 
                 n.trees = 500, 
                 interaction.depth = 3, 
                 shrinkage = 0.01, 
                 cv.folds = 5)
# Display the summary of the GBM model
summary(gbm_model)


#--------------------------- Model Evaluation --------------------#

# Logistic Regression Evaluation
logistic_pred <- predict(logistic_model, test_employee_data, type = "response")
logistic_pred_class <- ifelse(logistic_pred > 0.5, 1, 0)
accuracy_logistic <- mean(logistic_pred_class == test_employee_data$left)
print(paste("Accuracy for Logistic Regression: ", round(accuracy_logistic, 4)))

# Decision Tree Evaluation
tree_pred <- predict(tree_model, test_employee_data, type = "class")
accuracy_tree <- mean(tree_pred == test_employee_data$left)
print(paste("Accuracy for Decision Tree: ", round(accuracy_tree, 4)))

# Random Forest Evaluation
rf_pred_class <- predict(rf_model, test_employee_data, type = "prob")[,2]
rf_pred_class <- ifelse(rf_pred_class > 0.5, 1, 0)
accuracy_rf <- mean(rf_pred_class == test_employee_data$left)
print(paste("Accuracy for Random Forest: ", round(accuracy_rf, 4)))

# Gradient Boosting Machine (GBM) Evaluation
gbm_pred <- predict(gbm_model, test_employee_data, n.trees = 500, type = "response")
gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)
accuracy_gbm <- mean(gbm_pred_class == test_employee_data$left)
print(paste("Accuracy for GBM: ", round(accuracy_gbm, 4)))


# Function to calculate precision, recall, and F1-score
calculate_metrics <- function(conf_matrix) {
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  return(list(accuracy = accuracy, precision = precision, recall = recall, f1_score = f1_score))
}

#------------------- Model 1 Evaluation (Original Variables) -------------------#

# Logistic Regression Evaluation
logistic_pred_class <- ifelse(logistic_pred > 0.5, 1, 0)
conf_matrix_logistic <- table(test_employee_data$left, logistic_pred_class)
logistic_metrics <- calculate_metrics(conf_matrix_logistic)
print("Logistic Regression (Model 1) Metrics:")
print(logistic_metrics)

# Decision Tree Evaluation
conf_matrix_tree <- table(test_employee_data$left, tree_pred)
tree_metrics <- calculate_metrics(conf_matrix_tree)
print("Decision Tree (Model 1) Metrics:")
print(tree_metrics)

# Random Forest Evaluation
rf_pred_class <- ifelse(rf_pred_class > 0.5, 1, 0)
conf_matrix_rf <- table(test_employee_data$left, rf_pred_class)
rf_metrics <- calculate_metrics(conf_matrix_rf)
print("Random Forest (Model 1) Metrics:")
print(rf_metrics)

# Gradient Boosting Machine (GBM) Evaluation
gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)
conf_matrix_gbm <- table(test_employee_data$left, gbm_pred_class)
gbm_metrics <- calculate_metrics(conf_matrix_gbm)
print("GBM (Model 1) Metrics:")
print(gbm_metrics)

#--------------------------- Plot ROC Curves for All Models --------------------#

roc_logistic <- roc(test_employee_data$left, logistic_pred, levels = c(0, 1), direction = "<")
roc_tree <- roc(test_employee_data$left, as.numeric(tree_pred), levels = c(0, 1), direction = "<")
roc_rf <- roc(test_employee_data$left, rf_pred_class, levels = c(0, 1), direction = "<")
roc_gbm <- roc(test_employee_data$left, gbm_pred, levels = c(0, 1), direction = "<")


# Plot ROC curves for all models
plot(roc_logistic, col = "blue", main = "ROC Curves for All Models", lwd = 2)
lines(roc_tree, col = "green", lwd = 2)
lines(roc_rf, col = "red", lwd = 2)
lines(roc_gbm, col = "purple", lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Decision Tree", "Random Forest", "GBM"), 
       col = c("blue", "green", "red", "purple"), lty = 1, lwd = 2, cex = 0.6)



#---------------------------- Model Building 2 ------------------------#

# Building the model using derived variables: workload_balance, satisfaction_evaluation_interaction

# Logistic Regression Model with derived variables
logistic_model <- glm(left ~ workload_balance + work_accident + promotion_last_5years + 
                        sales + salary + satisfaction_evaluation_interaction, 
                      data = train_employee_data, family = binomial)
summary(logistic_model)

# Decision Tree Model with derived variables
tree_model <- rpart(left ~ workload_balance + work_accident + promotion_last_5years + 
                      sales + salary + satisfaction_evaluation_interaction, 
                    data = train_employee_data, method = "class")
rpart.plot(tree_model)

# Ensure that 'left' is treated as a factor for classification
train_employee_data$left <- as.factor(train_employee_data$left)
test_employee_data$left <- as.factor(test_employee_data$left)

# Random Forest Model with derived variables
rf_model <- randomForest(left ~ workload_balance + work_accident + promotion_last_5years + 
                           sales + salary + satisfaction_evaluation_interaction, 
                         data = train_employee_data, ntree = 500, mtry = 3, importance = TRUE)
print(rf_model)

# Ensure the target variable 'left' is in numeric format (0 or 1)
train_employee_data$left <- as.numeric(as.character(train_employee_data$left))
test_employee_data$left <- as.numeric(as.character(test_employee_data$left))

# Gradient Boosting Machine (GBM) Model with derived variables
gbm_model <- gbm(left ~ workload_balance + work_accident + promotion_last_5years + 
                   sales + salary + satisfaction_evaluation_interaction, 
                 data = train_employee_data, 
                 distribution = "bernoulli", 
                 n.trees = 500, 
                 interaction.depth = 3, 
                 shrinkage = 0.01, 
                 cv.folds = 5)
# Display the summary of the GBM model
summary(gbm_model)



#--------------------------- Checking Multicollinearity ---------------------------#

# Calculate VIF for the logistic regression model
vif(logistic_model)

#--------------------------- Model Evaluation 2---------------------------#

# Logistic Regression Evaluation
logistic_pred <- predict(logistic_model, test_employee_data, type = "response")
logistic_pred_class <- ifelse(logistic_pred > 0.5, 1, 0)
accuracy_logistic <- mean(logistic_pred_class == test_employee_data$left)
print(paste("Accuracy for Logistic Regression: ", round(accuracy_logistic, 4)))

# Decision Tree Evaluation
tree_pred <- predict(tree_model, test_employee_data, type = "class")
accuracy_tree <- mean(tree_pred == test_employee_data$left)
print(paste("Accuracy for Decision Tree: ", round(accuracy_tree, 4)))

# Random Forest Evaluation
rf_pred_class <- predict(rf_model, test_employee_data, type = "prob")[,2]
rf_pred_class <- ifelse(rf_pred_class > 0.5, 1, 0)
accuracy_rf <- mean(rf_pred_class == test_employee_data$left)
print(paste("Accuracy for Random Forest: ", round(accuracy_rf, 4)))

# Gradient Boosting Machine (GBM) Evaluation
gbm_pred <- predict(gbm_model, test_employee_data, n.trees = 500, type = "response")
gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)
accuracy_gbm <- mean(gbm_pred_class == test_employee_data$left)
print(paste("Accuracy for GBM: ", round(accuracy_gbm, 4)))

# Logistic Regression Evaluation (Model 2)
logistic_pred_class <- ifelse(logistic_pred > 0.5, 1, 0)
conf_matrix_logistic_2 <- table(test_employee_data$left, logistic_pred_class)
logistic_metrics_2 <- calculate_metrics(conf_matrix_logistic_2)
print("Logistic Regression (Model 2) Metrics:")
print(logistic_metrics_2)

# Decision Tree Evaluation (Model 2)
conf_matrix_tree_2 <- table(test_employee_data$left, tree_pred)
tree_metrics_2 <- calculate_metrics(conf_matrix_tree_2)
print("Decision Tree (Model 2) Metrics:")
print(tree_metrics_2)

# Random Forest Evaluation (Model 2)
rf_pred_class <- ifelse(rf_pred_class > 0.5, 1, 0)
conf_matrix_rf_2 <- table(test_employee_data$left, rf_pred_class)
rf_metrics_2 <- calculate_metrics(conf_matrix_rf_2)
print("Random Forest (Model 2) Metrics:")
print(rf_metrics_2)

# Gradient Boosting Machine (GBM) Evaluation (Model 2)
gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)
conf_matrix_gbm_2 <- table(test_employee_data$left, gbm_pred_class)
gbm_metrics_2 <- calculate_metrics(conf_matrix_gbm_2)
print("GBM (Model 2) Metrics:")
print(gbm_metrics_2)

#--------------------------- Plot ROC Curves for All Models -----------------------#

roc_logistic <- roc(test_employee_data$left, logistic_pred, levels = c(0, 1), direction = "<")
roc_tree <- roc(test_employee_data$left, as.numeric(tree_pred), levels = c(0, 1), direction = "<")
roc_rf <- roc(test_employee_data$left, rf_pred_class, levels = c(0, 1), direction = "<")
roc_gbm <- roc(test_employee_data$left, gbm_pred, levels = c(0, 1), direction = "<")

# Plot ROC curves for all models
plot(roc_logistic, col = "blue", main = "ROC Curves for All Models (With Derived Variables)", lwd = 2)
lines(roc_tree, col = "green", lwd = 2)
lines(roc_rf, col = "red", lwd = 2)
lines(roc_gbm, col = "purple", lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Decision Tree", "Random Forest", "GBM"), 
       col = c("blue", "green", "red", "purple"), lty = 1, lwd = 2, cex = 0.6)


