# Human Capital Analytics – Employee Turnover Prediction

This project applies advanced analytics and machine learning techniques to identify the key drivers of employee turnover and develop targeted retention strategies.

## 📁 Project Structure

```
Human-Capital-Analytics/
├── code/
│   ├── R-file-3-submittedfile.R
│   └── R-file-3.R
├── CSDA 6010 - Project 1 - Presentation - SubashYadav-1.pptx
├── CSDA 6010 Project 1 Final- Subash Yadav-1.docx
├── CSDA 6010 Project 1 Final- Subash Yadav-1.pdf
├── data/
│   └── Employee.csv
├── Human Capital Analytics For Students.docx

```

## 🎯 Business Goals

- Identify key drivers of employee turnover such as job dissatisfaction, limited promotion, and high workload.
- Predict turnover risk using data-driven models.
- Segment employees into actionable groups for targeted retention efforts.

## 📊 Analytical Approaches

- Exploratory Data Analysis (EDA)
- Hypothesis Testing (Salary, Safety, Promotion)
- Feature Engineering:
  - Satisfaction-to-Performance Score
  - Binned Workload Categories
- Predictive Modeling:
  - Logistic Regression (AUC = 0.84)
  - Decision Tree (Accuracy = 96.33%)
  - Random Forest (Accuracy = 97.71%)
- Clustering using K-means (3 Segments)

## 📈 Key Insights

- **Satisfaction Level** is the most critical predictor of turnover.
- Employees with **low satisfaction and high tenure** are most likely to leave.
- **Workload imbalance** and lack of **promotions** also drive attrition.
- Clustering revealed:
  - Cluster 1: High satisfaction, low risk
  - Cluster 2: Moderate satisfaction, medium risk
  - Cluster 3: Low satisfaction, high tenure, high risk

## 🛠 Tech Stack

- R (caret, glmnet, cluster)
- CSV Dataset: `Employee.csv`
- Visualization Tools: ggplot2, base R plots

## 📎 Deliverables

- Full analytical report with visuals (PDF)
- Cleaned code scripts
- Derived features for improved interpretability
- Final model and clustering insights

## 📬 Author

**Subash Yadav**   
[LinkedIn](https://www.linkedin.com/in/mathachew7)  
[GitHub](https://github.com/mathachew7)  
