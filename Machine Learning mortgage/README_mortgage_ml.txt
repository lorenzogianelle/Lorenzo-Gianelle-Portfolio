# Mortgage Default Prediction  
"The Risk Behind the Approval: Uncovering Default Before It Happens"

## Project Overview
This project aims to predict mortgage default risk using historical loan-level data from Freddie Mac. By combining explainable machine learning models with domain insights, we support early interventions and smarter credit decisions.

## Objectives
- Predict mortgage default risk for individual loans.
- Identify key risk drivers (e.g., FICO score, DTI, interest rate).
- Support stakeholder strategies for early warning and intervention.

## Key Challenges
- Extreme class imbalance: only ~0.6% of loans are defaulted.
- High-dimensional dataset: 30+ features and over 200,000 loan records.
- Balancing model interpretability and predictive performance.

## Tools and Libraries
- Python (pandas, numpy, matplotlib, seaborn)
- Scikit-learn (Logistic Regression, Random Forest, SVM)
- Imbalanced-learn (oversampling pipelines)
- GeoPandas and Folium (geospatial visualizations)

## Skills Applied
- Data cleaning and preprocessing (handling placeholders, feature encoding)
- Exploratory Data Analysis (correlation, outlier detection, distribution analysis)
- Feature engineering (domain-informed transformations and risk groupings)
- Model development and validation (classification, cross-validation, grid search)
- Evaluation metrics: AUC, F1-score, precision, recall, f-beta score
- Business insight extraction and actionable recommendations

## Key Insights
- FICO score, DTI, interest rate, and servicer identity are among the most predictive features.
- Random Forest improved recall and F-beta score over Logistic Regression.
- Geographic and institutional patterns in defaults highlight areas for risk-based intervention.