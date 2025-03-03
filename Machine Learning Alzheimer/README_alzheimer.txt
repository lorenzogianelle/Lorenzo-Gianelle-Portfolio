PREDICTING ALZHEIMER'S PROGRESSION WIH MACHINE LEARNING MODELS

OVERVIEW

This project applies machine learning techniques to predict 24-month cognitive decline in individuals diagnosed with Mild Cognitive Impairment (MCI) and Alzheimer’s Disease (AD). The goal is to build a predictive model using clinical, genetic, and neuroimaging data to estimate cognitive performance over time.

Due to confidentiality agreements, the dataset cannot be shared. However, the final report (ML1.pdf) includes a detailed analysis, methodology, and results of the study.

PROJECT SCOPE

Develop a predictive model for cognitive decline based on ADAS-Cog 13 scores, APOE4 genotype, and brain volumetrics.
Evaluate multiple regression models to determine the best-performing one.
Identify key predictors influencing Alzheimer’s progression.
Provide clinically useful insights to support early intervention and patient monitoring.

MODELS CONSIDERED
The following machine learning models were evaluated:

-Baseline Linear Regression (selected for its simplicity and interpretability)
-Lasso Regression (reported in the final analysis as the best-performing model)
-Ridge Regression (tested but not included in the final code)
-Elastic Net (tested but not included in the final code)
-Polynomial Regression (excluded due to overfitting)

Note: The provided code only includes Lasso Regression, as it was identified as the best model for prediction accuracy. Other models were tested but not retained in the final implementation, in the final conclusion results are discussed from all the different methods.

LIBRARIES USED

The analysis was conducted using Python with the following key libraries:

-pandas – Data manipulation and preprocessing
-numpy – Numerical computations
-scikit-learn – Machine learning model implementation
-matplotlib / seaborn – Data visualization
-statsmodels – Statistical analysis
-scipy – Statistical functions

FILE DESCRIPTION

ML1.pdf – The final PDF report containing the full machine learning workflow, including data preprocessing, model selection, results, and interpretation.

Note: Group work completed at the University of Edinburgh. All tasks were understood and shared, ensuring that all applied skills were learned throughout the project.