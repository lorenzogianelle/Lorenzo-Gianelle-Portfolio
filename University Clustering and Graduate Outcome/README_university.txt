# Clustering UK Universities by Academic Quality and Career Outcomes

## Project Overview
This project investigates whether UK universities can be grouped based on academic characteristics and whether these groupings explain differences in graduate career success. Using 2022 data from The Guardian and UCAS, we apply Bayesian clustering and regression techniques to uncover interpretable academic groupings and their implications.

## Objectives
- Cluster universities based on academic indicators (e.g., entry tariff, satisfaction, funding).
- Model post-graduation outcomes using Beta regression.
- Interpret how institutional characteristics relate to student career success.

## Key Challenges
- Small dataset: 118 universities with 7 academic features and 1 outcome variable.
- Overfitting risk in Bayesian models due to limited observations.
- Multicollinearity among variables addressed via PCA.

## Tools and Libraries
- R
- STAN (for Bayesian Beta regression)
- JAGS (for Gaussian Mixture Models)
- PCA for dimensionality reduction
- Custom diagnostics (ESS, DIC, posterior predictive checks)

## Skills Applied
- Bayesian hierarchical modeling
- Gaussian mixture models and latent class inference
- Beta regression for proportion outcomes
- Principal Component Analysis (PCA)
- Model comparison using DIC and convergence diagnostics
- Interpretation of posterior distributions and cluster stability

## Key Insights
- Two main clusters were identified, differentiated primarily by institutional selectivity and resources (PC1).
- Cluster 2, with higher PC1 scores, showed better, but more variable, graduate outcomes.
- The results suggest that academic quality and institutional resources significantly impact post-graduation success.
