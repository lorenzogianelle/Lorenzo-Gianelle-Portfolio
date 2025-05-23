# Bayesian Modelling Projects

This folder presents four applied Bayesian modelling projects, each addressing a real-world environmental or ecological problem. The emphasis is not only on technical implementation using mathematical programming tools, but also on interpreting the results to draw meaningful conclusions. The projects cover clustering, count regression, temporal analysis, and spatio-temporal modelling.

---

## South American Frogs: Acoustic Signal Clustering (JAGS)

**Objective**: Cluster frog calls based on Mel-Frequency Cepstral Coefficients (MFCCs) using bivariate Gaussian mixture models.

**Tools**: R, JAGS, rjags, coda  
**Skills**:
- Bayesian hierarchical modelling and mixture models
- Custom prior specification and label-switching constraints
- MCMC convergence diagnostics (trace plots, Gelman-Rubin, autocorrelation)
- Interpretation of posterior estimates and correlations

---

## Effect of Insecticides on Caterpillar Eggs (Stan)

**Objective**: Quantify the impact of sprayed and lead-based insecticides on caterpillar egg-laying rates using count regression.

**Tools**: R, Stan, rstan, coda, bayesplot  
**Skills**:
- Poisson and Negative Binomial regression with log-offsets
- Posterior predictive checks and comparison of treatment effects
- Effective communication of model findings

**Insights**: Both insecticides significantly reduced egg counts per unit area. Posterior inference revealed synergistic effects when both were used, with credible intervals supporting the statistical significance and practical importance of the reductions.

---

## Rainfall Trend Analysis in Edinburgh (INLA)

**Objective**: Model long-term rainfall patterns in Edinburgh using linear regression with environmental covariates.

**Tools**: R, INLA  
**Skills**:
- Bayesian linear modelling with Gaussian likelihood
- Prior tuning for fixed and precision effects
- Posterior diagnostics (WAIC, CPO, studentised residuals)
- Visualisation of posterior densities and residual patterns

**Insights**: Wind speed and year emerged as significant predictors of rainfall, while temperature did not. Interpretation of posterior distributions confirmed an increasing rainfall trend over time. Residual diagnostics supported model assumptions of homoscedasticity and independence.

---

## Spatio-Temporal Ozone Modelling in the UK (inlabru + SPDE)

**Objective**: Model spatial and temporal variability in ground-level ozone across England and Wales using SPDE random fields.

**Tools**: R, INLA, inlabru, fmesher, sf  
**Skills**:
- Spatio-temporal modelling with Gaussian random fields (SPDE)
- Mesh construction and PC priors
- Bayesian inference on spatial range and variability
- Decomposition of fixed and random effects over space and time

**Insights**: Ozone levels were found to vary spatially at a range of ~0.5 degrees and temporally over ~2 years. Fixed effects showed increasing ozone over time. Spatial variability was more pronounced than temporal, suggesting localized environmental influences dominate.

---

## Summary of Core Skills

- **Probabilistic Programming**: JAGS, Stan, INLA, inlabru
- **Bayesian Inference**: Multivariate models, count models, spatial fields
- **Diagnostics**: Gelman-Rubin, trace plots, posterior densities, WAIC
- **Interpretation**: Clear explanation of effect sizes, uncertainty, and credible intervals
- **Communication**: Data storytelling through statistical summaries and plots
