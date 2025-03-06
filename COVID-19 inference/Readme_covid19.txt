
COVID-19 Fatal Incidence Inference

OVERVIEW

This project implements a simulation-based method to infer COVID-19 fatal incidence rates from daily death data recorded in English hospitals during the early stages of the pandemic. The method estimates infection-to-death durations using a log-normal distribution, generates initial guesses for infection times, and iteratively adjusts them to closely match the observed death data by minimizing a goodness-of-fit statistic. Bootstrapping is also applied to quantify the uncertainty in the inferred incidence rates.

METHODOLOGY

Data Handling: Reads COVID-19 death data (first 150 days) from English hospitals.

Infection-to-Death Duration Estimation: Uses a log-normal distribution to model the infection-to-death period.

Simulation of Infection Times:
Generates initial guesses for infection times by subtracting sampled durations from real death days.
Iteratively adjusts the values to optimize the fit to the real death distribution.

Goodness-of-Fit Optimization:
Uses a Pearson-like statistic to assess the fit between real and simulated death counts.
Proposes random shifts of infection times and accepts them if they improve the goodness-of-fit.

Bootstrapping for Uncertainty Analysis:
Replaces real death data with Poisson-simulated deaths to quantify uncertainty in the inferred infections.

Visualization:
Generates plots showing estimated new infections per day, simulated vs. real deaths, and uncertainty bounds.

Dependencies
The code is implemented in base R with no external package dependencies.



