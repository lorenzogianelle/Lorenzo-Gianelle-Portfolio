BOXCAR RIDE SHARING SIMULATION

OVERWIEW
This project focuses on simulating and analyzing the performance of BoxCar, a ride-sharing service operating in Squareshire. The goal is to evaluate the efficiency of the system, particularly regarding driver availability, passenger wait times, and overall ride-matching fairness. The simulation is implemented using R and follows a discrete-event simulation approach, allowing detailed modeling of ride-sharing dynamics.

SKILLS USED

Simulation Modeling: Implementing a discrete-event simulation.

Statistical Analysis: Testing and validating probability distributions against real-world data.

Data Processing: Managing and analyzing datasets related to drivers and passengers.

R Programming: Implementing the entire simulation in base R with structured code.

FILES INCLUDED

Project_1_simulation.pdf – Full report detailing methodology, implementation, testing, and results.

BoxCar.R – Initial implementation of the simulation using BoxCar’s provided data.

BoxCar_new_d.R – Improved version incorporating tested probability distributions.

BoxCar_improve.R – Further refined simulation with enhanced driver-passenger matching.

Distribution Analysis.Rmd – Statistical analysis of BoxCar’s data to validate assumptions.

drivers.xlsx – Real-world driver data from BoxCar.

riders.xlsx – Real-world rider data from BoxCar.

IMPLEMENTATION SUMMARY

The simulation operates as a discrete-event system, meaning it processes events sequentially to model real-world operations. It uses probability distributions to simulate:

-Driver & Passenger Arrivals (modeled with exponential distributions, later adjusted based on data analysis).

-Matching Mechanisms (assigning drivers to passengers using distance-based and fairness-based approaches).

-Ride Completion & System Exit (tracking income, working hours, and waiting times).

