# This code implements a simulation-based method for inferring Covid fatal incidence rates 
# from daily death data in English hospitals during the early stages of the pandemic. 
# The code first estimates the infection-to-death durations using a log-normal distribution.
# It then generates initial guesses for infection times and iteratively adjusts them to 
# match the real death data as closely as possible, minimizing a goodness-of-fit (P) statistic.
# The code can also apply bootstrapping to quantify uncertainty in the inferred fatal incidence rates.
# Finally, it produces visualizations showing the inferred number of infections per day, 
# the real vs simulated deaths, and the uncertainty in the infection estimates.


rm(list = ls())


# Read in the Covid data for English hospitals
# 'julian' is the day of the year, 'nhs' is the number of deaths.


data <- read.table('engcov.txt')
data <- data[1:150, c('julian', 'nhs')]


# Main 'deconv()' function to estimate infection times using simulation 
# Inputs:
#   - t: vector of days on which deaths occurred
#   - deaths: vector of death counts corresponding to each day
#   - n.rep: number of iterations for the simulation (default: 100)
#   - bs: flag to indicate whether bootstrapping should be applied (default: FALSE)
#   - t0: optional vector of infection times (default: NULL, in which case it's generated 
#         within the function)
# Outputs:
#   - A list containing:
#     - P: vector of goodness-of-fit (P) values across iterations
#     - inft: matrix of inferred infections per day across iterations
#     - t0: final infection times after simulation
#     - sim_deaths: simulated death counts based on the final infection times



deconv <- function (t,deaths,n.rep=100,bs=FALSE,t0=NULL){
  
  stuff=list() #List to store results
  P_hist<-numeric(n.rep) #Vector to store P statistic for each iteration
  t0_hist<-matrix(0,310,n.rep) #Matrix storing inferred infections per day across iterations
  
  #Setting up distribution for infection-death duration, assumed to be log-normal
  durations = c(1:80)
  
  infection_to_death_dist <- dlnorm(durations, meanlog = 3.152, sdlog = 0.451)
  
  density_sum <- sum(infection_to_death_dist)
  
  #normalizing to obtain a probability
  infection_to_death_dist_norm <- infection_to_death_dist / density_sum  
  
  probabilities <- infection_to_death_dist_norm #Vector from which we will sample durations
  
  
  # Generate initial guesses for infection times (t0) if not provided
  
  individual_death_days <- rep(t,deaths)  #Vector of death days for each individual
  
  n_ind<-length(individual_death_days)
  
  # Generate t0 only if it is NULL in the call of the function
  
  if(is.null(t0)){
    
    #Generate infection times by subtracting sampled durations from real death days
  
    t0 <- individual_death_days - sample(1:80, n_ind, prob = probabilities, replace = TRUE) 
    
    t0[t0 < 1] = 1  #setting to one days of infection in 2019
    
    t0[t0 >310 ] = 310 #in order to not exceed the limit given
  }
  
  # Tabulate real death counts per day (in order to compute P later) and
  # append 0s at beginning and end for days where no deaths occurred to ensure 
  # a vector length of 310 (avoids out of range indexing)
  
  table0 <- c(rep(0,61), deaths)
  table0 <- c(table0,rep(0,310-length(table0))) 
    
  mean_boot <- table0 #to use later for Poisson simulation in bootstrap

  
 
  #Simulation and fitting loop (for n.rep iterations)
  
  for(i in 1:n.rep){
    
    # Simulate death days by adding sampled infection-to-death durations to infection times (t0)
    
    infection_to_death_new_draws <- sample(1:80, n_ind, prob = probabilities, replace = TRUE)
    
    sampled_death_days <- t0 + infection_to_death_new_draws
    
    if(bs == TRUE){
      
      # If bootstrapping, replace real death counts with Poisson-simulated data
      
      table0 <- rpois(length(table0), lambda = mean_boot)
    
    }
    
    # Tabulate simulated death counts based on sampled death days

    table1 <- tabulate(sampled_death_days)
    table1<-c(table1,rep(0,310-length(table1)))
    
    # Calculate initial P statistic 
    p_number <- sum((table0 - table1)^2/pmax(1,table1)) 
    
    P_hist[i]<-p_number #Store P for this iteration
    
    
    # Define vector for shifts and adjust magnitude base on iteration stage
    
    if(i <= 50 ){
      
      shift <- c(-8, -4, -2, -1, 1, 2, 4, 8)
    
    }else if( i<=75 ){
      
      shift <- c(-4, -2, -1, 1, 2, 4)  
      
    }else{
      
      shift <- c(-2, -1, 1, 2)
      
    }
    
    
    random_order <- sample(1:n_ind,n_ind)  #randomly order iteration for next loop
  
    random_shift <- sample(shift, n_ind, replace = TRUE) #sample the shifts for our simulation
    
    
    # Iterate through the random arrangement of infection times and propose
    # the corresponding random shift
    
    for (j in 1:length(random_order)){
      
      index_random <- random_order[j]

      #In each iteration we find the old date of death and using random shift
      #we find the new one
      
      old_day <- sampled_death_days[index_random] 
      
      new_day <- old_day + random_shift[j]
      
      # Constrain new_day between 1 and 310
      
      if(new_day < 1){
        
        new_day=1
        
      }
      
      if(new_day > 310){
        
        new_day=310
        
      }
      
      # Manually change table1 to avoid tabulating again
      
      table1[old_day]<-table1[old_day]-1
      
      table1[new_day]<-table1[new_day]+1
      
      #Compute the new p

      p_new <- sum((table0 - table1)^2/pmax(1,table1)) 
      
      # If P is lower with the shift, update t0 and sampled_death_days.
      
      if (p_new < p_number){
        
        p_number <- p_new 
        
        t0[index_random] <- t0[index_random] + random_shift[j]
        sampled_death_days[index_random] <- new_day
        
      } else { 
        
        # Otherwise, discard the change made in table1 and continue
        
        table1[old_day]<-table1[old_day]+1
        
        table1[new_day]<-table1[new_day]-1
      }
      
    }
    
    # Store infection counts for this iteration
    
    inf_per_day_post_update<-tabulate(t0,nbins=310)
    
    t0_hist[,i]<-inf_per_day_post_update
    
    
    
    #Plotting every 10th iteration and at the final one (useful if n.rep is not multiple of 10)
    
    if( i%%10 == 0 || i == n.rep  ){
    
      par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  
      # Plot P history
      plot(1:length(P_hist), P_hist, xlab = "Step", ylab = "P History", main = "P History", type = "l")
  
      # Plot estimated new infections
      plot(1:310, inf_per_day_post_update, type = "l", col = "green", lwd = 2,
           xlab = "Day of Year", ylab = "Estimated Infections", main = "Estimated New Infections per Day",
           ylim = range(0, max(inf_per_day_post_update, na.rm = TRUE)))
  
      # Combined plot of simulated deaths and real deaths
      max_y <- max(c(table0, table1), na.rm = TRUE) 
      plot(1:310, table1, type = "l", col = "blue", lwd = 2,
           xlab = "Day of Year", ylab = "Counts", main = "Simulated vs Real Deaths",
           ylim = c(0, max_y)) # Use consistent y-axis limit
      lines(1:310, table0, col = "red", lwd = 2) 
  
      # Create a blank plot for the legend
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "Deaths", main = "Legend")
      legend("center", legend = c("Simulated Deaths", "Real Deaths"),
             col = c("blue", "red"), lwd = 2, cex = 1.2)
      

    }
  }
    
    # Store history of P, history of t0, final fitted t0 (inferred infections) 
    # and final table1 (simulated death days) in return list
    
    stuff[[1]]=P_hist
    stuff[[2]]=t0_hist
    stuff[[3]]=t0
    stuff[[4]]=table1
    
    names(stuff)=c("P","inft","t0","sim_deaths")
    
    return(stuff)
}
  

# Call function with no bootsrapping

my_list<-deconv(t=data$julian,deaths=data$nhs,bs=FALSE)

# Call function with bootstrapping, passing in fitted t0 from first run

bootstrap_list <-deconv(t=data$julian,deaths=data$nhs,bs=TRUE, t0=my_list$t0)


# Calculate max and min inferred infections for each day over all iterations in 
# the bootsrapping step to use as measure of uncertainty

max_e <- apply(bootstrap_list$inft,1,max)
min_e <- apply(bootstrap_list$inft,1,min)



dev.off() # Clear plot area


# Plot 1: Simulated vs Real Deaths
table0_plot <- c(rep(0, 61), data$nhs)
table0_plot <- c(table0_plot, rep(0, 310 - length(table0_plot)))

par(mar = c(4, 4, 2, 1))  # Set margins for the second plot
plot(1:310, my_list$sim_deaths, type = "l", col = "blue", lwd = 2,
     xlab = "Day of Year", ylab = "Deaths", main = "Simulated vs Real Deaths")
lines(1:310, table0_plot, col = "red", lwd = 2)
legend("topright", legend = c("Simulated Deaths", "Real Deaths"),
       col = c("blue", "red"), lwd = 2, cex = 0.8)

# Plot 2: Uncertainty Plot

#plot the uncertainty area as the Min and Max Inferred Infections from Bootstrapping
#and plot the converged day of infections

plot(1:310, type="n", ylim=c(min(min_e), max(max_e)), 
     xlab="Day", ylab="Number of infections", main="Uncertainty")
polygon(c(1:310, rev(1:310)), c(max_e, rev(min_e)), col="grey")
lines(1:310, tabulate(my_list$t0, nbins = 310), type = "l", col="blue")
abline(v=84, col="red")
axis(1, at=c(0, 50, 84, 100, 150, 200, 250, 300))

legend("topright", legend = c("Uncertainty Area ", "Inferred Infections", "Start of lockdown"),
       col = c("grey", "blue", "red"), lwd=2, cex = 0.8)  






