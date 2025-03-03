#in this R code the simulation is performed using the new distribution proposed
#in the report, along with implementation of our simulation (details can be found
#in the section "Improving the simulation" and in "results")

#the implementation of the score function in order to assign the driver to the
#passenger (new version of match.P.to.D) is the main change in this code compared
#to the previous version of the simulations.

#Aside of the function used, the main code is the same as the previous version,
#for more detailed commenting the previous version 'BoxCar' can be looked at.


library(truncnorm)
set.seed(123)
note_drivers <- data.frame(index.d = numeric(0),
                           location.d = I(list()), 
                           DA = numeric(0),
                           DD = numeric(0),
                           DB = logical(0),
                           income = numeric(0),
                           working.time = numeric(0),
                           single.drive.t = numeric(0),
                           money.drive = numeric(0))

note_passengers <- data.frame(index.p = numeric(0),
                              location.begin = I(list()),
                              location.end = I(list()),
                              AB = logical(0),
                              PA = numeric(0),
                              PB = logical(0),
                              waiting.time = numeric(0))


event.calendar <- data.frame(event.type = character(0),
                             index.driver = numeric(0),
                             index.passenger = numeric(0),
                             event.time = numeric(0))
# distribution
inter.arrival.driver <- function(){
  rexp(1,rate = 4.09)
}

inter.arrival.passenger <- function(){
  rexp(1,rate = 30)
}

patience.time <- function(){
  rexp(1,rate = 5)
}

work.time.driver <- function(){
  runif(1,6,8)
} 

appear.location.dirver <- function(){
  x_coordinate <- rtruncnorm(1, a = 0, b = 20, mean = 9.608550, sd = 4.777625)
  y_coordinate <- rtruncnorm(1, a = 0, b = 20, mean = 11.825901, sd = 4.998953)
  return(c(x_coordinate,y_coordinate))
}

appear.location.passenger.pu <- function(){
  x_coordinate <- rtruncnorm(1, a = 0, b = 20, mean = 4.516799 , sd = 5.156148)
  y_coordinate <- rtruncnorm(1, a = 0, b = 20, mean = 7.537288, sd = 5.088524)
  return(c(x_coordinate,y_coordinate))
}

appear.location.passenger.do <- function(){
  x_coordinate <- rtruncnorm(1, a = 0, b = 20, mean = 9.009698 , sd = 5.497038)
  y_coordinate <- rtruncnorm(1, a = 0, b = 20, mean = 11.908724, sd = 5.367687)
  return(c(x_coordinate,y_coordinate))
}

driving.time.price <- function(index.d,index.p){
  loc.d <- note_drivers[index.d,]$location.d[[1]]
  loc.p <- note_passengers[index.p,]$location.end[[1]]
  dod <- dist(rbind(loc.d,loc.p))
  e.time <- dod/20
  r.time <- runif(1,e.time*0.8,e.time*1.2)
  price <- 3 + (2-0.2)*dod
  
  return(c(r.time,price))  
  
}

driving.time.cost <- function(index.d,index.p){
  loc.d <- note_drivers[index.d,]$location.d[[1]]
  loc.p <- note_passengers[index.p,]$location.begin[[1]]
  dod <- dist(rbind(loc.d,loc.p))
  e.time <- dod/20
  r.time <- runif(1,e.time*0.8,e.time*1.2)
  cost <- -0.2 * dod
  return(c(r.time,cost))
}

# check D to P (ALTERNATIVE IN CASE WANT TO SIMULATE BY PASSANGER WHO HAS
                #BEEN WAITING THE MOST)

#match.D.to.P <- function(index.d,index.p){
#  return(index.p[1])
#}

match.D.to.P <- function(index.d,index.p){
  loc.d <- note_drivers[index.d,]$location.d[[1]]
  distance <- rep(0,length(index.p))
  for (i in 1:length(index.p)){
    loc.p <- note_passengers[index.p[i],]$location.begin[[1]]
    distance[i] <- dist(rbind(loc.d,loc.p))
  }
  return(index.p[which.min(distance)])
}


match.P.to.D <- function(index.p, index.d, weight_work = 10) {
  loc_p <- note_passengers[index.p, ]$location.begin[[1]]
  score <- numeric(length(index.d))
  
  for (i in seq_along(index.d)) {
    driver_index <- index.d[i]
    loc_d <- note_drivers[driver_index, ]$location.d[[1]]
    distance <- dist(rbind(loc_p, loc_d))
    working_time <- note_drivers[driver_index, ]$working.time
    
    score[i] <- distance + weight_work * working_time
  }
  
  return(index.d[which.min(score)])
}

generate.driver <- function(index.d, time.now){
  outcome <- data.frame(index.d = index.d, 
                        location.d = I(list(appear.location.dirver())), 
                        DA = time.now, 
                        DD = time.now + work.time.driver(),
                        DB = FALSE,
                        income = 0,
                        working.time = 0,
                        single.drive.t = 0,
                        money.drive = 0)
  return(outcome)
}

generate.passenger <- function(index.p, time.now){
  outcome <- data.frame(index.p = index.p,
                        location.begin = I(list(appear.location.passenger.pu())),
                        location.end = I(list(appear.location.passenger.do())),
                        AB = FALSE,
                        PA = time.now,
                        PB = FALSE,
                        waiting.time = 0)
  return(outcome)
}

generate.event <- function(event.type,index.driver,index.passenger,event.time){
  outcome <- data.frame(event.type = event.type,
                        index.driver = index.driver,
                        index.passenger = index.passenger,
                        event.time = event.time)
  return(outcome)
}

termination.event <- generate.event("T",0,0,24*5)

# init element
tnow <- 0 
num.driver <- 0
num.passenger <- 0

event.calendar <- rbind(event.calendar,termination.event)
event.calendar <- rbind(event.calendar,generate.event("DA",1,0,inter.arrival.driver()))
event.calendar <- rbind(event.calendar,generate.event("PA",0,1,inter.arrival.passenger()))


event.calendar <- event.calendar[order(event.calendar$event.time),]


while (tnow < termination.event$event.time){ # right now tnow is previous time
  event.type.now <- event.calendar[1,]$event.type
  tnow <- event.calendar[1,]$event.time
  if(event.type.now == "DA"){
    num.driver <- num.driver + 1
    
    # update note_drivers
    note_drivers <- rbind(note_drivers,generate.driver(num.driver,tnow))
    
    # generate next DA event, for next driver
    event.calendar <- rbind(event.calendar,generate.event("DA",num.driver+1,0,tnow + inter.arrival.driver()))
    
    # generate DD for recent
    event.calendar <- rbind(event.calendar,generate.event("DD",num.driver,0,tnow+work.time.driver()))
    
    if (any(note_passengers$PB == FALSE)){
      # find the passenger to pick up
      pass.free.index <- which(note_passengers$PB == FALSE)
      next.passenger.index <- match.D.to.P(num.driver,pass.free.index)
      
      drive.pickup <- driving.time.cost(num.driver,next.passenger.index)  #(r.time,cost)
      
      #store single drive time and money
      note_drivers[num.driver,]$single.drive.t <- drive.pickup[1]
      note_drivers[num.driver,]$money.drive <- drive.pickup[2]
      
      # insert PP
      event.calendar = rbind(event.calendar,generate.event("PP",
                                                           num.driver,
                                                           next.passenger.index,
                                                           tnow + note_drivers[num.driver,]$single.drive.t))
      
      # delete PL
      event.calendar = event.calendar[-(which(event.calendar$event.type == "PL" & 
                                                event.calendar$index.passenger == next.passenger.index)),] 
      #update DB and PB
      note_drivers[num.driver,]$DB <- TRUE
      note_passengers[next.passenger.index,]$PB <- TRUE
    }
    
  } else if(event.type.now == "DD"){
    now.driver.index <- event.calendar[1,]$index.driver
    PP.index <- which(event.calendar$event.type == "PP" & event.calendar$index.driver == now.driver.index) # int
    if (note_drivers[now.driver.index,]$DB == FALSE){    #driver not working when he leaves
      
      # driver cannot be selected anymore
      note_drivers[now.driver.index,]$DB = TRUE 
      
    }else if(identical(PP.index, integer(0)) == FALSE){
      # find passenger
      now.passenger.index <- event.calendar[PP.index,]$index.passenger
      
      # update waiting time
      note_passengers[now.passenger.index,]$waiting.time <- event.calendar[PP.index,]$event.time - note_passengers[now.passenger.index,]$PA 
      
      # update working time and income(price)
      note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time + 
        note_drivers[now.driver.index,]$single.drive.t
      note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income + 
        note_drivers[now.driver.index,]$money.drive
      
      #update location
      note_drivers[now.driver.index,]$location.d[[1]] <- note_passengers[now.passenger.index,]$location.begin[[1]]
      
      # generate PD
      drive.to.dropoff <- driving.time.price(now.driver.index,now.passenger.index)
      
      # update the working time and income for future PD
      note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income + drive.to.dropoff[2]
      note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time + drive.to.dropoff[1]
      
      # update DD
      note_drivers[now.driver.index,]$DD <- event.calendar[PP.index,]$event.time + drive.to.dropoff[1]
      
      # delete PP
      event.calendar <- event.calendar[-PP.index,]
      
    } else{  #driver is working when he wants to leave
      
      # get the index of PD from event calendar
      PD.index <- which(event.calendar$event.type == "PD" & event.calendar$index.driver == now.driver.index)
      
      # index of passenger on board
      now.passenger.index <- event.calendar[PD.index,]$index.passenger
      
      # update DD to time of dropping off passenger
      note_drivers[now.driver.index,]$DD <- event.calendar[PD.index,]$event.time
      
      # update the working time and income for driver
      note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time +
        note_drivers[now.driver.index,]$single.drive.t
      note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income + 
        note_drivers[now.driver.index,]$money.drive
      
      # delete PD
      event.calendar <- event.calendar[-PD.index,]
    } 
  }else if(event.type.now == "PA"){
    
    num.passenger <- num.passenger + 1
    
    # update note_passenger
    note_passengers <- rbind(note_passengers,generate.passenger(num.passenger,tnow))
    
    # generate next PA event, for next passenger
    event.calendar <- rbind(event.calendar,generate.event("PA",num.passenger+1,0,tnow + inter.arrival.passenger()))
    
    if(any(note_drivers$DB == FALSE)){ # the note_drivers is not empty and have free drivers
      # match the driver
      driver.free.index <- which(note_drivers$DB == FALSE)
      next.driver.index <- match.P.to.D(num.passenger,driver.free.index)
      drive.pickup <- driving.time.cost(next.driver.index,num.passenger)
      
      # store the single drive time and money
      note_drivers[next.driver.index,]$single.drive.t <- drive.pickup[1]
      note_drivers[next.driver.index,]$money.drive <- drive.pickup[2]
      
      # insert PP
      event.calendar <- rbind(event.calendar,generate.event("PP",next.driver.index,num.passenger,tnow +
                                                              note_drivers[next.driver.index,]$single.drive.t))
      
      # update DB and PB
      note_drivers[next.driver.index,]$DB <- TRUE
      note_passengers[num.passenger,]$PB <- TRUE
    }else{
      
      # insert PL
      event.calendar <- rbind(event.calendar,generate.event("PL",0,num.passenger,tnow + patience.time()))
    }
    
  }else if(event.type.now == "PP"){
    now.passenger.index <- event.calendar[1,]$index.passenger
    now.driver.index <- event.calendar[1,]$index.driver
    
    # update waiting, working, income(cost)
    note_passengers[now.passenger.index,]$waiting.time <- tnow - note_passengers[now.passenger.index,]$PA
    note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time + 
      note_drivers[now.driver.index,]$single.drive.t
    note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income + 
      note_drivers[now.driver.index,]$money.drive
    
    #update location of driver
    note_drivers[now.driver.index,]$location.d[[1]] <- note_passengers[now.passenger.index,]$location.begin[[1]]
    
    # store the single drive time and money
    drive.to.dropoff <- driving.time.price(now.driver.index,now.passenger.index)
    note_drivers[now.driver.index,]$single.drive.t <- drive.to.dropoff[1]
    note_drivers[now.driver.index,]$money.drive <- drive.to.dropoff[2]
    
    # insert PD
    event.calendar <- rbind(event.calendar,generate.event("PD",now.driver.index,now.passenger.index,tnow + 
                                                            note_drivers[now.driver.index,]$single.drive.t))
    
  }else if(event.type.now == "PD"){
    now.passenger.index <- event.calendar[1,]$index.passenger
    now.driver.index <- event.calendar[1,]$index.driver
    
    # update working, income(price)
    note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time +
      note_drivers[now.driver.index,]$single.drive.t
    note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income +
      note_drivers[now.driver.index,]$money.drive
    
    # update the location of driver
    note_drivers[now.driver.index,]$location.d[[1]] <- note_passengers[now.passenger.index,]$location.end[[1]]
    
    # search next passenger
    if(any(note_passengers$PB == FALSE)){
      
      # find the passenger to pick up
      pass.free.index <- which(note_passengers$PB == FALSE)
      next.passenger.index <- match.D.to.P(now.driver.index,pass.free.index)
      
      # store the single drive time and money
      drive.pickup <- driving.time.cost(now.driver.index,next.passenger.index)  #(r.time,cost)
      note_drivers[now.driver.index,]$single.drive.t <- drive.pickup[1]
      note_drivers[now.driver.index,]$money.drive <- drive.pickup[2]
      
      # insert PP
      event.calendar = rbind(event.calendar,generate.event("PP",
                                                           now.driver.index,
                                                           next.passenger.index,
                                                           tnow + note_drivers[now.driver.index,]$single.drive.t ))
      # delete PL
      event.calendar = event.calendar[-(which(event.calendar$event.type == "PL" & 
                                                event.calendar$index.passenger == next.passenger.index)),]
      #update PB
      note_passengers[next.passenger.index,]$PB <- TRUE
    } else{
      note_drivers[now.driver.index,]$DB <- FALSE
    }
  }else if(event.type.now == "PL"){
    now.passenger.index <- event.calendar[1,]$index.passenger
    
    #update AB and PB
    note_passengers[now.passenger.index,]$AB <- TRUE
    note_passengers[now.passenger.index,]$PB <- TRUE
  }else{#termination
    
    
    fil_dirver_note <- note_drivers[note_drivers$DA <= 4*24+12,]
    fil_passenger_note <- note_passengers[note_passengers$PA <= 4*24+12,]
    
    # Rider
    
    # rate of abandonments
    abandon <- mean(note_passengers$AB)
    
    # waiting time for pick-up
    mean_waiting_time <- mean(fil_passenger_note$waiting.time[fil_passenger_note$AB == FALSE])
    
    # driver
    
    # average earning per hour
    income_per_hour <- na.omit(fil_dirver_note$income / (fil_dirver_note$DD - fil_dirver_note$DA))
    mean_income_hour <- mean(income_per_hour)
    
    #fairness
    
    sd_income_hour <- sd(income_per_hour)
    quan <- quantile(income_per_hour,c(0.05,0.95))
    
    
    #rest time
    rest_time <- fil_dirver_note$DD - fil_dirver_note$DA - fil_dirver_note$working.time
    mean_rest_time <- mean(rest_time)
    sd_rest_time <- sd(rest_time)
    quan_res <- quantile(rest_time,c(0.05,0.95))
    
    rr.2 <- list(AB = abandon, WT = mean_waiting_time, IPH = mean_income_hour, FAIR_SD = sd_income_hour, FAIR_Q = quan ,RES = c(mean_rest_time,sd_rest_time),
                 RES_Q = quan_res)
  }
  event.calendar <- event.calendar[-1,]
  event.calendar <- event.calendar[order(event.calendar$event.time),]
  
}


