#In this R-code the original simulation, with distributions provided by BoxCar
#has been implemented

#set the seed in order to obtain reproducible results
set.seed(123)

#initialisation of dataset for drivers
note_drivers <- data.frame(index.d = numeric(0),
                          location.d = I(list()), 
                          DA = numeric(0),
                          DD = numeric(0),
                          DB = logical(0),
                          income = numeric(0),
                          working.time = numeric(0),
                          single.drive.t = numeric(0),
                          money.drive = numeric(0))
#dataset for passenger
note_passengers <- data.frame(index.p = numeric(0),
                              location.begin = I(list()),
                              location.end = I(list()),
                              AB = logical(0),
                              PA = numeric(0),
                              PB = logical(0),
                              waiting.time = numeric(0))

#event calendar
event.calendar <- data.frame(event.type = character(0),
                             index.driver = numeric(0),
                             index.passenger = numeric(0),
                             event.time = numeric(0))

# distributions
inter.arrival.driver <- function(){
  rexp(1,rate = 3)
}

inter.arrival.passenger <- function(){
  rexp(1,rate = 30)
}

patience.time <- function(){
  rexp(1,rate = 5)
}

work.time.driver <- function(){
  runif(1,5,8)
} 

appear.location <- function(){
  runif(2,0,20)
}


driving.time.price <- function(index.d,index.p){
  #function to compute ride fare and fuel cost for driver
  #argument index.d and index.p are the index for driver and rider matched
  
  #driver location
  loc.d <- note_drivers[index.d,]$location.d[[1]]
  #rider location
  loc.p <- note_passengers[index.p,]$location.end[[1]]
  #euclidean distance
  dod <- dist(rbind(loc.d,loc.p))
  #expected time
  e.time <- dod/20
  #real time
  r.time <- runif(1,e.time*0.8,e.time*1.2)
  #fare for the drive: £3 pounds base, £2 for mile minus -£0.2 per mile of fuel
  price <- 3 + (2-0.2)*dod
  
  return(c(r.time,price))  
    
}

driving.time.cost <- function(index.d,index.p){
  #function to compute cost of fuel for the driver going to pick up rider
  #index.d and index.p are index of the matched couple driver-rider
  #returns how long the drive takes and how much fuel cost
  
  #driver location
  loc.d <- note_drivers[index.d,]$location.d[[1]]
  
  #passenger location
  loc.p <- note_passengers[index.p,]$location.begin[[1]]
  
  #euclidean distance between driver and passenger
  dod <- dist(rbind(loc.d,loc.p))
  #expected time
  e.time <- dod/20
  #real time
  r.time <- runif(1,e.time*0.8,e.time*1.2)
  #cost of fuel for the drive
  cost <- -0.2 * dod
  #return real time and fuel cost of the drive
  return(c(r.time,cost))
}

# check D to P
match.D.to.P <- function(index.d,index.p){
  
  #function to match a driver to a rider
  #index.d is the index of the available driver
  #index.p is a list of all riders looking for adriver
  
  #extract driver location
  loc.d <- note_drivers[index.d,]$location.d[[1]]
  
  #compute distance between rider and each rider waiting for a match
  distance <- rep(0,length(index.p))
  for (i in 1:length(index.p)){
    loc.p <- note_passengers[index.p[i],]$location.begin[[1]]
    distance[i] <- dist(rbind(loc.d,loc.p))
  }
  #return index of the closest rider looking for a match
  return(index.p[which.min(distance)])
}

# check P to D
match.P.to.D <- function(index.p,index.d){
  #matching a passenger to a driver
  #index.p is index of the rider looking for a driver
  #index.d is a vector of alla available drivers
  
  #extract passenger location
  loc.p <- note_passengers[index.p,]$location.begin[[1]]
  
  #compute distance between each passenger and all available drivers
  distance <- rep(0,length(index.d))
  for (i in 1:length(index.d)){
    loc.d <- note_drivers[index.d[i],]$location.d[[1]]
    distance[i] <- dist(rbind(loc.p,loc.d))
  }
  #return index of the closest driver available
  return(index.d[which.min(distance)])
}

# generation
generate.driver <- function(index.d, time.now){
  #generate a new driver in the system
  #index d is index of a new driver, time.now it's the time of arrival in the
  #system
  
  outcome <- data.frame(index.d = index.d, 
                        location.d = I(list(appear.location())), 
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
  #generate a new passenger in the system
  #index p is index of a new rider, time.now it's the time of arrival in the
  #system
  outcome <- data.frame(index.p = index.p,
                        location.begin = I(list(appear.location())),
                        location.end = I(list(appear.location())),
                        AB = FALSE,
                        PA = time.now,
                        PB = FALSE, #PB = passenger busy = FALSE (passenger is looking
                                                                  #for a driver)
                        waiting.time = 0)
  return(outcome)
}

generate.event <- function(event.type,index.driver,index.passenger,event.time){
  #generate a new event
  #event.type = event that is generated
  #index.driver = index of the driver involved (if no driver is involved, then 0)
  #index.passenger = index of the driver involved (if no driver is involved, then 0)
  #event.time = time at which event will happen
  
  outcome <- data.frame(event.type = event.type,
                        index.driver = index.driver,
                        index.passenger = index.passenger,
                        event.time = event.time)
  return(outcome)
}

#for our simulation we set the termination at 5 days
termination.event <- generate.event("T",0,0,24*5)

#initialise elements
tnow <- 0 
num.driver <- 0
num.passenger <- 0

#add termination to calendar
event.calendar <- rbind(event.calendar,termination.event)

#generate first driver
event.calendar <- rbind(event.calendar,generate.event("DA",1,0,inter.arrival.driver()))
#generate first passenger
event.calendar <- rbind(event.calendar,generate.event("PA",0,1,inter.arrival.passenger()))

#sort crhonologically the event.time
event.calendar <- event.calendar[order(event.calendar$event.time),]

#base our simulation on a while loop that runs until the time does not exceed
#termination
while (tnow < termination.event$event.time){
  #get event type of next event (event calendar is sorted so always first line)
  event.type.now <- event.calendar[1,]$event.type
  #get actual time
  tnow <- event.calendar[1,]$event.time
  
  #DRIVER ARRIVAL (DA)
  if(event.type.now == "DA"){
    #update number of driver
    num.driver <- num.driver + 1
    
    # generate new driver and update the drivers dataset
    note_drivers <- rbind(note_drivers,generate.driver(num.driver,tnow))
    
    # generate next DA event, for next driver
    event.calendar <- rbind(event.calendar,generate.event("DA",num.driver+1,0,tnow + inter.arrival.driver()))
    
    # generate DD (driver departure = when driver leaves the system) 
    event.calendar <- rbind(event.calendar,generate.event("DD",num.driver,0,tnow+work.time.driver()))
    
    #now driver looks for passengers
    if (any(note_passengers$PB == FALSE)){ #if any passenger is looking for a driver
      
      # find all passenger waiting for pick-up
      pass.free.index <- which(note_passengers$PB == FALSE)
      
      #match driver to passenger
      next.passenger.index <- match.D.to.P(num.driver,pass.free.index)
      
      #compute drive time and cost of the driver to go to pick up point
      drive.pickup <- driving.time.cost(num.driver,next.passenger.index)  #(r.time,cost)
      
      #store information about the drive to pick up (time and cost)
      note_drivers[num.driver,]$single.drive.t <- drive.pickup[1]
      note_drivers[num.driver,]$money.drive <- drive.pickup[2]
      
      #insert pick up event in the calendar (PP = passenger pick-up)
      event.calendar = rbind(event.calendar,generate.event("PP",
                                                             num.driver, #index of actual driver
                                                             next.passenger.index, #index of matched rider
                                                             tnow + note_drivers[num.driver,]$single.drive.t))#time of pickup = tnow + time of the ride
       
      #since we are picking up the passenger we delete 
      #the PL = passenger leaves (abandonment) event
      event.calendar = event.calendar[-(which(event.calendar$event.type == "PL" & 
                                                event.calendar$index.passenger == next.passenger.index)),] 
      #update DB (driver is now busy) 
      note_drivers[num.driver,]$DB <- TRUE
      #update PB (passenger matched a driver, so it's not looking for pick-up anymore)
      note_passengers[next.passenger.index,]$PB <- TRUE
    }
    
  } else if(event.type.now == "DD"){ #DRIVER DEPARTURE (driver leaves the system)
    #get index of the driver
    now.driver.index <- event.calendar[1,]$index.driver
    
    #try to look if the driver is going to pick-up a passenger at the moment (if
    #it's not PP.index will be integer(0))
    PP.index <- which(event.calendar$event.type == "PP" & 
                        event.calendar$index.driver == now.driver.index)
    
    if (note_drivers[now.driver.index,]$DB == FALSE){ #driver not working 
      
      #driver cannot be selected anymore and leaves the system
      note_drivers[now.driver.index,]$DB = TRUE 
      
    #driver is going to pick-up someone (PP.index is not integer(0))
    }else if(identical(PP.index, integer(0)) == FALSE){ 
      
      #find passenger who the driver is picking up
      now.passenger.index <- event.calendar[PP.index,]$index.passenger
      
      #update waiting time of the passenger (pick-up time - passenger arrivals time)
      note_passengers[now.passenger.index,]$waiting.time <- event.calendar[PP.index,]$event.time  - note_passengers[now.passenger.index,]$PA 
      
      # update working time and income(price)
      #note: we store info about price and time of pick up drive
      #when matching between driver and rider happens
      
      #update working time of driver
      note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time + 
                                                       note_drivers[now.driver.index,]$single.drive.t
      #update income of driver
      note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income + 
                                                note_drivers[now.driver.index,]$money.drive
      
      #update location of driver (goes to pick-up location)
      note_drivers[now.driver.index,]$location.d[[1]] <- note_passengers[now.passenger.index,]$location.begin[[1]]
      
      #generate info for drive from pick-up to drop-off
      drive.to.dropoff <- driving.time.price(now.driver.index,now.passenger.index)
      
      #update the working time and income for the driver (going to drop-off)
      note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income + drive.to.dropoff[2]
      note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time + drive.to.dropoff[1]
      
      # update DD (driver will leave the system after completed the ride)
      note_drivers[now.driver.index,]$DD <- event.calendar[PP.index,]$event.time + drive.to.dropoff[1]
      
      # delete PP (delete passenger pick-up, we have made it happen just above)
      #note PD not needed for this passenger since happened already
      event.calendar <- event.calendar[-PP.index,]
      
    } else{  #driver is going from pick-up to drop-off when he wants to leave
      
      # get the index of PD (passenger drop off) from event calendar
      PD.index <- which(event.calendar$event.type == "PD" & event.calendar$index.driver == now.driver.index)
      
      # get index of the passenger on board
      now.passenger.index <- event.calendar[PD.index,]$index.passenger
      
      # update DD (driver departure) to time of dropping off passenger
      note_drivers[now.driver.index,]$DD <- event.calendar[PD.index,]$event.time
      
      # update the working time and income for driver
      #because of the drive from pick up to drop off
      #working time
      note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time +
                                                      note_drivers[now.driver.index,]$single.drive.t
      #income
      note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income + 
                                                note_drivers[now.driver.index,]$money.drive
      
      #delete PD (passenger departure, made that happen here)
      event.calendar <- event.calendar[-PD.index,]
    } 
  }else if(event.type.now == "PA"){ #passenger arrival
    
    #new passenger
    num.passenger <- num.passenger + 1
    
    # update passangers dataset
    note_passengers <- rbind(note_passengers,generate.passenger(num.passenger,tnow))
    
    # generate next passanger arrivaò
    event.calendar <- rbind(event.calendar,generate.event("PA",num.passenger+1,0,tnow + inter.arrival.passenger()))
    
    if(any(note_drivers$DB == FALSE)){ #drivers dataset is not empty and there are availble drivers
      
      # match the driver
      
      #indexes of free drivers
      driver.free.index <- which(note_drivers$DB == FALSE)
      #match the driver
      next.driver.index <- match.P.to.D(num.passenger,driver.free.index)
      #get info on drive to pick-up (time and cost for driver)
      drive.pickup <- driving.time.cost(next.driver.index,num.passenger)
      
      # store the info about the drive to pick up in the driver's line
      note_drivers[next.driver.index,]$single.drive.t <- drive.pickup[1]
      note_drivers[next.driver.index,]$money.drive <- drive.pickup[2]
      
      # insert PP (pick-up event of the rider)
      event.calendar <- rbind(event.calendar,generate.event("PP",next.driver.index,num.passenger,tnow +
                                                              note_drivers[next.driver.index,]$single.drive.t))
        
      # update DB (driver is now busy)
      note_drivers[next.driver.index,]$DB <- TRUE
      #update PB (passenger has matched)
      note_passengers[num.passenger,]$PB <- TRUE
    
      }else{# no drivers are available to pick up the rider
      
      # insert PL (passenger leaves)
      #passenger will leave at time tnow + patience time generated
      event.calendar <- rbind(event.calendar,generate.event("PL",0,num.passenger,tnow + patience.time()))
    }
    
  }else if(event.type.now == "PP"){ #passenger pick-up
    #get index of the driver
    now.passenger.index <- event.calendar[1,]$index.passenger
    #get index of the passenger
    now.driver.index <- event.calendar[1,]$index.driver
    
    
    #waiting time is time of pick-up (now) - arrival time of the passenger
    note_passengers[now.passenger.index,]$waiting.time <- tnow -
      note_passengers[now.passenger.index,]$PA
    
    # update working time and income for driver goin to pick up the passenger
    #info have been stored previosly for this (when match happens between driver
    #and rider)
    
    #working time
    note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time + 
      note_drivers[now.driver.index,]$single.drive.t
    
    #uodate income (cost of fuel)
    note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income + 
      note_drivers[now.driver.index,]$money.drive
    
    #update location of driver (pick-up)
    note_drivers[now.driver.index,]$location.d[[1]] <- note_passengers[now.passenger.index,]$location.begin[[1]]
    
    #generate info about drive from pick-up to drop-off and store them in 
    #driver's line
    drive.to.dropoff <- driving.time.price(now.driver.index,now.passenger.index)
    #time 
    note_drivers[now.driver.index,]$single.drive.t <- drive.to.dropoff[1]
    #fare
    note_drivers[now.driver.index,]$money.drive <- drive.to.dropoff[2]
    
    # insert PD
    event.calendar <- rbind(event.calendar,generate.event("PD",now.driver.index,now.passenger.index,tnow + 
                                                            note_drivers[now.driver.index,]$single.drive.t))
    
  }else if(event.type.now == "PD"){ #passenger drop-off
    #get passenger and driver index
    now.passenger.index <- event.calendar[1,]$index.passenger
    now.driver.index <- event.calendar[1,]$index.driver
    
    # update working, income(price) of the drive from pick-up to drop-off
    note_drivers[now.driver.index,]$working.time <- note_drivers[now.driver.index,]$working.time +
      note_drivers[now.driver.index,]$single.drive.t
    note_drivers[now.driver.index,]$income <- note_drivers[now.driver.index,]$income +
      note_drivers[now.driver.index,]$money.drive
    
    # update the location of driver
    note_drivers[now.driver.index,]$location.d[[1]] <- note_passengers[now.passenger.index,]$location.end[[1]]
    
    # search next passenger
    if(any(note_passengers$PB == FALSE)){ #if any passenger is waiting for pick-up
      
      # find the passenger to pick up
      pass.free.index <- which(note_passengers$PB == FALSE)
      next.passenger.index <- match.D.to.P(now.driver.index,pass.free.index)
      
      # store the single drive time and money (drive to pick up new passenger)
      drive.pickup <- driving.time.cost(now.driver.index,next.passenger.index)  #(r.time,cost)
      note_drivers[now.driver.index,]$single.drive.t <- drive.pickup[1]
      note_drivers[now.driver.index,]$money.drive <- drive.pickup[2]
      
      # insert PP (passenger pick up event)
      event.calendar = rbind(event.calendar,generate.event("PP",
                                                           now.driver.index,
                                                           next.passenger.index,
                                                           tnow + note_drivers[now.driver.index,]$single.drive.t ))
     
       # delete PL (passenger will not abandon since we are going to pick up )
      event.calendar = event.calendar[-(which(event.calendar$event.type == "PL" & 
                                                event.calendar$index.passenger == next.passenger.index)),]
      #update PB (passenger is now matched)
      note_passengers[next.passenger.index,]$PB <- TRUE
    } else{ #no passenger are waiting for pick up
      #driver is not busy anymore
      note_drivers[now.driver.index,]$DB <- FALSE
    }
  }else if(event.type.now == "PL"){ #passenger abandonmentù
    #get passenger index
    now.passenger.index <- event.calendar[1,]$index.passenger
    
    #update AB (passenger abandons the system)
    note_passengers[now.passenger.index,]$AB <- TRUE
    #update PB (passenger will not look for a match anymore)
    note_passengers[now.passenger.index,]$PB <- TRUE
    
  }else{#Termination
    
    #get results of the simulation
    
    #To not consider boundary effects, we only consider results of
    #driver and passenger who enter the system before 12 hours of the 
    #end of termination
    fil_dirver_note <- note_drivers[note_drivers$DA <= 4*24+12,]
    fil_passenger_note <- note_passengers[note_passengers$PA <= 4*24+12,]
    
    # Rider
    
    # rate of abandonments
    abandon <- mean(note_passengers$AB)
    
    # waiting time for pick-up (only consider waiting time of people who did not abandon)
    mean_waiting_time <- mean(fil_passenger_note$waiting.time[fil_passenger_note$AB == FALSE])
    
    # driver
    
    # average earning per hour (resting time included)
    income_per_hour <- na.omit(fil_dirver_note$income / (fil_dirver_note$DD - fil_dirver_note$DA))
    mean_income_hour <- mean(income_per_hour)
    
    #fairness (compute standard deviations and quantiles of income per hour)
    
    sd_income_hour <- sd(income_per_hour)
    quan <- quantile(income_per_hour,c(0.05,0.95))
    
    
    #rest time
    #resting time is shift duration (DD - DA) minus working time
    rest_time <- fil_dirver_note$DD - fil_dirver_note$DA - fil_dirver_note$working.time
    mean_rest_time <- mean(rest_time)
    
    #to analyse fairnes compute sd and quantiles of resting time
    sd_rest_time <- sd(rest_time)
    quan_res <- quantile(rest_time,c(0.05,0.95))
    
    #store results
    rr <- list(AB = abandon, WT = mean_waiting_time, IPH = mean_income_hour, FAIR_SD = sd_income_hour, FAIR_Q = quan ,RES = c(mean_rest_time,sd_rest_time),
               RES_Q = quan_res)
    
    
  }
  
  #delete first element of the event calendar (the one that just happened)
  event.calendar <- event.calendar[-1,]
  #sort the calendar by the event time (increasing order), so first event
  #is the one that will happen next
  event.calendar <- event.calendar[order(event.calendar$event.time),]
}

