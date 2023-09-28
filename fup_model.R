## Section 1: Initialization ----

# Clear the workspace
rm(list=ls()); gc();

# Load the required packages, make sure the required packages are installed. For the installation of the packages use line 13 (uncomment shift+Ctrl+C, or remove #)

# install.packages(c("simmer", "simmer.plot", "fitdistrplus")) # Install packages

library(simmer);
library(simmer.plot);
library(fitdistrplus);

# Set the working directory
setwd("")

# Load funtions for extracting monitored attributes
source("getSingleAttribute.R", echo=TRUE);
source("getMultipleAttributes.R", echo=TRUE);

# Load the dataset
load("");

## Section 2: Data analysis ----
#patient characteristics based on distributions??


# Define parameters
fit.death <- glm(formula=Death~Male+Age, family=binomial(link="logit"), data=dfit); # model for probability of death in a cycle

## Section 3: Supportive functions ----
setAge  <- function(Male) {                           
  
  if(Male==1) {
    age <- rnorm(n=1, mean=m.age.men, sd=sd.age.men);
  } else {
    age <- rnorm(n=1, mean=m.age.women, sd=sd.age.women);
  }
  
  return(age)
}  
setStage <-
setGrade <- 
setTrerapy <-
  
FUP.event <- function() {
  
  # Possible events:
  # 1) TN (full cycle)
  # 2) TP/FP
  # 3) FN
  # 4) Death other causes
  # 5) Maximum imaging events
  
  # If the maximum number of cycles is received by the patient, the event is 5
  if(Tx1.Cycles>=max.cycles) {
    
    event <- 5;
    
    # If not, randomly select the event to occur
  } else {
    
    # Determine applicable probabilities
    
    # Draw random number
    rand <- runif(1);
    
    if(rand<p.major) {
      event <- 3;       # Major complications
    } else if(rand<sum(p.major, p.death)) {
      event <- 4;       # Death
    } else {
      # A full cycle will be received by the patient, but there is still a chance that the patient
      # will experience minor complications
      event <- ifelse(runif(1)<p.minor, 2, 1);
    }
  }
  
  # Return the selected event
  return(event)
  
}

# Function for defining times
FUP.time <- function(FUP.Event) {
  
  # Possible events:
  # 1) TN (full cycle)
  # 2) TP/FP
  # 3) FN
  # 4) Death other causes
  # 5) Maximum imaging events
  
  # Select the appropriate cycle time
  time <- switch(FUP.Event,
                 "1"=rweibull(n=1, shape=t.cycle$estimate[1], scale=t.cycle$estimate[2]),
                 "2"=rweibull(n=1, shape=t.cycle$estimate[1], scale=t.cycle$estimate[2]),
                 "3"=rweibull(n=1, shape=t.major$estimate[1], scale=t.major$estimate[2]),
                 "4"=runif(n=1, min=t.death$estimate[1], max=t.death$estimate[2]),
                 "5"=0);
  
  # Return the selected time
  return(time);
}

Biopsy.result <- function() {
  #result <- 
    
  return(result);  
}

## Section 4: Discrete event simulation model ----
bsc.model <- trajectory() %>%
  
  # Initialization
  #patient characteristics
  set_attribute(key="FUP.year", value=0) %>%
  set_attribute(key="C", value=0) %>%
  set_attribute(key="E", value=0) %>%
  set_attribute(key="FUP.Event", value=function() FUP.event(Tx1.Cycles = get_attribute(bsc.sim, "FUP.year"))) %>%
  branch(option=function() get_attribute(bsc.sim, "FUP.Event"), continue=c(T,T,T,F,T),
        
         #Event 1: True Negative
         trajectory() %>%
           set_attribute(key="FUP.year", mod="+", value=function() 1) %>%
           #set_attribute(key="Time", value=function() FUP.time(get_attribute(bsc.sim, "FUP.Event"))) %>%
           seize(resource="TN", amount=1) %>% 
           timeout_from_attribute(key="Time") %>%
           release(resource="TN", amount=1) %>%
           #set_attribute(key="C", mod="+", value=function() c.Tx1.day*get_attribute(bsc.sim, "Time") + c.FUP.year) %>%
           #set_attribute(key="E", mod="+", value=function() get_attribute(bsc.sim, "QoL")*get_attribute(bsc.sim, "Time")) %>%
           rollback(amount=9, times=Inf),
         
         #Event 2: True Positive / False Positive
         trajectory() %>%
         
         #Event 3: False Negative
         trajectory() %>%
         
         #Event 4: Death other causes
         trajectory() %>%
           set_attribute(key="FUP.year", mod="+", value=function() 1) %>%
           #set_attribute(key="Time", value=function() FUP.time(get_attribute(bsc.sim, "FUP.Event"))) %>%
           seize(resource="Death", amount=1) %>% 
           timeout_from_attribute(key="Time") %>%
           release(resource="Death", amount=1),
           #set_attribute(key="C", mod="+", value=function() c.Tx1.day*get_attribute(bsc.sim, "Time") + c.Tx1.cycle) %>%
           #set_attribute(key="E", mod="+", value=function() get_attribute(bsc.sim, "QoL")*get_attribute(bsc.sim, "Time"))
           
         #Event 5: End of Follow-up
         trajectory() %>%
           timeout(task=function() FUP.time(get_attribute(bsc.sim, "FUP.Event")))
         
  )
   
# Visualize 
plot(bsc.model); 

# Visualize to check whether the defined model structure is ok
plot(bsc.model)