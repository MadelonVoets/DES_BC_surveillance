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
p.disease <-
p.detected <-
p.death <-  
p.sens.test <-
p.spec.test <-
time_img <- 1
time_biopsy <- 1
time_treatment <- 1

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
setTherapy <-
setRecurrence <- 
setTestOutcome <- ifelse(setRecurrence == 1,
                                 ifelse(runif(1) < p.sens.test, 1, 0),                   # 1 = true positive, 0 = false negative
                                 ifelse(setRecurrence == 0,
                                        ifelse(runif(1) < (1 - p.spec.test), 1, 0),      # 1 = false positive, 0 = true negative
                                        0))     
  
  
FUP.event <- function() {
  
  # Possible events:
  # 1) TN (full cycle)
  # 2) TP/FP
  # 3) FN
  # 4) Death other causes
  # 5) Maximum imaging events
  
  # If the maximum number of cycles is received by the patient, the event is 5
  if(FUP.year>=max.cycles) {
    
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
                 "1"=rweibull(),
                 "2"=rweibull(),
                 "3"=rweibull(),
                 "4"=runif(),
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
  set_attribute(key="FUP.Event", value=function() FUP.event(FUP.years = get_attribute(bsc.sim, "FUP.year"))) %>%
  branch(option=function() get_attribute(bsc.sim, "FUP.Event"), continue=c(T,T,T,F,T),
        
         #Event 1: True Negative
         trajectory() %>%
           set_attribute(key="FUP.year", mod="+", value=function() 1) %>%
           seize(resource="TN", amount=1) %>% 
           timeout(task = "time_img") %>%
           release(resource="TN", amount=1) %>%
           rollback(target=8, times=Inf),
         
         #Event 2: True Positive / False Positive
         trajectory() %>%
           timeout(task = "time_img") %>%
           branch(option=function() get_attribute(bsc.sim, "Biopsy.result"), continue=c(T,T),
                  #negative biopsy - rollback to FUP.event 
                  trajectory() %>%
                    set_attribute(key="FUP.year", mod="+", value=function() 1) %>%
                    seize(resource="FP", amount=1) %>% 
                    timeout(task = "time_biopsy") %>%
                    release(resource="FP", amount=1) %>%
                    rollback(target=7),
                    
                  #positive biopsy 
                  trajectory() %>%  
                    set_attribute(key="FUP.year", mod="+", value=function() 1) %>%
                    seize(resource="TP", amount=1) %>% 
                    timeout(task = "time_biopsy") %>%
                    release(resource="TP", amount=1)
           ),
         #Event 3: False Negative
         trajectory() %>%
           set_attribute(key="FUP.year", mod="+", value=function() 1) %>%
           seize(resource="FP", amount=1) %>% 
           timeout(task = "time_img") %>%
           release(resource="FP", amount=1),
         
         #Event 4: Death other causes
         trajectory() %>%
           set_attribute(key="FUP.year", mod="+", value=function() 1) %>%
           seize(resource="Death", amount=1) %>% 
           timeout(task = "time_img") %>%
           timeout(task="death") %>%
           release(resource="Death", amount=1),
           
         #Event 5: End of Follow-up
         trajectory() %>%
           timeout(task=function() FUP.time(get_attribute(bsc.sim, "FUP.Event")))
         
  )  %>% #Treatment positive biopsy
  set_attribute(key="Treat.Event", value=function() treat.event()) %>%
  seize(resource="Treatment", amount=1) %>% 
  timeout(task="time_treatment") %>%
  release(resource="Treatment", amount=1) %>%
  branch(option=function() get_attribute(bsc.sim, "Treat.Event"), continue=c(T,F),
         
         #Return to disease free after treatment
         trajectory() %>%
           rollback(target=13, times=Inf),
        
         #Death
         trajectory() %>%
           seize(resource="Death", amount=1) %>% 
           timeout(task="Time") %>%
           release(resource="Death", amount=1)
  ) %>% 
  
  #Distant Metastasis
  seize(resource="DM", amount=1) %>% 
  timeout(task="Time") %>%
  release(resource="DM", amount=1) %>% 

  #Breast Cancer Death
  seize(resource="BC_Death", amount=1) %>% 
  timeout(task="Time") %>%
  release(resource="BC_Death", amount=1)
         
   
# Visualize 
plot(bsc.model)

