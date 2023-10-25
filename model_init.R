## 1: INITIALISATION ----

# Clear the workspace
rm(list=ls()); gc();

# Load the required packages, make sure the required packages are installed. 

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

## 2: PARAMETERS ----
# These are the parameters that define and are used in the trajectory. The prefix of the 
# parameter name indicates what type of parameter it is:
#   d_    distribution parameter
#   n_    count
#   p_    probability
#   t_    time duration
#   c_    cost
#   u_    utility


# Parameter for background mortality (time to death other causes)
t_to_death_oc <- 
t_to_LRR <-
t_to_DM <- 
t_first_fup <-   #between x and y days
t_fup <- 1  

  
p.sens.test <- 
p.spec.test <- 
# TO DO: differentiate between mammo/us/mri
  
# Parameters patient characteristics 
d_pt_age <-
d_LRR <-   #if time to recurrence is smaller than time past, LRR occurs
d_DM <-   #if time to dm is smaller than time past, dm occurs 
p  
  
# Parameters for tumour characteristics // INFLUENCE parameters
p_stage     <- 
p_grade     <- 
p_nstatus   <-
p_multif    <- 
p_her2      <- 
p_hr        <-
p_sur       <-
p_chemo     <-
p_rad       <-
p_horm      <-
p_antiher2  <-
  
p_therapy <-   #vector of combined chemo, radio, horm, antiher2?
  

## 3: FUNCTIONS ----
fn_img_outcome <- function() {
  out <-  ifelse(d_LRR == 1,
                 ifelse(runif(1) < p.sens.test, 1, 0),                   # 1 = true positive, 0 = false negative
                 ifelse(d_LRR == 0,
                        ifelse(runif(1) < (1 - p.spec.test), 1, 0),      # 1 = false positive, 0 = true negative
                        0)) 
  return(out)
}

fn_img_event <- function (){
  #mammo, mri or US
  out <- c(event)
  
  return(out)  
}

fn_time_to_events <- function(currentime, attrb) {
  # currenttime         simulation time
  # attrb               vector with times of death other causes, time of lrr, time of dm (time of bc death?)
  
  
  time_start <- attrb[1]
  time_of_death <- currenttime
  
  out <- c(time_start, time_of_death)
  
  return(out)
}


## 4: TRAJECTORY ----
out_trj <- trajectory() %>% 
  set_attribute(
    keys   = c("time_of_death"), 
    values = function() fn_time_to_events(
      currenttime = now(.env = sim), 
      attrb       = get_attribute(.env = sim, keys = c("start_time"))
    )
  )

main_trj <- trajectory() %>% 
  set_attribute(keys = "start_time", values = function() now(.env = sim)) %>% 
  renege_in(t = function() now(.env = sim) + t_to_death_oc, out = out_trj) %>% 
  
  # Time to first imaging event
  timeout(task = t_first_fup) %>% #distribution to follow-up event
  
  # Follow-up imaging event
  seize(resource = "Imaging") %>%
  set_attribute(
    keys = c("event"),
    values = function() fn_img_event(at = now(.env = sim)),
    mod = '+'
  ) %>%
  timeout(task = t_fup) %>%
  release(resource = "Imaging")

  #first branch, based on what the outcome is of the imaging event
  branch(option = function() fn_img_outcome(get_attribute(sim, "event")), continue = c(T,T,T,F,T), 
         #how to handle additional imaging event?
         #how to handle adherence? 
         
         #Event 1:True Negative
         #Event 2:True Positive / False Positive
         #Event 3:False Negative? 
         
         
         
         
         ) %>% 
    


## 5: SIMULATION ----






























