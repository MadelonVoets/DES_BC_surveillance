## 1: INITIALISATION ----

# Clear the workspace
rm(list=ls()); gc();

# Load the required packages, make sure the required packages are installed. 

# install.packages(c("simmer", "simmer.plot", "fitdistrplus")) # Install packages

library(simmer);
library(simmer.plot);
library(fitdistrplus);
library(flexsurv)  ;

# Set the working directory
setwd("")

# Load funtions for extracting monitored attributes
source("getSingleAttribute.R", echo=TRUE);
source("getMultipleAttributes.R", echo=TRUE);

# Load the dataset
load("");
n.i <- 100

## 2: PARAMETERS ----
# These are the parameters that define and are used in the trajectory. The prefix of the 
# parameter name indicates what type of parameter it is:
#   d_    distribution parameter
#   n_    count
#   p_    probability
#   t_    time duration
#   c_    cost
#   u_    utility

#### PATIENT INITIALIZATION #####
# AGE                               <60 0 | 60-69 1 | 70-79 2 | >= 80 4
# GRADE                             G1 0 | G2 1 | G3 2
# STAGE                             S1 0 | S2 1 | S3 2 
# NODAL                             N0 0 | N1 1 | N2 2 | N3 3
# MULTI                             No 0 | Yes 1
# SUR                               BCS 0 | MST 1
# CHEMO                             No 0 | Yes 1
# RADIO                             No 0 | Yes 1
# HORM                              HR- 0 | HR+ THER- 1 | HR+ THER+ 2
# ANTIHER2                          HER2- 0 | HER2+ THER- 1 | HER2+ THER+ 2

# Parameters for tumour characteristics // INFLUENCE parameters
p_grade     <- 
p_stage     <- 
p_nstatus   <- #stage 1/2: no N2 and N3
p_multif    <- 
p_sur       <- 
p_chemo     <- 
p_rad       <-  
p_hrstat    <- 
p_her2stat  <- 
  
#make patient characterics dataframe  
df.char <- data.frame(ID = 1:n.i)  #n.i number of individuals
df.char$age <- fn_age()  #distribution or sample?
df.char$grade <- 
df.char$stage <- 
df.char$nstatus <- 
df.char$multif <- 
df.char$sur <- 
df.char$chemo <- 
df.char$radio <- 
df.char$horm <- 
df.char$antiher2 <- 

# Parameter for background mortality (time to death other causes)
#time to death other causes
d_death_shape <- 
d_death_rate  <- 

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
  


## 3: FUNCTIONS ----
fn_age <- function() {                           
  age <- rnorm(n=1, mean=50, sd=10)
  
  return(age)
}

fn_risk <- function(vector, matrix) {
  matching_rows <- which(apply(matrix[, 1:5], 1, function(row) all(row == vector)))
  if (length(matching_rows) > 0) {
    matching_row <- matrix[matching_rows[1], 6:10]
    return(matching_row)
    } else {
      return(NULL) # No exact match found
      }
}

fn_recurrence_year <- function(patient_vector) {
  annual_risk_vector <- patient_vector
  
  # Generate a random number to determine if recurrence happens in any year
  yearly_risks <- annual_risk_vector #* runif(5) #introduce extra randomness?
  
  # Check which year (if any) recurrence occurs
  recurrence_year <- which(yearly_risks > runif(1))
  
  if (length(recurrence_year) == 0) {
    # No recurrence during the follow-up period
    return(6)  # A value greater than 5 indicates no recurrence during follow-up
  } else {
    # Recurrence happens; return the first year of recurrence
    return(min(recurrence_year))
  }
}

fn_img_event <- function() {
  out <-  ifelse(d_LRR == 1,
                 ifelse(runif(1) < p.sens.test, 1, 0),                   # 1 = true positive, 0 = false negative
                 ifelse(d_LRR == 0,
                        ifelse(runif(1) < (1 - p.spec.test), 1, 0),      # 1 = false positive, 0 = true negative
                        0)) 
  return(out)
}

fn_img_mod <- function (){
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
    values = function() fn_img_mod(at = now(.env = sim)),
    mod = '+'
  ) %>%
  timeout(task = t_fup) %>%
  release(resource = "Imaging")

  #first branch, based on what the outcome is of the imaging event
  branch(option = function() fn_img_event(get_attribute(sim, "event")), continue = c(T,T,T,F,T), 
         #how to handle additional imaging event?
         #how to handle adherence? 
         
         #Event 1: True Negative
         #Event 2: True Positive / False Positive
         #Event 3: False Negative? 
         #Event 4: Additional imaging?
         
         
         
         
         ) %>% 
    


## 5: SIMULATION ----






























