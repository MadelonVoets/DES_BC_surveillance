## 3. FUNCTIONS ----

#PATIENT AGE
#<60 0 | 60-69 1 | 70-79 2 | >= 80 3
fn_age <- function(n) {                           
  age <- rnorm(n, mean = 65, sd = 10)
  print(age)
  age.grp <- cut(age, breaks = c(-Inf, 60, 70, 80, Inf), labels = FALSE, right = FALSE) - 1 
  #-1 to have values 0,1,2,3 instead of 1,2,3,4
  return(age.grp)
}

#Patient Sex (100% women) 1 = female, 0 = male
fn_sex <- function() {
  sex <- ifelse(runif(1)<p.female, 1, 0);
  return(sex)
}

#MATCH patient vector to INFLUENCE matrix
fn_risk <- function(vector, matrix, rec) {
  matching_rows <- which(apply(matrix[, 1:10], 1, function(row) all(row == vector)))
  if (length(matching_rows) > 0) {
    matching_row <- matrix[matching_rows[1], ]
    if (rec == 1) {
      return(matching_row[23:27])
    } else if (rec == 2) {
      return(matching_row[28:32])
    } else {
      stop("Invalid value. It should be either 1 (LRR) or 2 (DM).")
    }
  } else {
    return(NULL) # No exact match found
  }
}
#When cumulative (13:17) and when conditional (23:27) risk?

#DETERMINE in which YEAR RECURRENCE occurs
#input risk_vector is output fn_risk()
fn_time_to_DM <- function(risk_vector) {
  # Generate a random number to determine if recurrence happens in any year
  yearly_risks <- risk_vector #* runif(5) #introduce extra randomness?
  
  # Check which year (if any) recurrence occurs
  recurrence_year <- which(yearly_risks > runif(1))
  
  if (length(recurrence_year) == 0) {
    # No recurrence during the follow-up period
    return(6)  # A value greater than 5 indicates no recurrence during follow-up
  } else {
    # Recurrence happens; return the first year of recurrence
    #return(min(recurrence_year)) #in years or in days?
    return(min(recurrence_year*365))
  }
}

#DETERMINE in which YEAR DM occurs
fn_time_to_DM <- function(risk_vector) {
  # Check which year (if any) recurrence occurs
  dm_year <- which(risk_vector > runif(1))
  
  if (length(dm_year) == 0) {
    # No dm during the follow-up period
    return(6)  # A value greater than 5 indicates no dm during follow-up
  } else {
    # DM happens; return the first year of recurrence
    #return(min(dm_year)) 
    return(min(dm_year*365)) #in years or in days?
  }
}

#Background mortality
fn_days_death_oc <- function(age, sex, mortality_data) {
  # Get the probability of dying for the given age and sex
  prob_death <- mortality_data[age-17, 2+sex]
  
  # Simulate death event based on probability
  death_event <- rbinom(1, 1, prob_death)
  
  if (death_event == 1) {
    # Individual dies
    return((age-18)*365)
  } else {
    # Individual survives, increment age and check again
    return(t_days_death(age + 1, sex, mortality_data))
  }
}

fn_time_to_events <- function(currentime, attrb) {
  # currenttime         simulation time
  # attrb               vector with times of death other causes, time of lrr, time of dm (time of bc death?)
  
  time_start <- attrb[1]
  time_of_death <- currenttime
  
  out <- c(time_start, time_of_death)
  
  return(out)
}

fn_gen_pt <- function(n.pat = 100, seed = 1){
  # Arguments:
  # n_pat: number of patients to generate
  # seed: seed: seed for the random number generator, default is 1
  # Returns: 
  # df_patient: data frame of sampled parameter values
  
  set.seed(seed) # set a seed to be able to reproduce the same results
  
  df_patient <- data.frame(
    
    #INFLUENCE Characteristics 
    ID = 1:n.pat,                              #n.pat number of individuals
    age = fn_age(n.pat)                           #distribution or sample?
    #grade = ,                                 #
    #stage = ,
    #nstatus = , 
    #multif = ,
    #sur = ,
    #chemo = , 
    #radio = , 
    #horm = ,
    #antiher2 = ,
    
  
  )
  
  return(df_patient)
}


#########################################################################

#DETERMINE the IMAGING MODALITY and associated COSTS
fn_img_mod <- function (){
  #mammo
  #mri
  #US
  #PET/CT
  out <- c(mod, cost)
  
  return(out)  
}

#DETERMINE the IMAGING event based on fn_img_mod
fn_img_event <- function() {
  out <-  ifelse(d_LRR == 1,
                 ifelse(runif(1) < p.sens.test, 1, 0),                   # 1 = true positive, 0 = false negative
                 ifelse(d_LRR == 0,
                        ifelse(runif(1) < (1 - p.spec.test), 1, 0),      # 1 = false positive, 0 = true negative
                        0)) 
  return(out)
}




