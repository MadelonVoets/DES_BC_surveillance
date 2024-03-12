## 3. FUNCTIONS ----

#PATIENT AGE
fn_age <- function() {                           
  age <- rnorm(n.i, mean=50, sd=10)
  
  return(age)
}

#MATCH patient vector to INFLUENCE matrix
fn_risk <- function(vector, matrix) {
  matching_rows <- which(apply(matrix[, 1:10], 1, function(row) all(row == vector)))
  if (length(matching_rows) > 0) {
    matching_row <- matrix[matching_rows[1], 23:27]
    return(matching_row)
  } else {
    return(NULL) # No exact match found
  }
}
#When cumulative (13:17) and when conditional (23:27) risk?

#DETERMINE in which YEAR RECURRENCE occurs
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


fn_time_to_events <- function(currentime, attrb) {
  # currenttime         simulation time
  # attrb               vector with times of death other causes, time of lrr, time of dm (time of bc death?)
  
  
  time_start <- attrb[1]
  time_of_death <- currenttime
  
  out <- c(time_start, time_of_death)
  
  return(out)
}