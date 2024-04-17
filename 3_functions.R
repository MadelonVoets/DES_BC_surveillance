## 3. FUNCTIONS ----
#truncated normal distributions
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}

#PATIENT AGE
#<60 0 | 60-69 1 | 70-79 2 | >= 80 3
fn_age <- function(n) {                           
  age <- rtnorm(n, mean = m.age, sd = sd.age, a = 18, b = 100)
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
fn_risk <- function(vector, matrix = inf_matrix, rec) {
  matching_rows <- which(apply(matrix[, 1:10], 1, function(row) all(row == vector)))
  if (length(matching_rows) > 0) {
    matching_row <- matrix[matching_rows[1], ]
    if (rec == 1) {
      return(matching_row[13:17])
    } else if (rec == 2) {
      return(matching_row[18:22])
    } else {
      stop("Invalid value. It should be either 1 (LRR) or 2 (DM).")
    }
  } else {
    return(NULL) # No exact match found
  }
}
#When cumulative (13:17) and when conditional (23:27) risk? (LRR)

#DETERMINE in which YEAR RECURRENCE occurs
#input risk_vector is output fn_risk()
# fn_time_to_LRR <- function(risk_vector) {
#   # Generate a random number to determine if recurrence happens in any year
#   yearly_risks <- risk_vector #* runif(5) #introduce extra randomness?
#   
#   # Check which year (if any) recurrence occurs
#   recurrence_year <- which(yearly_risks > runif(1))
#   
#   if (length(recurrence_year) == 0) {
#     # No recurrence during the follow-up period
#     return(0)  # A value greater than 5 indicates no recurrence during follow-up
#   } else {
#     # Recurrence happens; return the first year of recurrence
#     #return(min(recurrence_year)) #in years or in days?
#     return(min(recurrence_year*365))
#   }
# }

#DETERMINE in which YEAR DM occurs
fn_t_to_tumour <- function(risk_vector) {
  # Check which year (if any) recurrence occurs
  dm_year <- which(risk_vector > runif(1))
  
  if (length(dm_year) == 0) {
    # No dm during the follow-up period
    return(0)  # A value greater than 5 indicates no dm during follow-up
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

#Truncated normal sampler a = min, b = max
#mean.nrom.vdt
#sd.norm.vdt
fn_trnorm <- function(n, mean, sd, vdt_min = 1, vdt_max = Inf){
  qnorm(runif(n, pnorm(vdt_min, mean, sd), pnorm(vdt_max, mean, sd)), mean, sd)
}

#volume doubling time 
fn_minmax <- function(V_t, V_0, t_min, t_max) {
  vdt_min <- t_min * log(2) / log(V_t / V_0)
  vdt_max <- t_max * log(2) / log(V_t / V_0)
  return(c(vdt_min, vdt_max))
}

fn_vdt <- function(V_0, t, vdt) {
  V_t <- V_0 * 2^(t / vdt)
  return(V_t)
}

fn_t <- function(V_t, V_0, vdt) {
  t <- vdt * log(V_t / V_0) / log(2)
  return(t)
}

fn_gen_pt <- function(n.pat = 100, seed = 1){
  # Arguments:
  # n_pat: number of patients to generate
  # seed: seed: seed for the random number generator, default is 1
  # Returns: 
  # df_patient: data frame of sampled parameter values
  
  set.seed(seed) # set a seed to be able to reproduce the same results
  age <- fn_age(n.pat)                                                                      #distribution or sample?
  grade <- sample(0:2, n.pat, replace = TRUE, prob = c(p.gr1, p.gr2, p.gr3))          
  stage <- sample(0:2, n.pat, replace = TRUE, prob = c(p.st1, p.st2, p.st3))
  nstatus <- sample(0:3, n.pat, replace = TRUE, prob = c(p.n0, p.n1, p.n2, p.n3))
  multif <- ifelse(runif(n.pat) < p.multi.y, 1, 0)
  sur <- ifelse(runif(n.pat) < p.mst, 1, 0)
  chemo <- ifelse(runif(n.pat) < p.chemo.y, 1, 0) 
  radio <- ifelse(runif(n.pat) < p.rt.y, 1, 0) 
  horm <- sample(0:2, n.pat, replace = TRUE, prob = c(p.hr.n, p.hr.y.ther.n, p.hr.y.ther.y))
  antiher2 <- sample(0:2, n.pat, replace = TRUE, prob = c(p.her2.n, p.her2.y.ther.n, p.her2.y.ther.y))
  

  df_patient <- data.frame(
    #INFLUENCE Characteristics 
    ID = 1:n.pat,                                                                             #n.pat number of individuals
    age = age,
    grade = grade,
    stage = stage,
    nstatus = nstatus,
    multif = multif,
    sur = sur,
    chemo = chemo,
    radio = radio,
    horm = horm,
    antiher2 = antiher2
  )
    
  #MODEL parameters
  #Time to DM based on INFLUENCE risk vector
  df_patient$t.DM = apply(df_patient, 1, function(row) {
    vector <- c(row["age"], row["grade"], row["stage"], row["nstatus"], row["multif"], row["sur"], row["chemo"], row["radio"], row["horm"], row["antiher2"])
    result <- fn_t_to_tumour(fn_risk(vector, inf_matrix, 2))
    return(result)
  })
  #Time to LRR based on INFLUENCE risk vector
  df_patient$t.LRR = apply(df_patient, 1, function(row) {
    vector <- c(row["age"], row["grade"], row["stage"], row["nstatus"], row["multif"], row["sur"], row["chemo"], row["radio"], row["horm"], row["antiher2"])
    result <- fn_t_to_tumour(fn_risk(vector, inf_matrix, 1))
    return(result)
  })
  #VDT LLR & DM
  df_patient$vdt_lrr <- apply(df_patient, 1, function(row) {
    t_LRR <- row["t.LRR"]
    if (t_LRR == 0) {
      vdt <- 0
    } else {
      vdt <- fn_trnorm(1, mean.norm.vdt, sd.norm.vdt, fn_minmax(V_d, V_0, t_LRR - 365, t_LRR)[1], fn_minmax(V_d, V_0, t_LRR - 365, t_LRR)[2]) 
      if (vdt < 1){
        vdt <- 0
      }
    }
    return(vdt)
  })
  
  df_patient$vdt_dm <- apply(df_patient, 1, function(row) {
    t_DM <- row["t.DM"]
    if (t_DM == 0) {
      vdt <- 0
    } else {
      vdt <- fn_trnorm(1, mean.norm.vdt, sd.norm.vdt, fn_minmax(V_d, V_0, t_DM - 365, t_DM)[1], fn_minmax(V_d, V_0, t_DM - 365, t_DM)[2]) 
      if (vdt < 1){
        vdt <- 0
      }
    }
    return(vdt)
  })
  
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




