## 3. FUNCTIONS ----

## COMPLETED ###
#truncated normal distributions
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}

#update to include impact QALYs, costs etc
fn_time_to_events <- function(currentime, attrb) {
  # currenttime         simulation time
  # attrb               vector with times of death other causes, time of lrr, time of dm (time of bc death?)
  
  time_start <- attrb[1]
  time_of_death <- currenttime
  
  out <- c(time_of_death-time_start)
  
  return(out)
}

#Patient characteristics
#<60 0 | 60-69 1 | 70-79 2 | >= 80 3
fn_age <- function() {                           
  age <- rtnorm(1, mean = m.age, sd = sd.age, a = 18, b = 100)
  age.grp <- cut(age, breaks = c(-Inf, 60, 70, 80, Inf), labels = FALSE, right = FALSE) - 1 
  #-1 to have values 0,1,2,3 instead of 1,2,3,4
  return(c(age, age.grp))
}
#Patient Sex (100% women) 1 = female, 0 = male
fn_sex <- function() {
  sex <- ifelse(runif(1)<p.female, 1, 0);
  return(sex)
}
#Tumour grade
fn_grade <- function() {
  grade <- sample(0:2, 1, replace = TRUE, prob = c(p.gr1, p.gr2, p.gr3))
  return(grade)
}
#Tumour stage
fn_stage <- function() {
  stage <- sample(0:2, 1, replace = TRUE, prob = c(p.st1, p.st2, p.st3))
    return(stage)
}
#Patient nodal status
fn_nstatus <- function() {
  nstatus <- sample(0:3, 1, replace = TRUE, prob = c(p.n0, p.n1, p.n2, p.n3))
    return(nstatus)
}
#Tumour multifocality
fn_multif <- function() {
  multif <- ifelse(runif(1) < p.multi.y, 1, 0)
    return(multif)
}
#Pirmary surgery BCS or MST
fn_sur <- function() {
  sur <- ifelse(runif(1) < p.mst, 1, 0)
    return(sur)
}
#Chemotherapy yes or no
fn_chemo <- function() {
  chemo <- ifelse(runif(1) < p.chemo.y, 1, 0) 
  return(chemo)
}
#Radiotherapy yes or no
fn_radio <- function() {
  radio <- ifelse(runif(1) < p.rt.y, 1, 0) 
    return(radio)
}
#Hormonal status and therapy
fn_horm <- function() {
  horm <- sample(0:2, 1, replace = TRUE, prob = c(p.hr.n, p.hr.y.ther.n, p.hr.y.ther.y))
    return(horm)
}
#HER2 status and therapy
fn_antiher2 <- function() {
  antiher2 <- sample(0:2, 1, replace = TRUE, prob = c(p.her2.n, p.her2.y.ther.n, p.her2.y.ther.y))
    return(antiher2)
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
      stop("Invalid value. Type should be either 1 (LRR) or 2 (DM).")
    }
  } else {
    return(NULL) # No exact match found - which should not be possible
  }
}

#DETERMINE in which YEAR DM/LRR occurs
fn_t_to_tumour <- function(risk_vector) {
  # Check which year (if any) recurrence occurs
  recurrence_year <- which(risk_vector > runif(1))
  if (length(recurrence_year) == 0) {
    # No dm/lrr during the follow-up period
    return(0)  # A value greater than 5 indicates no dm during follow-up
  } else {
    # DM happens; return the first year of recurrence
    return(min(recurrence_year*365)) #return in days
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
    return(fn_days_death_oc(age + 1, sex, mortality_data))
  }
}

#Truncated normal sampler for vdt, a = min, b = max
fn_trnorm <- function(n, mean, sd, vdt_min = 1, vdt_max = Inf){
  qnorm(runif(n, pnorm(vdt_min, mean, sd), pnorm(vdt_max, mean, sd)), mean, sd)
}

#Determine min-max range for volume doubling time 
fn_minmax <- function(V_t, V_0, t_min, t_max) {
  vdt_min <- t_min * log(2) / log(V_t / V_0)
  vdt_max <- t_max * log(2) / log(V_t / V_0)
  return(c(vdt_min, vdt_max))
}

#Determine tumour volume at time t
fn_vdt <- function(V_0, t, vdt) {
  V_t <- V_0 * 2^(t / vdt)
  return(V_t)
}

#Solve for t - determine t tumour passes detection threshold
fn_t <- function(V_t, V_0, vdt) {
  t <- vdt * log(V_t / V_0) / log(2)
  return(t)
}

#DETERMINE the IMAGING MODALITY SENS & SPEC
fn_img_mod <- function (mod = 0){
  #mammo = 0
  if(mod == 0){
    sens <- p.sens.mammo
    spec <- p.spec.mammo
  } else if(mod == 1){ #US = 1
    sens <- p.sens.us
    spec <- p.spec.us
  } else if(mod == 2){ #MRI = 2
      sens <- p.sens.mri
      spec <- p.spec.mri
    }
  out <- c(mod, cost, sens, spec)
  
  return(out)  
}

#DETERMINE the IMAGING event based on fn_img_mod
# 1 = TN
# 2 = Suspicion  / TP or FP 
# 3 = FN
fn_img_event <- function(V_0, t, vdt, sens, spec) {
  out <- if(fn_vdt(V_0, t, vdt) >= V_d){
    ifelse(runif(1) < sens, 2, 3)  # 1 = true positive, 0 = false negative
  } else if(fn_vdt(V_0, t, vdt) < V_d){
    out <- ifelse(runif(1) < (1 - p.spec.test), 2, 1)      # 1 = false positive, 0 = true negative
  } else{
    return("Something's up - check")
  }
  return(out)
}

#COST of imaging mammo, us and mri
fn_cost_img <- function(mod){
  #mammo = 0
  if(mod == 1){
    cost <- c_mammo(1)
  } else if(mod == 2){ #US = 1
    cost <- c_us(1)
  } else if(mod == 3){ #MRI = 2
    cost <- c_mri(1)
  }
  out <- c(cost)
  
  return(out)  
}

#DETERMINE Additional imaging event - FN no longer possible 
# 1 = TN
# 2 = TP / FP
# Assumption: FN is not possible
fn_add_img_event <- function(sens, spec){
  # Generate a random number between 0 and 1
  rand_num <- runif(1)
  if (rand_num < sens) {
    # True Positive
    return(2)
  } else if (rand_num >= spec) {
    # True Negative
    return(1)
  } else {
    # False Positive
    return(2)
  }
}

#BIOPSY result to distinguish FP and TP. Assumes 100% sensitivity
fn_biopsy <- function(result = 1){
  #1 = negative biopsy / FP
  #2 = positive biopsy / TP
  out <- ifelse(result == 1, 1, 
                ifelse(result == 2, 2, "No correct biopsy result - CHECK"))
  return(out)
}

#Whole body imaging - include mammo as well?
fn_img_wb <- function() {
  #whole body imaging
  out <- c_pet(1)
  return(out)
}

#Treatment after LRR diagnosis
fn_treatment <- function(horm, sur, chemo) {
  #Date of biopsy to mastectomy: 2 to 6 weeks
  #Recovery from mastectomy alone: up to 3 weeks.
  #Length of post-surgery chemotherapy: 2 to 5 months.
  #Length of radiation therapy: 3 to 6.5 weeks (standard) or 5 days (brachytherapy)
  #2 years Horm  
  if (horm == 0) {
    if (sur == 1) {
      if (chemo == 0) {
        out <- 1 #"RC"
        cost <- c_radio(1) + c_chemo(1)  # You need to define cost_R and cost_C
        t <- t_radio(1) + t_chemo(1)
      } else {
        out <- 2 #"R"
        cost <- c_radio(1)
        t <- t_radio(1)
      }
    } else {
      if (chemo == 0) {
        out <- 3 #"MC"
        cost <- c_MST(1) + c_chemo(1)  # You need to define cost_M and cost_C
        t <- t_MST(1) + t_chemo(1)
      } else {
        out <- 4 #"M"
        cost <- c_MST(1)
        t <- t_MST(1)
      }
    }
  } else {
    if (sur == 1) {
      if (chemo == 0) {
        out <- 5 #"RCH"
        cost <- c_radio(1) + c_chemo(1) + c_horm(1)  # You need to define cost_R, cost_C, and cost_H
        t <- t_radio(1) + t_chemo(1) + t_horm(1) 
      } else {
        out <- 6 #"RH"
        cost <- c_radio(1) + c_horm(1)
        t <- t_radio(1) + t_horm(1)
      }
    } else {
      if (chemo == 0) {
        out <- 7 #"MCH"
        cost <- c_MST(1) + c_chemo(1) + c_horm(1) # You need to define cost_M, cost_C, and cost_H
        t <- t_MST(1) + t_chemo(1) + t_horm(1)
      } else {
        out <- 8 #"MH"
        cost <- c_MST(1) + c_horm(1)
        t <- t_MST(1) + t_horm(1)
      }
    }
  }
  
  return(c(out,cost,t))
}

#days until death whilst on non-curative treatment
fn_day_death_bc <- function(type) {
  if(type == 1){
    t <- rweibull(1, shape = oligo.scale, scale = exp(oligo.coef))
  } else if(type == 2){
    t <- rweibull(1, shape = dm.scale, scale = exp(dm.coef))
  } else {
    return("Something's up - check")
  }
  return(t)
}

#### INCOMPLETE #####

#type, cost and timeout for non curative treatment
#TO DO INCLUDE time and costs
fn_nc_treatment <- function(her2, hr) {
  if (type==1) {
    ther <- sample(0:4, 1, replace = TRUE, prob = c(p.o.s.horm, p.o.s.ch.tar, p.o.s.chemo, p.o.s.tar, p.o.s.n))
    ther <- ifelse(runif(1)<p.o.l.rt, ther+5,ther) #plus local therapy
  } else{
    if(her2 > 0){
      if(hr >0){  
        ther <- ifelse(runif(1)<p.s.horm, 10, 11) #10 = horm, 11= target + chemo
        ther <- ifelse(runif(1)<p.l.rt, ther+2,ther)#plus local therapy 12/13
      } else{
        ther <- 11 #target+chemo
        ther <- ifelse(runif(1)<p.l.rt, ther+2,ther) #plus local therapy (13)
      }
    } else{
      ther <- 14 #chemo
      ther <- ifelse(runif(1)<p.l.rt, ther+1,ther) #plus local therapy
    }
  }
  cost <- 0
  return(ther, cost)
}

#probability of survival probability (remaining asymptomatic). 1-probability is probability of symptomatic disease at vdt,dfi
#dfi since diagnosis!! -> time when tumour passes the detection threshold
fn_symp_prob <- function(vdt,dfi){
  p <- 1- as.numeric(curve_cont(data=df_patient, variable="vdt", model=model, horizon=c(vdt), times=c(dfi))[2])
  return(p)
}



