## 3. FUNCTIONS ----

## COMPLETED ###
#truncated normal distributions ----
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}

#Truncated normal sampler for vdt, a = min, b = max
fn_trnorm <- function(n, mean, sd, vdt_min = 1, vdt_max = Inf){
  qnorm(runif(n, pnorm(vdt_min, mean, sd), pnorm(vdt_max, mean, sd)), mean, sd)
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

## PATIENT CHARACTERISTICS ## ----
#<60 0 | 60-69 1 | 70-79 2 | >= 80 3
fn_age <- function(n.pat=1) {                           
  age <- rtnorm(n.pat, mean = m.age, sd = sd.age, a = 18, b = 100)
  age.grp <- cut(age, breaks = c(-Inf, 60, 70, 80, Inf), labels = FALSE, right = FALSE) - 1 
  #-1 to have values 0,1,2,3 instead of 1,2,3,4
  return(c(age, age.grp))
}
#Patient Sex (100% women) 1 = female, 0 = male
fn_sex <- function(n.pat=1) {
  sex <- ifelse(runif(n.pat)<p.female, 1, 0);
  return(sex)
}
#Tumour grade
fn_grade <- function(n.pat=1) {
  grade <- sample(0:2, n.pat, replace = TRUE, prob = c(p.gr1, p.gr2, p.gr3))
  return(grade)
}
#Tumour stage
fn_stage <- function(n.pat=1) {
  stage <- sample(0:2, n.pat, replace = TRUE, prob = c(p.st1, p.st2, p.st3))
    return(stage)
}
#Patient nodal status
fn_nstatus <- function(n.pat=1) {
  nstatus <- sample(0:3, n.pat, replace = TRUE, prob = c(p.n0, p.n1, p.n2, p.n3))
    return(nstatus)
}
#Tumour multifocality
fn_multif <- function(n.pat=1) {
  multif <- ifelse(runif(n.pat) < p.multi.y, 1, 0)
    return(multif)
}
#Primary surgery BCS or MST
fn_sur <- function(n.pat=1) {
  sur <- ifelse(runif(n.pat) < p.mst, 1, 0)
    return(sur)
}
#Chemotherapy yes or no
fn_chemo <- function(n.pat=1) {
  chemo <- ifelse(runif(n.pat) < p.chemo.y, 1, 0) 
  return(chemo)
}
#Radiotherapy yes or no
fn_radio <- function(sur, n.pat=1) {
  if (sur == 1) {
    radio <- ifelse(runif(n.pat) < p.mst.rt.y, 1, 0)
    } else if (sur == 0) {
    radio <- ifelse(runif(n.pat) < p.bsc.rt.y, 1, 0)
    } else {
    stop("Invalid value. No surgery defined")
  }
  return(radio)
}
#Hormonal status and therapy
fn_horm <- function(n.pat=1) {
  horm <- sample(0:2, n.pat, replace = TRUE, prob = c(p.hr.n, p.hr.y.ther.n, p.hr.y.ther.y))
    return(horm)
}
#HER2 status and therapy
fn_antiher2 <- function(n.pat=1) {
  antiher2 <- sample(0:2, n.pat, replace = TRUE, prob = c(p.her2.n, p.her2.y.ther.n, p.her2.y.ther.y))
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

## MORTALITY FUNCTIONS ## ----
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

#days until death whilst on non-curative treatment = BC DEATH
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

## VDT FUNCTIONS ## ----
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
fn_t <- function(V_t = V_d, V_0, vdt) {
  t <- vdt * log(V_t / V_0) / log(2)
  return(t)
}

## IMAGING FUNCTIONS ## ----
#DETERMINE the IMAGING MODALITY SENS & SPEC
fn_img_mod <- function(mod = 0){
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
  out <- c(mod, sens, spec)
  
  return(out)  
}

#DETERMINE the IMAGING event based on fn_img_mod
# 1 = TN
# 2 = Suspicion  / TP or FP 
# 3 = FN
fn_img_event <- function(V_0, t, vdt, sens, spec) {
  out <- if(fn_vdt(V_0, t, vdt) >= V_d){
    ifelse(runif(1) < sens, 2, 3)  # 2=suspicion after positive, 3 = false negative
  } else if(fn_vdt(V_0, t, vdt) < V_d){
    out <- ifelse(runif(1) < (1 - p.spec.test), 2, 1)      # 1 = false positive, 0 = true negative
  } else{
    return("Something's up - check")
  }
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
  mod <- 0
  cost <- c_pet(1)
  
  out <- c(mod, cost) 
  return(out)
}

## COST FUNCTION ## ----
#COST of imaging mammo, us and mri
fn_cost_img <- function(mod){
  #mammo = 0
  if(mod == 0){ #mammo=1
    cost <- c_mammo()
  } else if(mod == 1){ #US = 1
    cost <- c_us()
  } else if(mod == 2){ #MRI = 2
    cost <- c_mri()
  }

  return(cost)  
}

#cost of nc_treatment
#TO DO: for now kept oligo and non-oligo therapy separated in case of different costs
#TO DO: include timeout??
fn_cost_nc_treatment <- function(nc_ther) {
  # Initialize cost variable
  cost <- 0
  if (nc_ther == 0) {
    cost <- c_horm
  } else if (nc_ther == 1) {
    cost <- c_chemo + c_tar
  } else if (nc_ther == 2) {
    cost <- c_chemo
  } else if (nc_ther == 3) {
    cost <- c_tar
  } else if (nc_ther == 4) {
    cost <- 0
  } else if (nc_ther == 5) {
    cost <- c_horm + c_rt
  } else if (nc_ther == 6) {
    cost <- c_chemo + c_tar + c_rt
  } else if (nc_ther == 7) {
    cost <- c_chemo + c_rt
  } else if (nc_ther == 8) {
    cost <- c_tar + c_rt
  } else if (nc_ther == 9) {
    cost <- 0 + c_rt
  } else if (nc_ther == 10) {
    cost <- c_horm
  } else if (nc_ther == 11) {
    cost <- c_chemo + c_tar
  } else if (nc_ther == 12) {
    cost <- c_horm + c_rt
  } else if (nc_ther == 13) {
    cost <- c_chemo + c_tar + c_rt
  } else if (nc_ther == 14) {
    cost <- c_chemo
  } else if (nc_ther == 15) {
    cost <- c_chemo + c_rt
  } else {
    stop("Invalid input: should be value between 0 and 15")
  }
  return(cost)
}

## TREATMENT FUNCTIONS ## ----
#type, cost and timeout for non curative treatment
#TO DO INCLUDE costs
fn_nc_treatment <- function(her2, hr, type) {
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
  cost <- fn_cost_nc_treatment(ther)
  return(ther, cost)
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

## SYMPTOMATIC DISEASE ## ----
#time to symptoms
fn_days_symptomatic <- function(vdt, data, model, t = 0) {
  #time when tumour passes detection threshold and can become symptomatic
  dfi <- fn_t(V_d, V_0, vdt) 
  t_thres <- t + dfi
  #message(paste('ok made it this far with t=',t_thres))
  # Get the probability of dying for the given age and sex
  prob_symp <- fn_symp_prob(vdt, t_thres, data, model)
  # Simulate death event based on probability
  symp_event <- rbinom(1, 1, prob_symp)
  if (symp_event == 1) {
    # Individual dies
    return(t_thres)
  } else {
    # Individual has no symptoms yet, increment dfi and check again
    return(fn_days_symptomatic(vdt, data, model, t + 30))
  }
}

#probability of survival probability (remaining asymptomatic). 1-probability is probability of symptomatic disease at vdt,dfi
#dfi since diagnosis!! -> time when tumour passes the detection threshold
fn_symp_prob <- function(vdt,dfi, data, model){
  p <- 1- as.numeric(curve_cont(data=data, variable="vdt", model=model, horizon=c(vdt), times=c(dfi))[2])
  return(p)
}

# Function to determine if symptomatic detection or routine visit happens first
fn_detection_type_static <- function(dfi, t_symp) {
  if (dfi == 0) {
    return(0) # No recurrence detected
  } else {
    # Filter the visit times that are after dfi
    #static times
    valid_visits <- visit_times[visit_times > dfi]
    
    # If there are no valid visits, set closest_visit to Inf
    closest_visit <- if (length(valid_visits) == 0) Inf else min(valid_visits)
    
    # Determine if symptomatic detection or routine visit is closer
    if (t_symp < closest_visit) {
      return(1) # Symptomatic detection
    } else {
      return(2) # Routine visit
    }
  }
}

# Function to determine if symptomatic detection or routine visit happens first
fn_detection_type_dyn <- function(dfi, t_symp, mean = 15.5, sd = 10, n_visits = 6) {
  # Generate dynamic visit times
  original_visit_times <- c(0, 365, 730, 1095, 1460, 1825)
  visit_times <- original_visit_times + round(rtnorm(n_visits, mean = mean, sd = sd, a = 0, b = Inf))
  
  if (dfi == 0) {
    return(0) # No recurrence detected
  } else {
    # Filter the visit times that are after dfi
    #dystatic times
    valid_visits <- visit_times[visit_times > dfi]
    
    # If there are no valid visits, set closest_visit to Inf
    closest_visit <- if (length(valid_visits) == 0) Inf else min(valid_visits)
    
    # Determine if symptomatic detection or routine visit is closer
    if (t_symp < closest_visit) {
      return(1) # Symptomatic detection
    } else {
      return(2) # Routine visit
    }
  }
}

# Function to generate dynamic visit times
fn_dynamic_visits <- function(original_times, mean = 15.5, sd = 10, n = length(original_times)) {
  # Generate truncated normal samples
  trunc_norm_samples <- round(rtnorm(n, mean = mean, sd = sd, a = 0, b = Inf))
  
  # Add truncated normal samples to original visit times
  dynamic_visit_times <- original_times + trunc_norm_samples
  
  return(dynamic_visit_times)
}

#type, cost and timeout for non curative treatment - based on hr/her status
#TO DO INCLUDE costs oligo 
fn_nc_treatment2 <- function(her2, hr, type) {
  if (type==1) {
    #ther <- sample(0:4, 1, replace = TRUE, prob = c(p.o.s.horm, p.o.s.ch.tar, p.o.s.chemo, p.o.s.tar, p.o.s.n))
    #ther <- ifelse(runif(1)<p.o.l.rt, ther+5,ther) #plus local therapy
    prob_vector <- c(p.o.s.horm, p.o.s.ch.tar, p.o.s.chemo, p.o.s.tar, p.o.s.n)
    names(prob_vector) <- c("p.o.s.horm", "p.o.s.ch.tar", "p.o.s.chemo", "p.o.s.tar", "p.o.s.n")
    ther <- sample(names(prob_vector), 1, replace = TRUE, prob = prob_vector)
    if (runif(1) < p.o.l.rt) {
      ther <- append(ther, "p.o.l.rt")
    }
  } else { # type == 0
    if (her2 > 0) { # HER+
      if (hr > 0) { # HR+
        prob_vector <- c(p.pp.t, p.pp.c, p.pp.h, p.pp.rt, p.pp.s)
        names(prob_vector) <- c("p.pp.t", "p.pp.c", "p.pp.h", "p.pp.rt", "p.pp.s")
      } else { # HR-
        prob_vector <- c(p.mp.t, p.mp.c, p.mp.h, p.mp.rt, p.mp.s)
        names(prob_vector) <- c("p.mp.t", "p.mp.c", "p.mp.h", "p.mp.rt", "p.mp.s")
      }
    } else { # HER-
      if (hr > 0) { # HR+
        prob_vector <- c(p.pm.t, p.pm.c, p.pm.h, p.pm.rt, p.pm.s)
        names(prob_vector) <- c("p.pm.t", "p.pm.c", "p.pm.h", "p.pm.rt", "p.pm.s")
      } else { # HR-
        prob_vector <- c(p.mm.t, p.mm.c, p.mm.h, p.mm.rt, p.mm.s)
        names(prob_vector) <- c("p.mm.t", "p.mm.c", "p.mm.h", "p.mm.rt", "p.mm.s")
      }
    }
    ther <- names(prob_vector)[runif(length(prob_vector)) < prob_vector]
  }
  cost <- calculate_cost(ther, cost.vec.nc.treat)
  #return(list(ther = ther, cost = cost))
  return(list(ther, cost))
}

cost.vec.nc.treat <- list(
  #oligo
  p.o.s.horm.c = function() round(rtnorm(1, mean = 1, sd = 1, a = 0), digits = 2), 
  p.o.s.ch.tar.c = function() round(rtnorm(1, mean = 1, sd = 1, a = 0), digits = 2), 
  p.o.s.chemo.c = function() round(rtnorm(1, mean = 1, sd = 1, a = 0), digits = 2), 
  p.o.s.tar.c = function() round(rtnorm(1, mean = 1, sd = 1, a = 0), digits = 2), 
  p.o.s.n.c = function() round(rtnorm(1, mean = 1, sd = 1, a = 0), digits = 2),
  p.o.l.rt.c = function() round(rtnorm(1, mean = 1, sd = 1, a = 0), digits = 2),
  #non oligo
  #HR+ / HER2+
  p.pp.t.c = function() round(rtnorm(1, mean = 76588, sd = 56408, a = 0), digits = 2), 
  p.pp.c.c = function() round(rtnorm(1, mean = 8641, sd = 7852, a = 0), digits = 2), 
  p.pp.h.c = function() round(rtnorm(1, mean = 2912, sd = 5467, a = 0), digits = 2), 
  p.pp.rt.c = function() round(rtnorm(1, mean = 3648, sd = 3658, a = 0), digits = 2), 
  p.pp.s.c = function() round(rtnorm(1, mean = 1902, sd = 2230, a = 0), digits = 2),
  #HR- / HER2+
  p.mp.t.c = function() round(rtnorm(1, mean = 60350, sd = 58487, a = 0), digits = 2), 
  p.mp.c.c = function() round(rtnorm(1, mean = 7907, sd = 6495, a = 0), digits = 2), 
  p.mp.h.c = function() round(rtnorm(1, mean = 43, sd = 28, a = 0), digits = 2), 
  p.mp.rt.c = function() round(rtnorm(1, mean = 3910, sd = 3209, a = 0), digits = 2), 
  p.mp.s.c = function() round(rtnorm(1, mean = 3834, sd = 3652, a = 0), digits = 2),
  #HR+ / HER2-
  p.pm.t.c = function() round(rtnorm(1, mean = 34440, sd = 27406, a = 0), digits = 2), 
  p.pm.c.c = function() round(rtnorm(1, mean = 9808, sd = 8712, a = 0), digits = 2), 
  p.pm.h.c = function() round(rtnorm(1, mean = 2626, sd = 4656, a = 0), digits = 2), 
  p.pm.rt.c = function() round(rtnorm(1, mean = 3363, sd = 4361, a = 0), digits = 2), 
  p.pm.s.c = function() round(rtnorm(1, mean = 2617, sd = 2790, a = 0), digits = 2),
  #HR- / HER2-
  p.mm.t.c = function() round(rtnorm(1, mean = 37478, sd = 18775, a = 0), digits = 2), 
  p.mm.c.c = function() round(rtnorm(1, mean = 9809, sd = 8368, a = 0), digits = 2), 
  p.mm.h.c = function() round(rtnorm(1, mean = 13, sd = 9, a = 0), digits = 2), 
  p.mm.rt.c = function() round(rtnorm(1, mean = 3586, sd = 4174, a = 0), digits = 2), 
  p.mm.s.c = function() round(rtnorm(1, mean = 1780, sd = 2731, a = 0), digits = 2)
)


calculate_cost <- function(selected_parameters, cost.vec.nc.treat) {
  selected_parameters <- paste0(selected_parameters,".c")
  # Call the cost functions for the selected parameters
  selected_costs <- sapply(selected_parameters, function(param) cost.vec.nc.treat[[param]]())
  # Sum the costs
  total_cost <- sum(selected_costs, na.rm = TRUE)
  return(total_cost)
}

#TO DO costs uitzoeken
c_poli <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 120, sd = 0, a = 0), digits = 2) #BRON: kostenhandleiding ZIN Euro 2022
  return(cost)}

#cost conventional OR, per minute
c_or <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 11.09, sd = 0, a = 0), digits = 2) #BRON: kostenhandleiding ZIN Euro 2022
  return(cost)}

c_mammo <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 97.63, sd = 3.94, a = 0), digits = 2) #BRON: ZK23 & CZ24
  return(cost)
}
c_us <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 89.86, sd = 3.88, a = 0), digits = 2) #BRON: ZK23 & CZ24
  return(cost)
}
c_mri <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 295.52, sd = 9.55, a = 0), digits = 2) #BRON: ZK23 & CZ24
  return(cost)
}
c_pet_wb <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 1048.83, sd = 327.51, a = 0), digits = 2) #BRON: ZK23 & CZ24
  return(cost)
}
c_pet_pt <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 829.65, sd = 41.23, a = 0), digits = 2) #BRON: ZK23 & CZ24
  return(cost)
}
c_biopsy <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 161.40, sd = 4.89 , a = 0), digits = 2) #BRON: ZK23 & CZ24
  return(cost)
} #dependent on image-guided?

#treatment costs
c_horm <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0), digits = 2)
  return(cost)
}
c_radio <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0), digits = 2)
  return(cost)
}
c_chemo <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0), digits = 2)
  return(cost)
}
c_MST <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0), digits = 2)
  return(cost)
}
c_tar <- function(n.pat=1){
  cost <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0), digits = 2)
  return(cost)
}
#treatment timeouts
t_horm <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0))
  return(timeout)
}
t_radio <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0))
  return(timeout)
}
t_chemo <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0))
  return(timeout)
}
t_MST <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0))
  return(timeout)
}
t_tar <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 1, sd = 1, a = 0))
  return(timeout)
}


#### INCOMPLETE // WORK IN PROGRESS #####















      