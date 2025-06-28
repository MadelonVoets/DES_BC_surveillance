## 3. FUNCTIONS ----

#truncated normal distributions ----
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}

#Truncated normal sampler for vdt, a = min, b = max
fn_trnorm <- function(n, mean, sd, vdt_min = 1, vdt_max = Inf){
  qnorm(runif(n, pnorm(vdt_min, mean, sd), pnorm(vdt_max, mean, sd)), mean, sd)
}

#update to include impact QALYs, costs etc
fn_time_to_events <- function(currenttime, attrb) {
  # currenttime         simulation time
  # attrb               vector with times of death other causes, time of lrr, time of dm (time of bc death?)
  
  time_start <- attrb[1]
  time_of_death <- currenttime
  
  out <- c(time_of_death, time_start)
  
  return(out)
}

#determine beta distribution parameters - for utilities
beta_params <- function(mean, sd) {
  if (mean <= 0 || mean >= 1) {
    stop("Mean must be between 0 and 1 (exclusive).")
  }
  
  var <- sd^2
  max_var <- mean * (1 - mean)
  
  if (var >= max_var) {
    stop("Standard deviation is too large; it implies negative alpha/beta. Variance must be less than mean*(1 - mean).")
  }
  
  tmp <- mean * (1 - mean) / var - 1
  alpha <- mean * tmp
  beta <- (1 - mean) * tmp
  
  return(list(alpha = alpha, beta = beta))
}

#determine gamma distribution parameters - for costs
gamma_params <- function(mean, sd) {
  if (mean <= 0 || sd <= 0) {
    stop("Mean and standard deviation must both be positive.")
  }
  
  shape <- (mean / sd)^2
  rate <- mean / (sd^2)
  
  return(list(shape = shape, rate = rate))
}


## PATIENT CHARACTERISTICS ## ----
#<60 0 | 60-69 1 | 70-79 2 | >= 80 3
fn_age <- function(n.pat=1) {                           
  age <- round(rtnorm(n.pat, mean = m.age, sd = sd.age, a = 18, b = 100),1)
  age.grp <- cut(age, breaks = c(-Inf, 60, 70, 80, Inf), labels = FALSE, right = FALSE) - 1 
  #-1 to have values 0,1,2,3 instead of 1,2,3,4
  return(c(age, age.grp))
}
fn_age_gp <- function(age, n.pat=1) {
  age.grp <- cut(age, breaks = c(-Inf, 60, 70, 80, Inf), labels = FALSE, right = FALSE) - 1 
  #-1 to have values 0,1,2,3 instead of 1,2,3,4
  return(age.grp)
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
fn_days_death_oc <- function(age, sex, mortality_data, days_survived = 0) {
  # Get the probability of dying for the given age and sex
  if(age >= 100){
    prob_death <- mortality_data[82, 2+sex]
  } else{
    prob_death <- mortality_data[age-17, 2+sex]
  }
  # Simulate death event based on probability
  death_event <- rbinom(1, 1, prob_death)
  if (death_event == 1) {
    # Individual dies
    return(days_survived)
  } else {
    # Individual survives, increment age and check again
    return(fn_days_death_oc(age + 1, sex, mortality_data, days_survived+365))
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
#days until death after lrr diagnosis
fn_day_death_lrr <- function(type) {
    t <- rexp(1, rate = lrr_exp_rate)
    return(t)
    }

## VDT FUNCTIONS ## ----
#Determine min-max range for volume doubling time 
fn_minmax <- function(V_t, V_0, t_min, t_max) {
  vdt_min <- t_min * log(2) / log(V_t / V_0)
  vdt_min <- ifelse(vdt_min < 10, 10, vdt_min) #minimal 10 vdt
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
  if(vdt == 0){ #no recurrence, TP not possible
    out <- ifelse(runif(1) < (1 - spec), 2, 1) # 1 = true negative, 2 = suspicion/FP
    state <- 1
  } else if(fn_vdt(V_0, t, vdt) >= V_d){
    out <- ifelse(runif(1) < sens, 2, 3)  # 2=suspicion after positive/TP, 3 = false negative
    state <- 2
  } else if(fn_vdt(V_0, t, vdt) < V_d){
    out <- ifelse(runif(1) < (1 - spec), 2, 1)      # 1 = true negative, 2 = suspicion/FP
    state <- 3
  } else{
    return("Check - imaging event")
  }
  return(c(out, state))
}

#DETERMINE Additional imaging event - FN no longer possible 
# 1 = TN
# 2 = TP / FP
# Assumption: FN is not possible
fn_add_img_event <- function(sens, spec, state){
  # Generate a random number between 0 and 1
  rand_num <- runif(1)
  if(state == 1){ #no recurrence, TP not possible
    if(rand_num < (1 - spec)){
      event <- 2
      TF <- 1
    } else{
      event <- 1
      TF <- NA
    } 
    #VDT is not 0
  } else if (state == 2) {
    # True Positive
    event <- ifelse(rand_num < sens, 2, 1) #2 TP, 1 FN (TN arm)
    TF <- 2
  } else if (state == 3) {
    event <- ifelse(rand_num < (1-spec), 2, 1) #2 FP, 1 TN
    TF <- 1
  } else {
    return("Check - state unknown")
  }
  return(c(event, TF))
}

#BIOPSY result to distinguish FP and TP. Assumes 100% sensitivity
fn_biopsy <- function(TF){
  #1 = negative biopsy / FP
  #2 = positive biopsy / TP
  out <- ifelse(TF == 1, 1, 
                ifelse(TF == 2, 2, "Check - biopsy result"))
  return(out)
}

#Whole body imaging - decision between whole body and partial pet based on ..
fn_img_pet <- function() {
  #whole body imaging
  #pet wb = 3, pet pt =4
  mod <- 3 #wb
  #mod <- 4 #pt
  
  #if(mod == 3){
  #  cost <- c_pet_wb()
  #} else if(mod == 4){
  #  cost <- c_pet_pt()
  #}
  out <- mod 
  return(out)
}

## COST FUNCTION ## ----
#COST of imaging mammo, us and mri
fn_cost_img <- function(mod, psa){
  #mammo = 0
  if(mod == 0){ #mammo = 0
    cost <- c_mammo(psa = psa)
  } else if(mod == 1){ #US = 1
    cost <- c_us(psa = psa)
  } else if(mod == 2){ #MRI = 2
    cost <- c_mri(psa = psa)
  } else if(mod == 3){
    cost <- c_pet_wb(psa = psa)
  } else if(mod == 4){
    cost <- c_pet_pt(psa = psa)
  } else{
    stop("Invalid input: no imaging modality recognised")
  }
  return(cost)  
}

## TREATMENT FUNCTIONS ## ----
#type, cost and timeout for non curative treatment
#cost for treatment of recurrence
c_poli <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_poli_m, sd = c_poli_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_poli_m
    return(cost)
  }
}

c_or <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_or_m, sd = c_or_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_or_m
    return(cost)
  }
}

c_mammo <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_mammo_m, sd = c_mammo_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_mammo_m
    return(cost)
  }
}

c_us <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_us_m, sd = c_us_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_us_m
    return(cost)
  }
}

c_mri <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_mri_m, sd = c_mri_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_mri_m
    return(cost)
  }
}
c_pet_wb <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_pet_wb_m, sd = c_pet_wb_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_pet_wb_m
    return(cost)
  }
}
c_pet_pt <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_pet_pt_m, sd = c_pet_pt_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_pet_pt_m
    return(cost)
  }
}
c_biopsy <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_biopsy_m, sd = c_biopsy_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_biopsy_m
    return(cost)
  }
}
c_horm <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_horm_m, sd = c_horm_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_horm_m
    return(cost)
  }
}
c_radio <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_radio_m, sd = c_radio_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_radio_m
    return(cost)
  }
}
c_chemo <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_chemo_m, sd = c_chemo_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_chemo_m
    return(cost)
  }
}
c_MST <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_MST_m, sd = c_MST_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_MST_m
    return(cost)
  }
}
c_tar <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_tar_m, sd = c_tar_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_tar_m
    return(cost)
  }
}
c_dm <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- gamma_params(mean = c_dm_m, sd = c_dm_sd)
    cost <- round(rgamma(n = n.pat, shape = params$shape, rate = params$rate), digits = 2)
    return(cost)
  } else {
    cost <- c_dm_m
    return(cost)
  }
}

#Treatment after LRR diagnosis
fn_treatment <- function(horm, sur, chemo, psa=0) {
  #Date of biopsy to mastectomy: 2 to 6 weeks
  #Recovery from mastectomy alone: up to 3 weeks.
  #Length of post-surgery chemotherapy: 2 to 5 months.
  #Length of radiation therapy: 3 to 6.5 weeks (standard) or 5 days (brachytherapy)
  #2 years Horm  
  if (horm == 0) {
    if (sur == 1) {
      if (chemo == 0) {
        out <- 1 #"RC"
        cost <- c_radio(1,psa=psa) + c_chemo(1,psa=psa)  # You need to define cost_R and cost_C
        t <- t_radio(1) + t_chemo(1)
      } else {
        out <- 2 #"R"
        cost <- c_radio(1,psa=psa)
        t <- t_radio(1)
      }
    } else {
      if (chemo == 0) {
        out <- 3 #"MC"
        cost <- c_MST(1,psa=psa) + c_chemo(1,psa=psa)  # You need to define cost_M and cost_C
        t <- t_MST(1) + t_chemo(1)
      } else {
        out <- 4 #"M"
        cost <- c_MST(1,psa=psa)
        t <- t_MST(1)
      }
    }
  } else {
    if (sur == 1) {
      if (chemo == 0) {
        out <- 5 #"RCH"
        cost <- c_radio(1,psa=psa) + c_chemo(1,psa=psa) + c_horm(1,psa=psa)  # You need to define cost_R, cost_C, and cost_H
        t <- t_horm(1) #t_radio(1) + t_chemo(1) + 
      } else {
        out <- 6 #"RH"
        cost <- c_radio(1,psa=psa) + c_horm(1,psa=psa)
        t <- t_radio(1) + t_horm(1)
      }
    } else {
      if (chemo == 0) {
        out <- 7 #"MCH"
        cost <- c_MST(1,psa=psa) + c_chemo(1,psa=psa) + c_horm(1,psa=psa) # You need to define cost_M, cost_C, and cost_H
        t <- t_MST(1) + t_chemo(1) + t_horm(1)
      } else {
        out <- 8 #"MH"
        cost <- c_MST(1,psa=psa) + c_horm(1,psa=psa)
        t <- t_MST(1) + t_horm(1)
      }
    }
  }
  
  return(c(out,cost,t))
}

#cost for treatment DM
fn_trt_dm_c <- function(time){
  cost <- (time / 30.4) * c_dm(1,0)
}

#treatment timeouts
#hormonal therapy
t_horm <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 1400, sd = 1, a = 0))
  return(timeout)
}
#radiotherapy: 3 to 6 weeks
t_radio <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 31, sd = 6, a = 0))
  return(timeout)
}
#chemotherapy:3 to 6 months
t_chemo <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 126, sd = 20, a = 0))
  return(timeout)
}
#mastectomy: 
t_MST <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 25, sd = 2, a = 0))
  return(timeout)
}
t_tar <- function(n.pat=1){
  timeout <- round(rtnorm(n.pat, mean = 365, sd = 1, a = 0))
  return(timeout)
}

## UTILITY FUNCTIONS
u_dm_trt <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- beta_params(mean = u_dm_t_m, sd = u_dm_t_sd)
    util <- round(rbeta(n = n.pat, shape1 = params$alpha, shape = params$beta), digits = 2)
    return(util)
  } else {
    util <- u_dm_t_m
    return(util/365.25)
  }
}
#disease free
u_df <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- beta_params(mean = u_df_m, sd = u_df_sd)
    util <- round(rbeta(n = n.pat, shape1 = params$alpha, shape = params$beta), digits = 2)
    return(util)
  } else {
    util <- u_df_m
    return(util/365.25)
  }
}
#disutility function FP
u_dis <- function(n.pat=1, mod = 0, psa = 0){
  if(mod == 2){
    if (psa == 1){
      params <- beta_params(mean = u_FP_mri_m, sd = u_FP_mri_sd)
      util <- round(rbeta(n = n.pat, shape1 = params$alpha, shape = params$beta), digits = 2)
      return(util)
    } else {
      util <- u_FP_mri_m
      return(util/365.25)
    }
  } else {
    if (psa == 1){
      params <- beta_params(mean = u_FP_b_m, sd = u_FP_b_sd)
      util <- round(rbeta(n = n.pat, shape1 = params$alpha, shape = params$beta), digits = 2)
      return(util/365.5)
    } else {
      util <- u_FP_b_m
      return(util/365.5)
    }
  }
}
#disutility symptomatic detection
u_dis_symp <- function(n.pat=1, psa = 0){
  if (psa == 1){
    params <- beta_params(mean = u_sdet_m, sd = u_sdet_sd)
    util <- round(rbeta(n = n.pat, shape1 = params$alpha, shape = params$beta), digits = 2)
    return(util)
  } else {
    util <- u_sdet_m
    return(util/365.25)
  }
}

fn_u_treat <- function(trt, psa) {
  if (trt == 1 | trt == 2){
    if (psa == 1){
      params <- beta_params(mean = u_lrr_rt_m, sd = u_lrr_rt_sd)
      util <- round(rbeta(n = n.pat, shape1 = params$alpha, shape = params$beta), digits = 2)
      return(util/365.5)
    } else {
      util <- u_lrr_rt_m
      return(util/365.5)
    }
  } else if (trt == 3 | trt == 4){
    if (psa == 1){
      params <- beta_params(mean = u_lrr_mst_m, sd = u_lrr_mst_sd)
      util <- round(rbeta(n = n.pat, shape1 = params$alpha, shape = params$beta), digits = 2)
      return(util/365.5)
    } else {
      util <- u_lrr_mst_m
      return(util/365.5)
    }
  } else if (trt > 4){
    if (psa == 1){
      params <- beta_params(mean = u_lrr_h_m, sd = u_lrr_h_sd)
      util <- round(rbeta(n = n.pat, shape1 = params$alpha, shape = params$beta), digits = 2)
      return(util/365.5)
    } else {
      util <- u_lrr_h_m
      return(util/365.5)
    }
  }
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
  #prob_symp <- fn_symp_probl(vdt, t_thres, model)  #Penalised model
  #prob_symp <- prob_symp * 0.01
  # Simulate death event based on probability
  symp_event <- rbinom(1, 1, prob_symp*0.02)
  if (symp_event == 1) {
    # Individual dies
    return(t_thres)
  } else {
    # Individual has no symptoms yet, increment dfi and check again
    return(fn_days_symptomatic(vdt, data, model, t + 2))
  }
}

#probability of survival probability (remaining asymptomatic). 1-probability is probability of symptomatic disease at vdt,dfi
#dfi since diagnosis!! -> time when tumour passes the detection threshold
fn_symp_prob <- function(vdt,dfi, data, model){
  p <- 1- as.numeric(curve_cont(data=data, variable="vdt", model=model, horizon=c(vdt), times=c(dfi))[2])
  return(p)
}
































      