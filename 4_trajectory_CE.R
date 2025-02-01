#Health utilities
u_DF <- rtnorm(1, 0.824, 0.168, a = 0, b=1)/365; #TN average of T0 0.852 T6 0.799 T12 0.820 (Cancers 2024)
fn_udf <- function(n.pat=1) {
  udf <- rtnorm(1, 0.824, 0.168, a = 0, b=1)/365;
  return(udf)
}
#D1. False positive for breast cancer; no biopsy or MRI N 102 − 0.031 0.049 − 0.041 to − 0.021
#D2. False positive for breast cancer; biopsy performed 102 − 0.058 0.079 − 0.073 to − 0.042 <----------
#D3. False positive for breast cancer; MRI performed 102 − 0.067 0.083 − 0.083 to − 0.050
#99% cofidence interval  lower bound=mean−(2.575∗X) = -0.261425, upper bound = 0.145425
u_disFP <- rtnorm(1, -0.058, 0.079, a = -0.261425, b=0)/365;
u_TP <- rtnorm(1, 0.73, 0.02, a = 0, b=1)/365; #EBC-local recurrence first year 0.73 +- 0.02; #EBC-local recurrence >first year 0.71 +- 0.09
u_DM <- rtnorm(1, 0.58, 0.06, a = 0, b=1)/365; #EBC-DM first year 0.58 +- 0.06; EBC-DM >first year 0.60 +- 0.05 

u_disSUS <- rtnorm(1, -0.0, 0.0, a = -0, b=0)/365;
u_disFN <- rtnorm(1, 0, 0.0, a = 0, b=1)/365 #FALSE NEGATIVE
#treatments LRR
u_lrr.ch <- 0.77 #chemotherapy - recurrence
u_lrr.horm <- 0.82 #hrt at least 1 year after primary diagnosis / second and following years after primary BC
u_lrr.mst <- rtnorm(1, 0.84, 0.18, a = 0, b=1) #SG Mastectomy after breast conserving surgery
U_lrr.rt <- 0.76 #radiation therapy within 6 months of diagnosis (primary?)
#treatments DM
u_dm.ch <- 0.69 #chemotherapy - DM
u_DMt <- rtnorm(1, 0.69, 0.26, a = 0, b=1)/365; #MBC DM on treatment
u_DMp <- rtnorm(1, 0.55, 0.08, a = 0, b=1)/365; #MBC DM progressive disease



########################################
out.trj <- trajectory() %>% 
  set_attribute(
    keys   = c("time_to_death", "start"), values = function() fn_time_to_events(currenttime = now(.env = sim), attrb = get_attribute(sim, "start.sim")))  

# treatment (symptomatic) 
treat.trj <- trajectory() %>%
  set_attribute(key="treat.s", values = function() now(.env = sim)) %>%    
  set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
  set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
  set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #pathological confirmation?
  #seize(resource = "LRR.Treatment") %>%
  set_attribute(keys = "LRR_treat_st", values = function() now(.env = sim)) %>%              
  set_attribute(keys=c("LRR_treat", "cost.treat", "t_treat"), values=function() fn_treatment(horm = get_attribute(sim, "Horm"), sur = get_attribute(sim, "Sur"),chemo = get_attribute(sim, "Chemo")), mod = "+") %>%
  timeout(task = function() (get_attribute(sim, "t_treat"))) %>%
  set_attribute(keys = "cost", values = function() get_attribute(sim, "cost.treat"), mod = "+") %>%        
  #utility for treatment        
  set_attribute(key="QoL", value = function() fn_u_treat(trt = get_attribute(sim, "LRR_treat"))) %>%  
  set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "t_treat")) %>%         
  #release(resource = "LRR.Treatment") %>%
  set_attribute(keys = "LRR_treat_end", values = function() now(.env = sim)) %>% 
  set_attribute(keys = "End.Surveillance", values = function() now(.env = sim)) %>% 
  join(out.trj)  #endpoint, no going back into surveillance            
  #timeout(task = function() (round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380)) - get_attribute(sim, "t_treat"))) %>%              
  #set_attribute(key="Age", value=function() get_attribute(sim, "Age")+get_attribute(sim, "surv.year")) %>%
  #set_attribute(key="A.grp", values=function() fn_age_gp(age = get_attribute(sim, "Age"))) %>%
  #rollback(target=20, times=Inf)  
                
# treatment (after detection) 
treat.d.trj <- trajectory() %>%
  set_attribute(key="treat.d", values = function() now(.env = sim)) %>%
  #seize(resource = "LRR.Treatment") %>%
  set_attribute(keys = "LRR_treat_st", values = function() now(.env = sim)) %>%               
  set_attribute(keys=c("LRR_treat", "cost.treat", "t_treat"), values=function() fn_treatment(horm = get_attribute(sim, "Horm"),sur = get_attribute(sim, "Sur"),chemo = get_attribute(sim, "Chemo")), mod = "+") %>%
  timeout(task = function() (get_attribute(sim, "t_treat"))) %>%
  set_attribute(keys = "cost", values = function() get_attribute(sim, "cost.treat"), mod = "+") %>%
  set_attribute(key="QoL", value = function() fn_u_treat(trt = get_attribute(sim, "LRR_treat"))) %>%        
  set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "t_treat")) %>%         
  #release(resource = "LRR.Treatment") %>%
  set_attribute(keys = "LRR_treat_end", values = function() now(.env = sim)) %>%
  set_attribute(keys = "End.Surveillance", values = function() now(.env = sim)) %>% 
  join(out.trj)  #endpoint, no going back into surveillance               
  #timeout(task = function() (round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380)) - get_attribute(sim, "t_treat"))) %>%              
  #set_attribute(key="Age", value=function() get_attribute(sim, "Age")+get_attribute(sim, "surv.year")) %>%
  #set_attribute(key="A.grp", values=function() fn_age_gp(age = get_attribute(sim, "Age"))) %>%
  #rollback(target=30, times=Inf)              
                                
#symptomatic DM INCOMPLETE
dm.trj <- trajectory() %>% 
  set_attribute(key="type.dm", value = function() ifelse(runif(1) < p.oligo, 1, 2)) %>% #1 oligo , 2 dm
  #seize(resource = "WB.Imaging") %>%
  set_attribute(keys = "WB_img_st", values = function() now(.env = sim)) %>% 
  set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
  set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
  set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #pathological confirmation?
  timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between screening and result #TO DO VERIFY
  #release(resource = "WB.Imaging") %>%
  set_attribute(keys = "WB_img_end", values = function() now(.env = sim)) %>%         
  #non-curative / palliative care
  #seize(resource = "NC.treatment") %>%
  set_attribute(keys = "NC_treat_st", values = function() now(.env = sim)) %>%
  renege_in(t = function() now(.env = sim) + fn_day_death_bc(type = get_attribute(sim, "type.dm")), 
            out = trajectory() %>% set_attribute(key="E", mod="+", value=function() (u_DMp*(now(.env = sim)-get_attribute(sim, "NC_treat_st")))) %>% set_attribute(key="Death_BC", value=1)) %>%              
  set_attribute(keys = "cost", values = function() fn_nc_treatment2(her2 = get_attribute(sim, "Antiher2"), hr = get_attribute(sim,"Horm"), type = get_attribute(sim,"type.dm"))[[2]],
                mod = "+" ) %>%
  set_attribute(keys = "ther",values = function() fn_nc_treatment2(her2 = get_attribute(sim, "Antiher2"), hr = get_attribute(sim,"Horm"), type = get_attribute(sim,"type.dm"))[[1]]) %>%              
  timeout(task = function() 1) %>% #TO DO VERIFY fn_nc_treatment
  #release(resource = "NC.treatment") %>%
  set_attribute(keys = "NC_treat_end", values = function() now(.env = sim)) %>%         
  wait() #wait until patient dies, either background mortality or bc death  
                                            
# MAIN TRAJECTORY 
fup.trj <- trajectory() %>% 
  #patient characteristics
  set_attribute(key="Sex", value=function() fn_sex()) %>%
  set_attribute(key=c("Age","A.grp"), value=function() fn_age()) %>%
  set_attribute(key="Grade", value=function() fn_grade()) %>%
  set_attribute(key="Stage", value=function() fn_stage()) %>%
  set_attribute(key="Nstatus", value=function() fn_nstatus()) %>%
  set_attribute(key="Multif", value=function() fn_multif()) %>%
  set_attribute(key="Sur", value=function() fn_sur()) %>%
  set_attribute(key="Chemo", value=function() fn_chemo()) %>%
  set_attribute(key="Radio", value=function() fn_radio(sur=get_attribute(sim, "Sur"))) %>%
  set_attribute(key="Horm", value=function() fn_horm()) %>%
  set_attribute(key="Antiher2", value=function() fn_antiher2()) %>%
  set_attribute(key="surv.year", value = 0) %>%
  set_attribute(key="LRR_treat_end", value = 0) %>%
  set_attribute(key = "E", value = 0) %>%    #added          
  #time to localregional recurrence and/or distant metastasis
  set_attribute(key="t_LRR", value=function() fn_t_to_tumour(fn_risk(vector = c(get_attribute(sim, "A.grp"),
                                                                                get_attribute(sim, "Grade"),
                                                                                get_attribute(sim, "Stage"),
                                                                                get_attribute(sim, "Nstatus"),
                                                                                get_attribute(sim, "Multif"),
                                                                                get_attribute(sim, "Sur"),
                                                                                get_attribute(sim, "Chemo"),
                                                                                get_attribute(sim, "Radio"),
                                                                                get_attribute(sim, "Horm"),
                                                                                get_attribute(sim, "Antiher2")), rec=1))) %>%
  set_attribute(key="t_DM", value=function() fn_t_to_tumour(fn_risk(vector = c(get_attribute(sim, "A.grp"),
                                                                               get_attribute(sim, "Grade"),
                                                                               get_attribute(sim, "Stage"),
                                                                               get_attribute(sim, "Nstatus"),
                                                                               get_attribute(sim, "Multif"),
                                                                               get_attribute(sim, "Sur"),
                                                                               get_attribute(sim, "Chemo"),
                                                                               get_attribute(sim, "Radio"),
                                                                               get_attribute(sim, "Horm"),
                                                                               get_attribute(sim, "Antiher2")), rec=2))) %>%
  set_attribute(keys = "start.sim", values = function() now(.env = sim)) %>%  
  set_attribute(key="QoL", value = u_DF) %>% #added utility disease free - dependent on if someone develops recurrence or not?              
  #volume doubling time
  set_attribute(key="vdt_lrr", value=function() ifelse(get_attribute(sim,"t_LRR") == 0, 0, fn_trnorm(1, mean.norm.vdt, sd.norm.vdt,
                                                                                                     fn_minmax(V_d, V_0, t_min = get_attribute(sim,"t_LRR") - 365, t_max = get_attribute(sim,"t_LRR"))[1], #+1??
                                                                                                     fn_minmax(V_d, V_0, t_min = get_attribute(sim,"t_LRR") - 365, t_max = get_attribute(sim,"t_LRR"))[2]))) %>%
  set_attribute(key="vdt_dm", value=function() ifelse(get_attribute(sim,"t_DM") == 0, 0, fn_trnorm(1, mean.norm.vdt, sd.norm.vdt,
                                                                                                   fn_minmax(V_d, V_0, t_min = get_attribute(sim,"t_DM") - 365, t_max = get_attribute(sim,"t_DM"))[1],
                                                                                                   fn_minmax(V_d, V_0, t_min = get_attribute(sim,"t_DM") - 365, t_max = get_attribute(sim,"t_DM"))[2]))) %>%
  
  #does patient become symptomatic or is lrr detected at routine interval? 
  set_attribute(key = "t_symp_lrr", value=function() ifelse(get_attribute(sim,"t_LRR") == 0, 0, fn_days_symptomatic(vdt = get_attribute(sim, "vdt_lrr"), data = df_patient, model = symp_cox_model))) %>%
  #background mortality
  renege_in(t = function() fn_days_death_oc(age = get_attribute(sim, "Age"), sex = get_attribute(sim, "Sex"), mortality_data), out = out.trj) %>%
  
  #Renege when DM passes detection threshold
  renege_in(
    t = function() {
      vdt_dm <- get_attribute(sim, "vdt_dm")
      if (vdt_dm == 0) {
        return(Inf)  # A very large value to effectively never renege
      } else {
        return(now(.env = sim) + round(fn_t(V_0 = V_0, vdt = vdt_dm),0))
      }
    }, out = dm.trj) %>%
  
  #Time to symptomatic LRR - renege to treatment
  renege_in(
    t = function() { 
      if (get_attribute(sim, "vdt_lrr") == 0) {
        return(Inf)  # A very large value to effectively never renege
      } else {
        return(now(.env = sim) + get_attribute(sim, "t_symp_lrr"))
      }
    }, out = treat.trj) %>%
  
  # Time to imaging event from eligibility
  set_attribute(key="Time", value=function() round(rtnorm(1, mean = 365, sd = 15, a = 1, b = 395))) %>% #distribution to follow-up event, truncated to not be negative
  set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "Time"))  %>%
  timeout_from_attribute(key="Time") %>%              
  #do we really need this?
  set_attribute(key="state.DF", 1, mod="+") %>%
  #insert counter years of surveillance
  set_attribute(key="surv.year", 1, mod="+") %>%
  
  #0 continue with imaging 
  #1 end of surveillance
  branch(option = function() ifelse(get_attribute(sim, "surv.year") >= 6, 1,0), continue=c(F),
         # 1 End of Surveillance
         trajectory() %>%
           set_attribute(keys = "End.Surveillance", values = function() now(.env = sim)) #%>%
         #wait()
  ) %>%
  
  set_attribute(keys = "start.imaging", values = function() now(.env = sim)) %>%
  # Annual Follow-up Imaging Event
  set_attribute(keys = "IMG_st", values = function() now(.env = sim)) %>%               
  set_attribute(
    keys = c("mod", "sens", "spec"),
    values = function() fn_img_mod(mod = sample(0:2, 1, replace = TRUE, prob = c(p.s.mammo, p.s.us, p.s.mri)))
  ) %>%
  set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
  set_attribute(
    keys = c("event", "state"),
    values = function() fn_img_event(V_0 = V_0, t=now(.env = sim) - get_attribute(sim,"LRR_treat_end"), vdt=get_attribute(sim, "vdt_lrr"), sens=get_attribute(sim, "sens"), spec=get_attribute(sim, "spec"))) %>%
  timeout(task = function() round(rtnorm(1, mean = 2, sd = 2, a = 1, b = 10))) %>% #time between hospital and result, right skewed #TO DO VERIFY
  #release(resource = "Imaging") %>%
  set_attribute(keys = "IMG_end", values = function() now(.env = sim)) %>%         
  
  #first branch, based on what the outcome is of the imaging event
  branch(option = function() get_attribute(sim, "event"), continue = c(F, T, T), 
         #Event 1: True Negative (return to DF)
         trajectory() %>%
           set_attribute(keys = "TN_st", values = function() now(.env = sim)) %>% 
           set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380))) %>% 
           timeout_from_attribute(key="Time") %>%              
           set_attribute(keys = "TN_end", values = function() now(.env = sim)) %>%
           set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "Time")) %>% #added              
           rollback(target=16, times=Inf),
         
         #Event 2: Suspicion with additional imaging
         trajectory() %>%
           set_attribute(key="A.IMG_st", values = function() now(.env = sim)) %>%
           set_attribute(
             keys = c("mod", "sens", "spec"),
             values = function() fn_img_mod(mod = sample(0:2, 1, replace = TRUE, prob = c(p.a.mammo, p.a.us, p.a.mri)))) %>%
           set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
           set_attribute(
             keys = c("a.event", "TF"),
             values = function() fn_add_img_event(sens=get_attribute(sim, "sens"), spec=get_attribute(sim, "spec"), state=get_attribute(sim, "state"))) %>% timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% 
           set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>%  #time between screening and result #TO DO VERIFY
           timeout_from_attribute(key="Time") %>%               
           set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_disSUS)*get_attribute(sim, "Time")) %>% #added          
           # TO DO uitzoeken
           set_attribute(key="A.IMG_end", values = function() now(.env = sim)) %>%        
           
           branch(option = function() get_attribute(sim, "a.event"), continue = c(F,T),
                  #Event 1: True Negative of FN if state = 3(return to DF)
                  trajectory() %>%
                    set_attribute(keys = "TN_st", values = function() now(.env = sim)) %>%  
                    set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380))) %>%
                    timeout_from_attribute(key="Time") %>%              
                    set_attribute(keys = "TN_end", values = function() now(.env = sim)) %>%
                    set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "Time")) %>% #added                  
                    rollback(target=26, times=Inf),
                  
                  #Event 2: True Positive / False Positive (biopsy to confirm)
                  trajectory() %>%
                    branch(option=function() fn_biopsy(get_attribute(sim, "TF")), continue=c(F,T), #assumes 100% sens
                           #negative biopsy - rollback to FUP.event 
                           trajectory() %>%
                             set_attribute(keys = "FP_st", values = function() now(.env = sim)) %>% 
                             set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% 
                             #timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                             set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 10, sd = 2, a = 1, b = 20))) %>%
                             timeout_from_attribute(key="Time") %>%              
                             set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_dispFP)*get_attribute(sim, "Time")) %>% #added       
                             set_attribute(keys = "FP_end", values = function() now(.env = sim)) %>% 
                             set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 355, sd = 10, a = 1, b = 380))) %>%  #-10 days
                             timeout_from_attribute(key="Time") %>%               
                             set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL"))*get_attribute(sim, "Time")) %>% #added                  
                             rollback(target=31, times=Inf),
                           
                           #positive biopsy 
                           trajectory() %>%
                             set_attribute(keys = "TP_st", values = function() now(.env = sim)) %>%
                             set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #TO DO from distribution
                             set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 10, sd = 2, a = 1, b = 20))) %>%
                             timeout_from_attribute(key="Time") %>%              
                             set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_disSUS)*get_attribute(sim, "Time")) %>% #added 
                             set_attribute(keys = "TP_end", values = function() now(.env = sim))             
                           #PET to exclude DM?? (according to guideline)
                    )
           ), 
         #Event 3: False Negative
         trajectory() %>%
           set_attribute(key="FN_st", values = function() now(.env = sim)) %>%
           #timeout(task = round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between surveillance and result #TO DO VERIFY
           set_attribute(keys = "FN_end", values = function() now(.env = sim)) %>%                          
           #1 undetected, unsymptomatic
           #0 undetected, symptomatic
           branch(option = function() ifelse(now(.env = sim) < get_attribute(sim,"t_symp_lrr"), 1, 0), continue = c(F),
                  #UNDETECTED UNSYMPTOMATIC UU -> return to DF
                  trajectory() %>%
                    set_attribute(key="UU_st", values = function() now(.env = sim)) %>%
                    set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380))) %>%
                    timeout_from_attribute(key="Time") %>%         
                    set_attribute(keys = "UU_end", values = function() now(.env = sim)) %>%
                    set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_disFN)*get_attribute(sim, "Time")) %>% #added               
                    rollback(target=19) #to DF
           ) %>%
           
           #UNDETECTED SYMPTOMATIC
           set_attribute(key="US_st", values = function() now(.env = sim)) %>%
           set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
           set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
           set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% 
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between imaging/biopsy and result #TO DO VERIFY 
           set_attribute(keys = "US_end", values = function() now(.env = sim)) %>%
           
           #DETECTED SYMPTOMATIC 
           set_attribute(key="DS_st", values = function() now(.env = sim)) %>%
           #assume 100% sens biopsy
           set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% 
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
           set_attribute(keys = "DS_end", values = function() now(.env = sim))        
  ) %>%
  
  #Join Treatment (after detection)
  join(treat.d.trj) #TO DO: different rollback?? #should be 25   

plot(fup.trj, fill = scales::brewer_pal("qual", palette = "Set3"), verbose = T)

model <- plot(fup.trj, fill = scales::brewer_pal("qual", palette = "Set3"), verbose = T)
model %>%
  export_svg() %>%
  charToRaw %>%
  rsvg_pdf("model_2412182.pdf")


#######################
out.trj <- trajectory() %>% 
  set_attribute(
    keys   = c("time_to_death", "start"), values = function() fn_time_to_events(currenttime = now(.env = sim), attrb = get_attribute(sim, "start.sim")))  

# treatment (symptomatic) 
treat.trj <- trajectory() %>%
  set_attribute(key="treat.s", values = function() now(.env = sim)) %>%    
  set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
  set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
  set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #pathological confirmation?
  #seize(resource = "LRR.Treatment") %>%
  set_attribute(keys = "LRR_treat_st", values = function() now(.env = sim)) %>%              
  set_attribute(keys=c("LRR_treat", "cost.treat", "t_treat"), values=function() fn_treatment(horm = get_attribute(sim, "Horm"), sur = get_attribute(sim, "Sur"),chemo = get_attribute(sim, "Chemo")), mod = "+") %>%
  timeout(task = function() (get_attribute(sim, "t_treat"))) %>%
  set_attribute(keys = "cost", values = function() get_attribute(sim, "cost.treat"), mod = "+") %>%        
  #utility for treatment        
  set_attribute(key="QoL", value = function() fn_u_treat(trt = get_attribute(sim, "LRR_treat"))) %>%        
  set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "t_treat")) %>%         
  #release(resource = "LRR.Treatment") %>%
  set_attribute(keys = "LRR_treat_end", values = function() now(.env = sim)) %>% 
  set_attribute(keys = "End.Surveillance", values = function() now(.env = sim)) %>% 
  join(out.trj)  #endpoint, no going back into surveillance            
#timeout(task = function() (round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380)) - get_attribute(sim, "t_treat"))) %>%              
#set_attribute(key="Age", value=function() get_attribute(sim, "Age")+get_attribute(sim, "surv.year")) %>%
#set_attribute(key="A.grp", values=function() fn_age_gp(age = get_attribute(sim, "Age"))) %>%
#rollback(target=20, times=Inf)  

# treatment (after detection) 
treat.d.trj <- trajectory() %>%
  set_attribute(key="treat.d", values = function() now(.env = sim)) %>%
  #seize(resource = "LRR.Treatment") %>%
  set_attribute(keys = "LRR_treat_st", values = function() now(.env = sim)) %>%               
  set_attribute(keys=c("LRR_treat", "cost.treat", "t_treat"), values=function() fn_treatment(horm = get_attribute(sim, "Horm"),sur = get_attribute(sim, "Sur"),chemo = get_attribute(sim, "Chemo")), mod = "+") %>%
  timeout(task = function() (get_attribute(sim, "t_treat"))) %>%
  set_attribute(keys = "cost", values = function() get_attribute(sim, "cost.treat"), mod = "+") %>%
  set_attribute(key="QoL", value = function() fn_u_treat(trt = get_attribute(sim, "LRR_treat"))) %>%        
  set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "t_treat")) %>%         
  #release(resource = "LRR.Treatment") %>%
  set_attribute(keys = "LRR_treat_end", values = function() now(.env = sim)) %>%
  set_attribute(keys = "End.Surveillance", values = function() now(.env = sim)) %>% 
  join(out.trj)  #endpoint, no going back into surveillance               
#timeout(task = function() (round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380)) - get_attribute(sim, "t_treat"))) %>%              
#set_attribute(key="Age", value=function() get_attribute(sim, "Age")+get_attribute(sim, "surv.year")) %>%
#set_attribute(key="A.grp", values=function() fn_age_gp(age = get_attribute(sim, "Age"))) %>%
#rollback(target=30, times=Inf)              

#symptomatic DM INCOMPLETE
dm.trj <- trajectory() %>% 
  set_attribute(key="type.dm", value = function() ifelse(runif(1) < p.oligo, 1, 2)) %>% #1 oligo , 2 dm
  #seize(resource = "WB.Imaging") %>%
  set_attribute(keys = "WB_img_st", values = function() now(.env = sim)) %>% 
  set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
  set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
  set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #pathological confirmation?
  timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between screening and result #TO DO VERIFY
  #release(resource = "WB.Imaging") %>%
  set_attribute(keys = "WB_img_end", values = function() now(.env = sim)) %>%         
  #non-curative / palliative care
  #seize(resource = "NC.treatment") %>%
  set_attribute(keys = "NC_treat_st", values = function() now(.env = sim)) %>%
  renege_in(t = function() now(.env = sim) + fn_day_death_bc(type = get_attribute(sim, "type.dm")), 
            out = trajectory() %>% set_attribute(key="E", mod="+", value=function() u_DMp*(now(.env = sim)-get_attribute(sim, "NC_treat_st")) %>% set_attribute(key="Death_BC", value=1)) %>%              
  set_attribute(keys = "cost", values = function() fn_nc_treatment2(her2 = get_attribute(sim, "Antiher2"), hr = get_attribute(sim,"Horm"), type = get_attribute(sim,"type.dm"))[[2]],
                mod = "+" ) %>%
  set_attribute(keys = "ther",values = function() fn_nc_treatment2(her2 = get_attribute(sim, "Antiher2"), hr = get_attribute(sim,"Horm"), type = get_attribute(sim,"type.dm"))[[1]]) %>%              
  timeout(task = function() 1) %>% #TO DO VERIFY fn_nc_treatment
  #release(resource = "NC.treatment") %>%
  set_attribute(keys = "NC_treat_end", values = function() now(.env = sim)) %>%         
  wait() #wait until patient dies, either background mortality or bc death  
            
            # MAIN TRAJECTORY 
            fup.trj <- trajectory() %>% 
              #patient characteristics
              set_attribute(key="Sex", value=function() fn_sex()) %>%
              set_attribute(key=c("Age","A.grp"), value=function() fn_age()) %>%
              set_attribute(key="Grade", value=function() fn_grade()) %>%
              set_attribute(key="Stage", value=function() fn_stage()) %>%
              set_attribute(key="Nstatus", value=function() fn_nstatus()) %>%
              set_attribute(key="Multif", value=function() fn_multif()) %>%
              set_attribute(key="Sur", value=function() fn_sur()) %>%
              set_attribute(key="Chemo", value=function() fn_chemo()) %>%
              set_attribute(key="Radio", value=function() fn_radio(sur=get_attribute(sim, "Sur"))) %>%
              set_attribute(key="Horm", value=function() fn_horm()) %>%
              set_attribute(key="Antiher2", value=function() fn_antiher2()) %>%
              set_attribute(key="surv.year", value = 0) %>%
              set_attribute(key="LRR_treat_end", value = 0) %>%
              set_attribute(key = "E", value = 0) %>%    #added          
              #time to localregional recurrence and/or distant metastasis
              set_attribute(key="t_LRR", value=function() fn_t_to_tumour(fn_risk(vector = c(get_attribute(sim, "A.grp"),
                                                                                            get_attribute(sim, "Grade"),
                                                                                            get_attribute(sim, "Stage"),
                                                                                            get_attribute(sim, "Nstatus"),
                                                                                            get_attribute(sim, "Multif"),
                                                                                            get_attribute(sim, "Sur"),
                                                                                            get_attribute(sim, "Chemo"),
                                                                                            get_attribute(sim, "Radio"),
                                                                                            get_attribute(sim, "Horm"),
                                                                                            get_attribute(sim, "Antiher2")), rec=1))) %>%
              set_attribute(key="t_DM", value=function() fn_t_to_tumour(fn_risk(vector = c(get_attribute(sim, "A.grp"),
                                                                                           get_attribute(sim, "Grade"),
                                                                                           get_attribute(sim, "Stage"),
                                                                                           get_attribute(sim, "Nstatus"),
                                                                                           get_attribute(sim, "Multif"),
                                                                                           get_attribute(sim, "Sur"),
                                                                                           get_attribute(sim, "Chemo"),
                                                                                           get_attribute(sim, "Radio"),
                                                                                           get_attribute(sim, "Horm"),
                                                                                           get_attribute(sim, "Antiher2")), rec=2))) %>%
              set_attribute(keys = "start.sim", values = function() now(.env = sim)) %>%  
              set_attribute(key="QoL", value = u_DF) %>% #added utility disease free - dependent on if someone develops recurrence or not?              
              #volume doubling time
              set_attribute(key="vdt_lrr", value=function() ifelse(get_attribute(sim,"t_LRR") == 0, 0, fn_trnorm(1, mean.norm.vdt, sd.norm.vdt,
                                                                                                                 fn_minmax(V_d, V_0, t_min = get_attribute(sim,"t_LRR") - 365, t_max = get_attribute(sim,"t_LRR"))[1], #+1??
                                                                                                                 fn_minmax(V_d, V_0, t_min = get_attribute(sim,"t_LRR") - 365, t_max = get_attribute(sim,"t_LRR"))[2]))) %>%
              set_attribute(key="vdt_dm", value=function() ifelse(get_attribute(sim,"t_DM") == 0, 0, fn_trnorm(1, mean.norm.vdt, sd.norm.vdt,
                                                                                                               fn_minmax(V_d, V_0, t_min = get_attribute(sim,"t_DM") - 365, t_max = get_attribute(sim,"t_DM"))[1],
                                                                                                               fn_minmax(V_d, V_0, t_min = get_attribute(sim,"t_DM") - 365, t_max = get_attribute(sim,"t_DM"))[2]))) %>%
              
              #does patient become symptomatic or is lrr detected at routine interval? 
              set_attribute(key = "t_symp_lrr", value=function() ifelse(get_attribute(sim,"t_LRR") == 0, 0, fn_days_symptomatic(vdt = get_attribute(sim, "vdt_lrr"), data = df_patient, model = symp_cox_model))) %>%
              #background mortality
              renege_in(t = function() fn_days_death_oc(age = get_attribute(sim, "Age"), sex = get_attribute(sim, "Sex"), mortality_data), out = out.trj) %>%
              
              #Renege when DM passes detection threshold
              renege_in(
                t = function() {
                  vdt_dm <- get_attribute(sim, "vdt_dm")
                  if (vdt_dm == 0) {
                    return(Inf)  # A very large value to effectively never renege
                  } else {
                    return(now(.env = sim) + round(fn_t(V_0 = V_0, vdt = vdt_dm),0))
                  }
                }, out = dm.trj) %>%
              
              #Time to symptomatic LRR - renege to treatment
              renege_in(
                t = function() { 
                  if (get_attribute(sim, "vdt_lrr") == 0) {
                    return(Inf)  # A very large value to effectively never renege
                  } else {
                    return(now(.env = sim) + get_attribute(sim, "t_symp_lrr"))
                  }
                }, out = treat.trj) %>%
              
              # Time to imaging event from eligibility
              set_attribute(key="Time", value=function() round(rtnorm(1, mean = 365, sd = 15, a = 1, b = 395))) %>% #distribution to follow-up event, truncated to not be negative
              set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "Time")) %>%  
              timeout_from_attribute(key="Time") %>%              
              #do we really need this?
              set_attribute(key="state.DF", 1, mod="+") %>%
              #insert counter years of surveillance
              set_attribute(key="surv.year", 1, mod="+") %>%
              
              #0 continue with imaging 
              #1 end of surveillance
              branch(option = function() ifelse(get_attribute(sim, "surv.year") >= 6, 1,0), continue=c(F),
                     # 1 End of Surveillance
                     trajectory() %>%
                       set_attribute(keys = "End.Surveillance", values = function() now(.env = sim)) #%>%
                     #wait()
              ) %>%
              
              set_attribute(keys = "start.imaging", values = function() now(.env = sim)) %>%
              # Annual Follow-up Imaging Event
              set_attribute(keys = "IMG_st", values = function() now(.env = sim)) %>%               
              set_attribute(
                keys = c("mod", "sens", "spec"),
                values = function() fn_img_mod(mod = sample(0:2, 1, replace = TRUE, prob = c(p.s.mammo, p.s.us, p.s.mri)))
              ) %>%
              set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
              set_attribute(
                keys = c("event", "state"),
                values = function() fn_img_event(V_0 = V_0, t=now(.env = sim) - get_attribute(sim,"LRR_treat_end"), vdt=get_attribute(sim, "vdt_lrr"), sens=get_attribute(sim, "sens"), spec=get_attribute(sim, "spec"))) %>%
              timeout(task = function() round(rtnorm(1, mean = 2, sd = 2, a = 1, b = 10))) %>% #time between hospital and result, right skewed #TO DO VERIFY
              #release(resource = "Imaging") %>%
              set_attribute(keys = "IMG_end", values = function() now(.env = sim)) %>%         
              
              #first branch, based on what the outcome is of the imaging event
              branch(option = function() get_attribute(sim, "event"), continue = c(F, T, T), 
                     #Event 1: True Negative (return to DF)
                     trajectory() %>%
                       set_attribute(keys = "TN_st", values = function() now(.env = sim)) %>% 
                       set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380))) %>% 
                       timeout_from_attribute(key="Time") %>%              
                       set_attribute(keys = "TN_end", values = function() now(.env = sim)) %>%
                       set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "Time")) %>% #added              
                       rollback(target=16, times=Inf),
                     
                     #Event 2: Suspicion with additional imaging
                     trajectory() %>%
                       set_attribute(key="A.IMG_st", values = function() now(.env = sim)) %>%
                       set_attribute(
                         keys = c("mod", "sens", "spec"),
                         values = function() fn_img_mod(mod = sample(0:2, 1, replace = TRUE, prob = c(p.a.mammo, p.a.us, p.a.mri)))) %>%
                       set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
                       set_attribute(
                         keys = c("a.event", "TF"),
                         values = function() fn_add_img_event(sens=get_attribute(sim, "sens"), spec=get_attribute(sim, "spec"), state=get_attribute(sim, "state"))) %>% timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% 
                       set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>%  #time between screening and result #TO DO VERIFY
                       timeout_from_attribute(key="Time") %>%               
                       set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_disSUS)*get_attribute(sim, "Time")) %>% #added          
                       # TO DO uitzoeken
                       set_attribute(key="A.IMG_end", values = function() now(.env = sim)) %>%        
                       
                       branch(option = function() get_attribute(sim, "a.event"), continue = c(F,T),
                              #Event 1: True Negative of FN if state = 3(return to DF)
                              trajectory() %>%
                                set_attribute(keys = "TN_st", values = function() now(.env = sim)) %>%  
                                set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380))) %>%
                                timeout_from_attribute(key="Time") %>%              
                                set_attribute(keys = "TN_end", values = function() now(.env = sim)) %>%
                                set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "Time")) %>% #added                  
                                rollback(target=26, times=Inf),
                              
                              #Event 2: True Positive / False Positive (biopsy to confirm)
                              trajectory() %>%
                                branch(option=function() fn_biopsy(get_attribute(sim, "TF")), continue=c(F,T), #assumes 100% sens
                                       #negative biopsy - rollback to FUP.event 
                                       trajectory() %>%
                                         set_attribute(keys = "FP_st", values = function() now(.env = sim)) %>% 
                                         set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% 
                                         #timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                                         set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 10, sd = 2, a = 1, b = 20))) %>%
                                         timeout_from_attribute(key="Time") %>%              
                                         set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_dispFP)*get_attribute(sim, "Time")) %>% #added       
                                         set_attribute(keys = "FP_end", values = function() now(.env = sim)) %>% 
                                         set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 355, sd = 10, a = 1, b = 380))) %>%  #-10 days
                                         timeout_from_attribute(key="Time") %>%               
                                         set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL"))*get_attribute(sim, "Time")) %>% #added                  
                                         rollback(target=31, times=Inf),
                                       
                                       #positive biopsy 
                                       trajectory() %>%
                                         set_attribute(keys = "TP_st", values = function() now(.env = sim)) %>%
                                         set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #TO DO from distribution
                                         set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 10, sd = 2, a = 1, b = 20))) %>%
                                         timeout_from_attribute(key="Time") %>%              
                                         set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_disSUS)*get_attribute(sim, "Time")) %>% #added 
                                         set_attribute(keys = "TP_end", values = function() now(.env = sim))             
                                       #PET to exclude DM?? (according to guideline)
                                )
                       ), 
                     #Event 3: False Negative
                     trajectory() %>%
                       set_attribute(key="FN_st", values = function() now(.env = sim)) %>%
                       #timeout(task = round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between surveillance and result #TO DO VERIFY
                       set_attribute(keys = "FN_end", values = function() now(.env = sim)) %>%                          
                       #1 undetected, unsymptomatic
                       #0 undetected, symptomatic
                       branch(option = function() ifelse(now(.env = sim) < get_attribute(sim,"t_symp_lrr"), 1, 0), continue = c(F),
                              #UNDETECTED UNSYMPTOMATIC UU -> return to DF
                              trajectory() %>%
                                set_attribute(key="UU_st", values = function() now(.env = sim)) %>%
                                set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380))) %>%
                                timeout_from_attribute(key="Time") %>%         
                                set_attribute(keys = "UU_end", values = function() now(.env = sim)) %>%
                                set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_disFN)*get_attribute(sim, "Time")) %>% #added               
                                rollback(target=19) #to DF
                       ) %>%
                       
                       #UNDETECTED SYMPTOMATIC
                       set_attribute(key="US_st", values = function() now(.env = sim)) %>%
                       set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
                       set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
                       set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% 
                       timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between imaging/biopsy and result #TO DO VERIFY 
                       set_attribute(keys = "US_end", values = function() now(.env = sim)) %>%
                       
                       #DETECTED SYMPTOMATIC 
                       set_attribute(key="DS_st", values = function() now(.env = sim)) %>%
                       #assume 100% sens biopsy
                       set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% 
                       timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                       set_attribute(keys = "DS_end", values = function() now(.env = sim))        
              ) %>%
              
              #Join Treatment (after detection)
              join(treat.d.trj) #TO DO: different rollback?? #should be 25    
