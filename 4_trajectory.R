## 4. TRAJECTORY ----

#background mortality - patient can die at any point in the model
out.trj <- trajectory() %>% 
  set_attribute(
    keys   = c("time_to_death", "start"), 
    values = function() fn_time_to_events(
      currenttime = now(.env = sim), 
      attrb       = get_attribute(sim, "start.sim")
    )
  )

# treatment (symptomatic) 
treat.trj <- trajectory() %>%
  set_attribute(key="treat.s", 1, mod="+") %>%
  set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
  set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
  set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #pathological confirmation?
  seize(resource = "LRR.Treatment") %>%
  set_attribute(keys=c("LRR_treat", "cost", "t_treat"), values=function() fn_treatment(horm = get_attribute(sim, "Horm"),
                                                               sur = get_attribute(sim, "Sur"),
                                                               chemo = get_attribute(sim, "Chemo")), mod = "+") %>%
  timeout(task = function() (get_attribute(sim, "t_treat"))) %>%
  release(resource = "LRR.Treatment") %>%
  set_attribute(key="Age", value=function() get_attribute(sim, "Age")+get_attribute(sim, "surv.year")) %>%
  set_attribute(key="A.grp", values=function() fn_age_gp(age = get_attribute(sim, "Age"))) %>%
  rollback(target=19, times=Inf)

# treatment (after detection) 
treat.d.trj <- trajectory() %>%
  set_attribute(key="treat.d", 1, mod="+") %>%
  seize(resource = "LRR.Treatment") %>%
  set_attribute(keys=c("LRR_treat", "cost", "t_treat"), values=function() fn_treatment(horm = get_attribute(sim, "Horm"),
                                                                                       sur = get_attribute(sim, "Sur"),
                                                                                       chemo = get_attribute(sim, "Chemo")), mod = "+") %>%
  timeout(task = function() (get_attribute(sim, "t_treat"))) %>%
  release(resource = "LRR.Treatment") %>%
  set_attribute(key="Age", value=function() get_attribute(sim, "Age")+get_attribute(sim, "surv.year")) %>%
  set_attribute(key="A.grp", values=function() fn_age_gp(age = get_attribute(sim, "Age"))) %>%
  rollback(target=28, times=Inf)

#symptomatic DM INCOMPLETE
dm.trj <- trajectory() %>% 
  set_attribute(key="type.dm", value = function() ifelse(runif(1) < p.oligo, 1, 2)) %>% #1 oligo , 2 dm
  seize(resource = "WB.Imaging") %>%
  set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
  set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
  set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #pathological confirmation?
  #set_attribute(
  #  keys = c("mod", "cost.img"),
  #  values = function() fn_img_wb(), #always PET? #TO DO
  #  mod = '+'
  #) %>%
  renege_in(t = function() now(.env = sim) + fn_day_death_bc(type = get_attribute(sim, "type.dm")), out = trajectory() %>% set_attribute(key="Death_BC", values=1)) %>%
  timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between screening and result #TO DO VERIFY
  release(resource = "WB.Imaging") %>%
  #non-curative / palliative care
  seize(resource = "NC.treatment") %>%
  set_attribute(
    keys = c("ther", "cost"),
    values = function() fn_nc_treatment2(her2 = get_attribute(sim, "Antiher2"), hr = get_attribute(sim,"Horm"), type = get_attribute(sim,"type.dm")),
    mod = "+" ) %>%
  timeout(task = function() 1) %>% #TO DO VERIFY fn_nc_treatment
  release(resource = "NC.treatment") %>%
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
  renege_in(t = function() now(.env = sim) + fn_days_death_oc(age = get_attribute(sim, "Age"), sex = get_attribute(sim, "Sex"), mortality_data), out = out.trj) %>%
  # #symptomatic DM INCOMPLETE
  #renege_in(t = function() now(.env = sim) + fn_days_symptomatic(vdt = get_attribute(sim, "vdt_dm"), data = df_patient, model = symp_cox_model), out = dm.trj) %>%
  #Renege based on Cox model LRR
  # renege_in(
  #   t = function() {
  #     vdt_dm <- get_attribute(sim, "vdt_dm")
  #     if (vdt_dm == 0) {
  #       return(Inf)  # A very large value to effectively never renege
  #     } else {
  #       return(now(.env = sim) + fn_days_symptomatic(vdt = vdt_dm, data = df_patient, model = symp_cox_model))
  #     }
  #   },
  #   out = dm.trj
  # ) %>%
  
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
  # TO DO: TAKE TIME AROUND ANNUAL SURVEILLANCE MOMENT INTO ACCOUNT
  renege_in(
    t = function() { 
      if (get_attribute(sim, "vdt_lrr") == 0) {
        return(Inf)  # A very large value to effectively never renege
      } else if (get_attribute(sim, "treat.d")) {
        return(Inf)  # A very large value to effectively never renege
      } else {
        return(now(.env = sim) + get_attribute(sim, "t_symp_lrr"))
      }
    }, out = treat.trj) %>%
  
  renege_abort() %>%

  # Time to imaging event from eligibility
  timeout(task = function() round(rnorm(1, mean = 365, sd = 15))) %>% #distribution to follow-up event, truncated to not be negative
  
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
  seize(resource = "Imaging") %>%
  set_attribute(
    keys = c("mod", "sens", "spec"),
    values = function() fn_img_mod(mod = sample(0:2, 1, replace = TRUE, prob = c(p.s.mammo, p.s.us, p.s.mri)))
  ) %>%
  set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
  set_attribute(
    keys = c("event", "state"),
    values = function() fn_img_event(V_0 = V_0, t=now(.env = sim), vdt=get_attribute(sim, "vdt_lrr"), sens=get_attribute(sim, "sens"), spec=get_attribute(sim, "spec"))) %>%
  timeout(task = function() round(rtnorm(1, mean = 2, sd = 2, a = 1, b = 10))) %>% #time between hospital and result, right skewed #TO DO VERIFY
  release(resource = "Imaging") %>%
  
  #first branch, based on what the outcome is of the imaging event
  branch(option = function() get_attribute(sim, "event"), continue = c(F, T, T), 
         #Event 1: True Negative (return to DF)
         trajectory() %>%
           seize(resource="TN", amount=1) %>%
           set_attribute(key="TN", 1, mod="+") %>%
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between imaging and result #TO DO VERIFY
           timeout(task = function() round(rnorm(1, mean = 365, sd = 15))) %>% # time +1 year
           release(resource="TN", amount=1) %>%
           rollback(target=16, times=Inf),
         
         #Event 2: Suspicion with additional imaging
         trajectory() %>%
           seize(resource = "A.Imaging") %>%
           set_attribute(key="A.Imaging", 1, mod="+") %>%
           set_attribute(
             keys = c("mod", "sens", "spec"),
             values = function() fn_img_mod(mod = sample(0:2, 1, replace = TRUE, prob = c(p.a.mammo, p.a.us, p.a.mri)))) %>%
           set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between screening and result #TO DO VERIFY
           # TO DO uitzoeken
           release(resource = "A.Imaging") %>%
           set_attribute(
             keys = c("a.event", "TF"),
             values = function() fn_add_img_event(sens=get_attribute(sim, "sens"), spec=get_attribute(sim, "spec"), state=get_attribute(sim, "state"))) %>%
           
           branch(option = function() get_attribute(sim, "a.event"), continue = c(F,T),
                  #Event 1: True Negative of FN if state = 3(return to DF)
                  trajectory() %>%
                    seize(resource="TN", amount=1) %>% 
                    set_attribute(key="TN", 1, mod="+") %>%
                    timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between imaging and result #TO DO VERIFY
                    release(resource="TN", amount=1) %>%
                    timeout(task = function() round(rtnorm(1, mean = 365, sd = 15))) %>% # time +1 year %>% # 1 year?
                    rollback(target=24, times=Inf),
            
                  #Event 2: True Positive / False Positive (biopsy to confirm)
                  trajectory() %>%
                    branch(option=function() fn_biopsy(get_attribute(sim, "TF")), continue=c(F,T), #assumes 100% sens
                           #negative biopsy - rollback to FUP.event 
                           trajectory() %>%
                             seize(resource="FP", amount=1) %>%
                             set_attribute(key="FP", 1, mod="+") %>%
                             set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% 
                             timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                             timeout(task = function() round(rtnorm(1, mean = 365, sd = 15))) %>% # time +1 year # 1 year?
                             release(resource="FP", amount=1) %>%
                             rollback(target=26, times=Inf),
                           
                           #positive biopsy 
                           trajectory() %>%
                             seize(resource="TP", amount=1) %>%
                             set_attribute(key="TP", 1, mod="+") %>%
                             set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #TO DO from distribution
                             timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                             release(resource="TP", amount=1) 
                             #PET to exclude DM?? (according to guideline)
                    )
           ), 
         #Event 3: False Negative
         trajectory() %>%
           seize(resource="FN", amount=1) %>% 
           set_attribute(key="FN", 1, mod="+") %>%
           timeout(task = round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between surveillance and result #TO DO VERIFY
           release(resource="FN", amount=1) %>%
           #1 undetected, unsymptomatic
           #0 undetected, symptomatic
           branch(option = function() ifelse(now(.env = sim) < get_attribute(sim,"t_symp_lrr"), 1, 0), continue = c(F),
                  #UNDETECTED UNSYMPTOMATIC UU -> return to DF
                  trajectory() %>%
                    seize(resource="UU", amount=1) %>% 
                    set_attribute(key="state_UU", values=1, mod = "+") %>%
                    timeout(task = function() round(rtnorm(1, mean = 365, sd = 15))) %>%  # 1 year wait?
                    release(resource="UU", amount=1) %>% 
                    rollback(target=20) #to DF
                  ) %>%
         
           #UNDETECTED SYMPTOMATIC
           seize(resource="US", amount=1) %>% 
           set_attribute(key="state_US", values=1, mod = "+") %>%
           set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
           set_attribute(keys = "cost.img", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
           set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% 
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between imaging/biopsy and result #TO DO VERIFY
           release(resource="US", amount=1) %>% 
           
           #DETECTED SYMPTOMATIC 
           seize(resource="DS", amount=1) %>%
           set_attribute(key="state_DS", values=1, mod = "+") %>%
           #assume 100% sens biopsy
           set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% 
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
           release(resource="DS", amount=1) 
  ) %>%
           
           #Join Treatment (after detection)
           join(treat.d.trj) #TO DO: different rollback?? #should be 25
        
 
# Visualize trajectories
#plot(fup.trj, fill = scales::brewer_pal("qual", palette = "Set3"), verbose = T)
#plot(out.trj, fill = scales::brewer_pal("qual", palette = "Set3"), verbose = T)
#plot(treat.trj, fill = scales::brewer_pal("qual", palette = "Set3"), verbose = T)
#plot(dm.trj, fill = scales::brewer_pal("qual", palette = "Set3"), verbose = T)

#save model plot as pdf
#library(DiagrammeRsvg)
model <- plot(fup.trj, fill = scales::brewer_pal("qual", palette = "Set3"), verbose = T)
model %>%
  export_svg() %>%
  charToRaw %>%
  rsvg_pdf("model_250105.pdf")


