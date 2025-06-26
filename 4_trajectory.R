## 4. TRAJECTORY ----

#background mortality - patient can die at any point in the model
out.trj <- trajectory() %>% 
  set_attribute(
    keys   = c("TimeInSurv", "TimeInTrt", "Effect.T", "Costs.T", "QALYs", "dQALYs", "Cost", "dCost"), 
    values = function() fn_calculate_impact(
      CurrentTime = now(.env = sim), 
      Attrs       = get_attribute(sim, keys=c("start.sim", "LRR_treat_st","End.Surveillance", "Death_BM", "Death_BC", "E", "E.t", "cost", "cost.t"))
    )
  )

#Breast cancer death
out.dm <- trajectory() %>%
  set_attribute(key="Trt_dm_complete", 1) %>%
  set_attribute(keys = "cost.t", values = function() fn_trt_dm_c(time = (now(.env = sim) - get_attribute(sim,"start_trt_dm"))), mod = "+") %>%
  set_attribute(key="QoL", value = function() fn_u_treat(trt = get_attribute(sim, "LRR_treat"))) %>%        
  set_attribute(key="E.t", mod="+", value=function() (now(.env = sim) - get_attribute(sim,"start_trt_dm"))*(u_dm_trt(psa =0))) %>%
  set_attribute(keys = "End.Surveillance", values = function() round(now(.env = sim), digits = 0)) %>%
  join(out.trj)
  
# treatment (symptomatic) 
treat.trj <- trajectory() %>%
  set_attribute(key="treat.s", values = function() round(now(.env = sim), digits = 0)) %>%
  set_attribute(keys = "Tvol", values = function() fn_vdt(V_0, t = now(.env = sim), vdt = get_attribute(sim, "vdt_lrr"))) %>% #tumour volume at detection
  set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
  set_attribute(keys = "cost.t", values = function() fn_cost_img(mod = get_attribute(sim, "mod"), psa = 0), mod = "+") %>%
  set_attribute(keys = "cost.t", values = c_biopsy(psa=0), mod = "+") %>% #pathological confirmation?
  set_attribute(keys = "LRR_treat_st", values = function() round(now(.env = sim), digits = 0)) %>%
  set_attribute(keys=c("LRR_treat", "cost.treat", "t_treat"), values=function() fn_treatment(horm = get_attribute(sim, "Horm"),
                                                               sur = get_attribute(sim, "Sur"),
                                                               chemo = get_attribute(sim, "Chemo"), psa=0)) %>%
  timeout(task = function() (get_attribute(sim, "t_treat"))) %>%
  set_attribute(keys = "cost.t", values = function() get_attribute(sim, "cost.treat"), mod = "+") %>% 
  set_attribute(key="QoL", value = function() fn_u_treat(trt = get_attribute(sim, "LRR_treat"))) %>%        
  set_attribute(key="E.t", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "t_treat")) %>%  
  set_attribute(key="E.t", mod="+", value=function() u_dis_symp(psa=0)*42) %>%  #disutility symptomatic detection for 6 weeks (=42 days)    
  set_attribute(keys = "LRR_treat_end", values = function() round(now(.env = sim), digits = 0)) %>% 
  set_attribute(keys = "End.Surveillance", values = function() round(now(.env = sim), digits = 0)) %>% 
  join(out.trj)

# treatment (after detection) 
treat.d.trj <- trajectory() %>%
  set_attribute(key="treat.d", values = function() round(now(.env = sim), digits = 0)) %>%
  set_attribute(keys = "Tvol", values = function() fn_vdt(V_0, t = now(.env = sim), vdt = get_attribute(sim, "vdt_lrr"))) %>% #tumour volume at detection
  set_attribute(keys = "LRR_treat_st", values = function() round(now(.env = sim), digits = 0)) %>% 
  set_attribute(keys=c("LRR_treat", "cost.treat", "t_treat"), values=function() fn_treatment(horm = get_attribute(sim, "Horm"),
                                                                                       sur = get_attribute(sim, "Sur"),
                                                                                       chemo = get_attribute(sim, "Chemo"), psa=0)) %>%
  timeout(task = function() (get_attribute(sim, "t_treat"))) %>%
  set_attribute(keys = "cost.t", values = function() get_attribute(sim, "cost.treat"), mod = "+") %>%
  set_attribute(key="QoL", value = function() fn_u_treat(trt = get_attribute(sim, "LRR_treat"))) %>%        
  set_attribute(key="E.t", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "t_treat")) %>%         
  set_attribute(keys = "LRR_treat_end", values = function() round(now(.env = sim), digits = 0)) %>%
  set_attribute(keys = "End.Surveillance", values = function() round(now(.env = sim), digits = 0)) %>%
  join(out.trj)

#symptomatic DM INCOMPLETE
dm.trj <- trajectory() %>% 
  set_attribute(key="type.dm", value = function() ifelse(runif(1) < p.oligo, 1, 2)) %>% #1 oligo , 2 dm
  set_attribute(keys = "WB_img_st", values = function() now(.env = sim)) %>% 
  set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
  set_attribute(keys = "cost.t", values = function() fn_cost_img(mod = get_attribute(sim, "mod"),psa=0), mod = "+") %>%
  set_attribute(keys = "cost.t", values = c_biopsy(psa=0), mod = "+") %>% #pathological confirmation?
  timeout(task = function() round(rtnorm(1, mean = 7, sd = 1, a = 0, b = 10))) %>% #time between screening and result #TO DO VERIFY
  set_attribute(keys = "WB_img_end", values = function() now(.env = sim)) %>%         
  #non-curative / palliative care
  set_attribute(key="start_trt_dm",values = function() now(.env = sim)) %>%
  renege_in(t = function() now(.env = sim) + fn_day_death_bc(type = get_attribute(sim, "type.dm")), 
            out = trajectory() %>% 
              set_attribute(key="Death_BC", value = function() now(.env = sim)) %>%
              set_attribute(keys = "cost.t", values = function() fn_trt_dm_c(time = (now(.env = sim) - get_attribute(sim,"start_trt_dm"))), mod = "+") %>%
              set_attribute(key="QoL", value = function() fn_u_treat(trt = get_attribute(sim, "LRR_treat"))) %>%        
              set_attribute(key="E.t", value=function() (now(.env = sim) - get_attribute(sim,"start_trt_dm"))*(u_dm_trt(psa =0)), mod="+",)) %>%
  set_attribute(
    keys = c("ther", "cost.t"),
    values = function() fn_dm_treatment(her2 = get_attribute(sim, "Antiher2"), hr = get_attribute(sim,"Horm"), type = get_attribute(sim,"type.dm")),
    mod = "+" ) %>%
  timeout(365*5) %>% 
  set_attribute(key="end_trt_dm",values = function() now(.env = sim)) %>%
  join(out.dm)
  #wait() #wait until patient dies, either background mortality or bc death

 
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
  renege_in(t = function() now(.env = sim) + fn_days_death_oc(age = get_attribute(sim, "Age"), sex = get_attribute(sim, "Sex"), mortality_data), out = trajectory() %>% set_attribute(key="Death_BM", values= function() now(.env = sim)) %>% join(out.trj)) %>%
  
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
  
  #renege_abort() %>%

  # Time to imaging event from eligibility
  set_attribute(key="Time", value=function() round(rtnorm(1, mean = 365, sd = 15, a = 1, b = 395))) %>% #distribution to follow-up event, truncated to not be negative
  set_attribute(key="QoL", value = u_df(psa = 0)) %>% #MOET WAARDE IEDER JAAR VERANDEREN?
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
  set_attribute(keys = "cost", values = function() fn_cost_img(mod = get_attribute(sim, "mod"),psa=0), mod = "+") %>%
  set_attribute(
    keys = c("event", "state"),
    values = function() fn_img_event(V_0 = V_0, t=now(.env = sim), vdt=get_attribute(sim, "vdt_lrr"), sens=get_attribute(sim, "sens"), spec=get_attribute(sim, "spec"))) %>%
  timeout(task = function() round(rtnorm(1, mean = 2, sd = 2, a = 1, b = 10))) %>% #time between hospital and result, right skewed #TO DO VERIFY
  set_attribute(keys = "IMG_end", values = function() now(.env = sim)) %>%  
  
  #first branch, based on what the outcome is of the imaging event
  branch(option = function() get_attribute(sim, "event"), continue = c(F, T, T), 
         #Event 1: True Negative (return to DF)
         trajectory() %>%
           set_attribute(keys = "TN_st", values = function() now(.env = sim)) %>% 
           set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380))) %>% 
           timeout_from_attribute(key="Time") %>%              
           set_attribute(keys = "TN_end", values = function() now(.env = sim)) %>%
           set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "Time")) %>% 
           rollback(target=16, times=Inf),
         
         #Event 2: Suspicion with additional imaging
         trajectory() %>%
           set_attribute(key="A.IMG_st", values = function() now(.env = sim)) %>%
           #set_attribute(key="A.Imaging", 1, mod="+") %>%
           set_attribute(
             keys = c("mod", "sens", "spec"),
             values = function() fn_img_mod(mod = sample(0:2, 1, replace = TRUE, prob = c(p.a.mammo, p.a.us, p.a.mri)))) %>%
           set_attribute(keys = "cost", values = function() fn_cost_img(mod = get_attribute(sim, "mod"), psa=0), mod = "+") %>%
           #timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between screening and result #TO DO VERIFY
           set_attribute(keys = "Time", values = function() ifelse(get_attribute(sim, "mod") == 2, round(rtnorm(1, mean = 20, sd = 1, a = 1, b = 24)), round(rtnorm(1, mean = 7, sd = 1, a = 1, b = 10)))) %>%
           # TO DO uitzoeken
           timeout_from_attribute(key="Time") %>%               
           set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_dis(mod = get_attribute(sim,"mod"), psa = 0))*get_attribute(sim, "Time")) %>% #added 
           set_attribute(
             keys = c("a.event", "TF"),
             values = function() fn_add_img_event(sens=get_attribute(sim, "sens"), spec=get_attribute(sim, "spec"), state=get_attribute(sim, "state"))) %>%
           set_attribute(key="A.IMG_end", values = function() now(.env = sim)) %>% 
           
           branch(option = function() get_attribute(sim, "a.event"), continue = c(F,T),
                  #Event 1: True Negative of FN if state = 3(return to DF)
                  trajectory() %>%
                    set_attribute(keys = "TN_st", values = function() now(.env = sim)) %>%  
                    set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 365, sd = 10, a = 1, b = 380))) %>%
                    timeout_from_attribute(key="Time") %>%              
                    set_attribute(keys = "TN_end", values = function() now(.env = sim)) %>%
                    set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "Time")) %>% #added                  
                    rollback(target=25, times=Inf),
            
                  #Event 2: True Positive / False Positive (biopsy to confirm)
                  trajectory() %>%
                    branch(option=function() fn_biopsy(get_attribute(sim, "TF")), continue=c(F,T), #assumes 100% sens
                           #negative biopsy - rollback to FUP.event 
                           trajectory() %>%
                             set_attribute(keys = "FP_st", values = function() now(.env = sim)) %>% 
                             set_attribute(keys = "cost", values = c_biopsy(psa=0), mod = "+") %>% 
                             #timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 1, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                             set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 10, sd = 2, a = 1, b = 20))) %>%
                             timeout_from_attribute(key="Time") %>%              
                             set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_dis(mod = 0, psa = 0))*get_attribute(sim, "Time")) %>% #added       
                             set_attribute(keys = "FP_end", values = function() now(.env = sim)) %>% 
                             set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 355, sd = 10, a = 1, b = 380))) %>%  #-10 days
                             timeout_from_attribute(key="Time") %>%               
                             set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL"))*get_attribute(sim, "Time")) %>% #added                  
                             rollback(target=30, times=Inf),
                           
                           #positive biopsy 
                           trajectory() %>%
                             renege_abort() %>%   #WHY?           
                             set_attribute(keys = "TP_st", values = function() now(.env = sim)) %>%
                             set_attribute(keys = "cost", values = c_biopsy(), mod = "+") %>% #TO DO from distribution
                             set_attribute(keys = "Time", values = function() round(rtnorm(1, mean = 10, sd = 2, a = 1, b = 20))) %>%
                             timeout_from_attribute(key="Time") %>%              
                             set_attribute(key="E", mod="+", value=function() (get_attribute(sim, "QoL") + u_dis(mod = 0, psa = 0))*get_attribute(sim, "Time")) %>%
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
                    set_attribute(key="E", mod="+", value=function() get_attribute(sim, "QoL")*get_attribute(sim, "Time")) %>% #added               
                    rollback(target=19) #to DF
                  ) %>%
         
           #UNDETECTED SYMPTOMATIC
           set_attribute(key="US_st", values = function() now(.env = sim)) %>%
           set_attribute(keys = "mod", values = function() fn_img_pet()) %>% #mod 3 = wb, 4 = pt
           set_attribute(keys = "cost", values = function() fn_cost_img(mod = get_attribute(sim, "mod"),psa=0), mod = "+") %>%
           set_attribute(keys = "cost", values = c_biopsy(psa=0), mod = "+") %>% 
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between imaging/biopsy and result #TO DO VERIFY 
           set_attribute(keys = "US_end", values = function() now(.env = sim)) %>%
           
           #DETECTED SYMPTOMATIC 
           set_attribute(key="DS_st", values = function() now(.env = sim)) %>%
           #assume 100% sens biopsy
           set_attribute(keys = "cost", values = c_biopsy(psa=0), mod = "+") %>% 
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
           set_attribute(keys = "DS_end", values = function() now(.env = sim))   
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
model.plot <- plot(fup.trj, fill = scales::brewer_pal("qual", palette = "Set3"), verbose = T)
model.plot %>%
  export_svg() %>%
  charToRaw %>%
  rsvg_pdf(paste0("model", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))


