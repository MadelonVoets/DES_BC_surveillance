## 4. TRAJECTORY ----

#background mortality - patient can die at any point in the model
out.trj <- trajectory() %>% 
  set_attribute(
    keys   = c("time_to_death"), 
    values = function() fn_time_to_events(
      currenttime = now(.env = sim), 
      attrb       = get_attribute(.env = sim, keys = c("start_time"))
    )
  )

# treatment (after positive biopsy) 
treat.trj <- trajectory() %>%
  seize(resource = "LRR.Treatment") %>%
  set_attribute(keys=c("LRR_treat", "cost", "t_treat"), values=function() fn_treatment(horm = get_attribute(sim, "Horm"),
                                                               sur = get_attribute(sim, "Sur"),
                                                               chemo = get_attribute(sim, "Chemo")), mod = "+") %>%
  timeout(task = function() (get_attribute(sim, "t_treat"))) %>%
  release(resource = "LRR.Treatment") %>%
  rollback(target=12, times=Inf)

#symptomatic DM INCOMPLETE
dm.trj <- trajectory() %>% 
  seize(resource = "WB.Imaging") %>%
  set_attribute(
    keys = c("mod", "cost"),
    values = function() 1, #always PET
    mod = '+'
  ) %>%
  timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between screening and result #TO DO VERIFY
  release(resource = "WB.Imaging") %>%
  #non-curative / palliative care
  seize(resource = "NC-treatment") %>%
  timeout(task = function() 1) %>% #TO DO VERIFY fn_nc_treatment
  release(resource = "NC-treatment") %>%
  #Breast cancer Death
  set_attribute(key="Death_BC", values=1) 
  #End of simulation

 
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
  set_attribute(key="Radio", value=function() fn_radio()) %>%
  set_attribute(key="Horm", value=function() fn_horm()) %>%
  set_attribute(key="Antiher2", value=function() fn_antiher2()) %>%

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
  set_attribute(key="vdt_lrr", value=function() fn_trnorm(1, mean.norm.vdt, sd.norm.vdt,
                                                         fn_minmax(V_d, V_0, t_LRR = get_attribute(sim,"t_LRR") - 365, t_LRR = get_attribute(sim,"t_LRR"))[1],
                                                         fn_minmax(V_d, V_0, t_LRR = get_attribute(sim,"t_LRR") - 365, t_LRR = get_attribute(sim,"t_LRR"))[2])) %>%
  set_attribute(key="vdt_dm", value=function() fn_trnorm(1, mean.norm.vdt, sd.norm.vdt,
                                                          fn_minmax(V_d, V_0, t_DM = get_attribute(sim,"t_DM") - 365, t_DM = get_attribute(sim,"t_DM"))[1],
                                                          fn_minmax(V_d, V_0, t_DM = get_attribute(sim,"t_DM") - 365, t_DM = get_attribute(sim,"t_DM"))[2])) %>%
  set_attribute(keys = "start_surveillance", values = function() now(.env = sim)) %>%
  # #background mortality
  renege_in(t = function() now(.env = sim) + fn_days_death_oc(age = get_attribute(sim, "Age"), sex = get_attribute(sim, "Sex"), mortality_data), out = out.trj) %>%
  # #symptomatic DM INCOMPLETE
  #renege_in(t = function() now(.env = sim) + fn_days_symp_dm(1), out = dm.trj) %>%
  # #symptomatic LRR INCOMPLETE
  # renege_in(t = function() now(.env = sim) + fn_days_symp_lrr(1), out = treat.trj) %>% #dfi and vdt as input parameters?

  # Time to first imaging event
  timeout(task = function() round(rtnorm(1, mean = 15.5, sd = 10, a = 0, b = Inf))) %>% #distribution to follow-up event, truncated to not be negative
  
  #set_attribute(key="state", "DF") %>%
  set_attribute(key="state_DF", mod="+", value=function() 1) %>%
  
  #TO DO: insert counter years of surveillance
  set_attribute(key="surv.year", mod="+", value=function() 1) %>%
  
  # Follow-up imaging event
  seize(resource = "Imaging") %>%
  set_attribute(
    keys = c("mod", "sens", "spec"),
    values = function() fn_img_mod(mod = sample(0:2, 1, replace = TRUE, prob = c(p.s.mammo, p.s.us, p.s.mri)))
  ) %>%
  set_attribute(keys = "cost", values = function() fn_cost_img(mod = get_attribute(sim, "mod")), mod = "+") %>%
  set_attribute(
    keys = c("event"),
    values = function() fn_img_event(V_0 = V_0, t=now(.env = sim), vdt=get_attribute(sim, "vdt_lrr"), sens=get_attribute(sim, "sens"), spec=get_attribute(sim, "spec"))) %>%
  timeout(task = function() round(rtnorm(1, mean = 2, sd = 2, a = 0, b = 10))) %>% #time between screening and result, right skewed #TO DO VERIFY
  release(resource = "Imaging") %>%
  
  #first branch, based on what the outcome is of the imaging event
  branch(option = function() get_attribute(sim, "event"), continue = c(F, T, T), 
         #Event 1: True Negative (return to DF)
         trajectory() %>%
           seize(resource="TN", amount=1) %>% 
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between imaging and result #TO DO VERIFY
           timeout(task = function() round(rtnorm(1, mean = 365, sd = 1, a = 0, b = 7))) %>% # 1 year?
           release(resource="TN", amount=1) %>%
           rollback(target=12, times=Inf),
         
         #Event 2: Suspicion with additional imaging
         trajectory() %>%
           seize(resource = "A_Imaging") %>%
           set_attribute(
             keys = c("a.mod", "cost"),
             values = function() fn_add_img(mod = sample(0:2, 1, replace = TRUE, prob = c(p.a.mammo, p.a.us, p.a.mri)), at = now(.env = sim)),
             mod = '+'
           ) %>%
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between screening and result #TO DO VERIFY
           # TO DO uitzoeken
           release(resource = "A_Imaging") %>%
           
           branch(option = function() fn_add_img_event(), continue = c(F,T),
                  #Event 1: True Negative (return to DF)
                  trajectory() %>%
                    seize(resource="TN", amount=1) %>% 
                    timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between imaging and result #TO DO VERIFY
                    timeout(task = function() round(rtnorm(1, mean = 365, sd = 1, a = 0, b = 7))) %>% # 1 year?
                    release(resource="TN", amount=1) %>%
                    rollback(target=17, times=Inf),
            
                  #Event 2: True Positive / False Positive (biopsy to confirm)
                  trajectory() %>%
                    branch(option=function() fn_biopsy(get_attribute(sim, "result")), continue=c(F,T), 
                           #negative biopsy - rollback to FUP.event 
                           trajectory() %>%
                             seize(resource="FP", amount=1) %>%
                             set_attribute(keys = "cost", values = c_biopsy, mod = "+") %>% #TO DO from distribution?
                             timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                             timeout(task = function() round(rtnorm(1, mean = 365, sd = 1, a = 0, b = 7))) %>% # 1 year?
                             release(resource="FP", amount=1) %>%
                             rollback(target=19, times=Inf),
                           
                           #positive biopsy 
                           trajectory() %>%
                             seize(resource="TP", amount=1) %>% 
                             set_attribute(keys = "cost", values = c_biopsy, mod = "+") %>% #TO DO from distribution
                             timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                             release(resource="TP", amount=1) 
                             #PET to exclude DM?? (according to guideline)
                             #join(treat.trj)
                    )
           ), 
         #Event 3: False Negative
         trajectory() %>%
           seize(resource="FN", amount=1) %>% 
           timeout(task = round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between surveillance and result #TO DO VERIFY
           release(resource="FN", amount=1) %>%
           set_attribute(key="state_UU", values=1, mod = "+") %>%
           
           branch(option = function() now(.env = sim) < fn_days_symp_lrr(1), continue = c(F),
                  #UNDETECTED UNSYMPTOMATIC UU -> reutrn to DF
                  trajectory() %>%
                    seize(resource="UU", amount=1) %>% 
                    timeout(task = round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #1 year?
                    release(resource="UU", amount=1) %>% 
                    rollback(target=16) #to DF
                  ) %>%
         
           #UNDETECTED SYMPTOMATIC
           seize(resource="US", amount=1) %>% 
           set_attribute(
             keys = c("mod", "cost"),
             values = function() fn_img_wb(),
             mod = '+'
           ) %>%
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between imaging and result #TO DO VERIFY
           release(resource="US", amount=1) %>% 
           
           #DETECTED SYMPTOMATIC 
           seize(resource="DS", amount=1) %>% 
           #assume 100% sens biopsy
           set_attribute(keys = "cost", values = c_biopsy, mod = "+") %>% #TO DO from distribution?
           timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
           release(resource="DS", amount=1) 
  ) %>%
           
           #Join Treatment
           join(treat.trj)
        
 
# Visualize 
plot(fup.trj, fill = scales::brewer_pal("qual", palette = "Set3"))

model %>%
  export_svg() %>%
  charToRaw %>%
  rsvg_pdf("model_240502.pdf")

