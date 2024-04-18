## 4. TRAJECTORY ----

#background mortality - patient can die at any point in the model
out.trj <- trajectory() %>% 
  set_attribute(
    keys   = c("time_of_death"), 
    values = function() fn_time_to_events(
      currenttime = now(.env = sim), 
      attrb       = get_attribute(.env = sim, keys = c("start_time"))
    )
  )

#clinical sings - at DF, Treatment & UU
cs.trj <- trajectory() %>% 
  set_attribute(
    keys   = c("time_of_cs"), 
    values = function() fn_time_to_events(
      currenttime = now(.env = sim), 
      attrb       = get_attribute(.env = sim, keys = c("start_time"))
    ) 
  ) %>%
  seize(resource = "WB.Imaging") %>%
  set_attribute(
    keys = c("mod", "cost"),
    values = function() 1, #always PET?
    mod = '+'
  ) %>%
  timeout(task = t_wbimaging) %>%
  release(resource = "WB.Imaging")

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
  
  
  #set_attribute(key="Age", value=function() setAge(get_attribute(bsc.sim, "Male"))) %>%
  #set_attribute(key="Tx1.Response", value=function() setTx1(get_attribute(bsc.sim, "Poor"))) %>%
  #set_attribute(key="Tx2.Response", value=function() setTx2(get_attribute(bsc.sim, "Tx1.Response"))) %>%
  #set_attribute(key="Tx1.Cycles", value=0) %>%
  #set_attribute(key="Tx2.Cycles", value=0) %>%
  #set_attribute(key="C", value=0) %>%
  #set_attribute(key="E", value=0) %>%
  
  set_attribute(keys = "start_time", values = function() now(.env = sim)) %>% 
  renege_in(t = function() now(.env = sim) + fn_days_death_oc(age = get_attribute(sim, "Age"), sex = get_attribute(sim, "Sex"), mortality_data), out = out.trj) %>% 

  #set_attribute(key="state", "DF") %>%
  set_attribute(key="state_DF", mod="+", value=function() 1) %>%
  
   # Time to first imaging event
  timeout(task = function() rnorm(1, mean = 0, sd = 15.5)) %>% #distribution to follow-up event
  
  # Follow-up imaging event
  seize(resource = "Imaging") %>%
  #set_attribute(
  #  keys = c("mod", "cost"),
  #  values = function() fn_img_mod(at = now(.env = sim)),
  #  mod = '+'
  #) %>%
  #timeout(task = t_fup) %>%
  release(resource = "Imaging") %>%
  
  #TO DO: insert counter years of surveillance
  set_attribute(key="surv.year", mod="+", value=function() 1) %>%

  #first branch, based on what the outcome is of the imaging event
  branch(option = function() fn_img_event(get_attribute(sim, "mod")), continue = c(T,T,T,T), #,F,T), 
       #Event 1: True Negative
       trajectory() %>%
         #set_attribute(key="surv.year", mod="+", value=function() 1) %>%
         seize(resource="TN", amount=1) %>% 
         timeout(task = t_img) %>% #wachttijd imaging? gemiddelde wachttijd tot uitslag?
         release(resource="TN", amount=1) %>%
         set_attribute(key="C", mod="+", value=function() 1) %>%
         set_attribute(key="E", mod="+", value=function() 1) %>%
         rollback(target=10, times=Inf),
       
       #Event 2: True Positive / False Positive
       trajectory() %>%
         branch(option=function() get_attribute(sim, "biopt.DF"), continue=c(T,T), #make it a function??
                #negative biopsy - rollback to FUP.event 
                trajectory() %>%
                  #set_attribute(key="surv.year", mod="+", value=function() 1) %>%
                  seize(resource="FP", amount=1) %>% 
                  timeout(task = t_img) %>% 
                  timeout(task  = t_biopsy) %>%
                  release(resource="FP", amount=1) %>%
                  set_attribute(key="C", mod="+", value=function() 1) %>%
                  set_attribute(key="E", mod="+", value=function() 1) %>%
                  rollback(target=12, times=Inf),
                
                #positive biopsy 
                trajectory() %>%  
                  #set_attribute(key="surv.year", mod="+", value=function() 1) %>%
                  seize(resource="TP", amount=1) %>% 
                  timeout(task = t_img) %>% 
                  timeout(task = t_biopsy) %>% 
                  release(resource="TP", amount=1) %>%
                  set_attribute(key="C", mod="+", value=function() 1) %>%
                  set_attribute(key="E", mod="+", value=function() 1) %>%
                  set_attribute(key="state_DU", values=1)
         ),
       
       #Event 3: Additional imaging?
       trajectory() %>%
         timeout(task = t_img) %>%
         set_attribute(key="C", mod="+", value=function() 1) %>%
         set_attribute(key="E", mod="+", value=function() 1) %>%
         rollback(target=8, times=Inf),
       
       #Event 4: False Negative? 
       trajectory() %>%
         seize(resource="FN", amount=1) %>% 
         timeout(task = t_img) %>%
         release(resource="FN", amount=1) %>%
         set_attribute(key="state_UU", values=1, mod = "+") %>%
         #set_attribute(key="surv.year", mod="+", value=function() 1) %>%
         
         branch(option = function() fn_img_event_UU(get_attribute(sim, "mod")), continue = c(T,T), #different fn_img_event function?? Denk het wel om van UU naar US te gaan // needs to be function that keeps track of time to move to state US
                #0 to skip branch to undetected symptomatic
                #undetected unsymptomatic UU
                trajectory() %>%
                  seize(resource="UU", amount=1) %>% 
                  timeout(task = t_img) %>%
                  release(resource="UU", amount=1) %>% 
                  set_attribute(key="surv.year", mod="+", values = 1) %>%
                  set_attribute(key="C", mod="+", value=function() 1) %>%
                  set_attribute(key="E", mod="+", value=function() 1) %>%
                  rollback(target=8),
                
                #undetected symptomatic
                trajectory() %>%
                  seize(resource="US", amount=1) %>% 
                  timeout(task = t_img) %>%
                  release(resource="US", amount=1) %>% 
                  set_attribute(key="surv.year", mod="+", values = 1) %>%
                  set_attribute(key="C", mod="+", value=function() 1) %>%
                  set_attribute(key="E", mod="+", value=function() 1) #what's better? Seize and release or set_attribute?
                  #set_attribute(key="state_US", values=1, mod = "+")
                ) %>%
         
         #detected (un)symptomatic
         set_attribute(key="state_DS", values=1, mod = "+") %>% 
         branch(option=function() get_attribute(sim, "biopt.US"), continue=c(T,T), #different biopsy value?? #make it a function based on probability?
                #negative biopsy - should this even be possible?
                trajectory() %>%
                  timeout(task = t_biopsy) %>%
                  rollback(target=5),
                
                #positive biopsy = 
                trajectory() %>%  
                  seize(resource="DS", amount=1) %>% 
                  #set_attribute(key="state_DS", values = 1, mod = "+") %>%
                  timeout(task = t_biopsy) %>% 
                  release(resource="DS", amount=1) 
         ) %>%
         timeout(task = t_fn)
  ) %>%
  seize(resource="Treatment", amount=1) %>%
  set_attribute(keys = "TreatmentCount", values=1, mod="+") %>%
  timeout(task = t_treatment) %>%
  release(resource="Treatment", amount=1) %>%
  
  timeout(task = t_surveilance) %>%
  branch #rollback to DF or DM

 
# Visualize 
plot(fup.trj)

model %>%
  export_svg() %>%
  charToRaw %>%
  rsvg_pdf("graph.pdf")

