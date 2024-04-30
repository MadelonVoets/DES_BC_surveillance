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

#symptomatic DM INCOMPLETE
dm.trj <- trajectory() %>% 
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

# treatment (after positive biopsy) INCOMPLETE
treat.trj <- trajectory() 

# MAIN TRAJECTORY 
fup.trj <- trajectory() %>% 
  #patient characteristics
  # set_attribute(key="Sex", value=function() fn_sex()) %>%
  # set_attribute(key=c("Age","A.grp"), value=function() fn_age()) %>%
  # set_attribute(key="Grade", value=function() fn_grade()) %>%
  # set_attribute(key="Stage", value=function() fn_stage()) %>%
  # set_attribute(key="Nstatus", value=function() fn_nstatus()) %>%
  # set_attribute(key="Multif", value=function() fn_multif()) %>%
  # set_attribute(key="Sur", value=function() fn_sur()) %>%
  # set_attribute(key="Chemo", value=function() fn_chemo()) %>%
  # set_attribute(key="Radio", value=function() fn_radio()) %>%
  # set_attribute(key="Horm", value=function() fn_horm()) %>%
  # set_attribute(key="Antiher2", value=function() fn_antiher2()) %>%
  # 
  # set_attribute(key="t_LRR", value=function() fn_t_to_tumour(fn_risk(vector = c(get_attribute(sim, "A.grp"),
  #                                                               get_attribute(sim, "Grade"),
  #                                                               get_attribute(sim, "Stage"),
  #                                                               get_attribute(sim, "Nstatus"),
  #                                                               get_attribute(sim, "Multif"),
  #                                                               get_attribute(sim, "Sur"),
  #                                                               get_attribute(sim, "Chemo"),
  #                                                               get_attribute(sim, "Radio"),
  #                                                               get_attribute(sim, "Horm"),
  #                                                               get_attribute(sim, "Antiher2")), rec=1))) %>%
  # set_attribute(key="t_DM", value=function() fn_t_to_tumour(fn_risk(vector = c(get_attribute(sim, "A.grp"),
  #                                                                              get_attribute(sim, "Grade"),
  #                                                                              get_attribute(sim, "Stage"),
  #                                                                              get_attribute(sim, "Nstatus"),
  #                                                                              get_attribute(sim, "Multif"),
  #                                                                              get_attribute(sim, "Sur"),
  #                                                                              get_attribute(sim, "Chemo"),
  #                                                                              get_attribute(sim, "Radio"),
  #                                                                              get_attribute(sim, "Horm"),
  #                                                                              get_attribute(sim, "Antiher2")), rec=2))) %>%
  # #set_attribute(key="vdt_lrr", value=function() ) %>%
  # #vdt <- fn_trnorm(1, mean.norm.vdt, sd.norm.vdt, fn_minmax(V_d, V_0, t_DM - 365, t_DM)[1], fn_minmax(V_d, V_0, t_DM - 365, t_DM)
  # 
  # set_attribute(keys = "start_time", values = function() now(.env = sim)) %>% 
  # #background mortality
  # renege_in(t = function() now(.env = sim) + fn_days_death_oc(age = get_attribute(sim, "Age"), sex = get_attribute(sim, "Sex"), mortality_data), out = out.trj) %>%
  # #symptomatic DM INCOMPLETE
  # renege_in(t = function() now(.env = sim) + fn_days_symp_dm(1), out = dm.trj) %>%
  # #symptomatic LRR INCOMPLETE
  # renege_in(t = function() now(.env = sim) + fn_days_symp_lrr(1), out = treat.trj) %>%

  #set_attribute(key="state", "DF") %>%
  set_attribute(key="state_DF", mod="+", value=function() 1) %>%
  
   # Time to first imaging event
  timeout(task = function() round(rtnorm(1, mean = 0, sd = 15.5, a = 0, b = Inf))) %>% #distribution to follow-up event, truncated to not be negative
  
  #TO DO: insert counter years of surveillance
  set_attribute(key="surv.year", mod="+", value=function() 1) %>%
  
  # Follow-up imaging event
  seize(resource = "Imaging") %>%
  set_attribute(
    keys = c("i.mod", "cost"),
    values = function() fn_img_mod(mod = sample(0:2, 1, replace = TRUE, prob = c(p.s.mammo, p.s.us, p.s.mri)), at = now(.env = sim)),
    mod = '+'
  ) %>%
  set_attribute(
    keys = c("result", "num"),
    values = function() fn_img_event()) %>%
  timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between screening and result #TO DO VERIFY
  # TO DO uitzoeken
  release(resource = "Imaging") %>%
  
  #first branch, based on what the outcome is of the imaging event
  branch(option = function() get_attribute(sim, "num"), continue = c(T, T), 
         #Event 1: Suspicion with additional imaging
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
           
           branch(option = function() fn_add_img_event(), continue = c(T,T),
                  #Event 1: True Negative (return to DF)
                  trajectory() %>%
                    seize(resource="TN", amount=1) %>% 
                    timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between imaging and result #TO DO VERIFY
                    release(resource="TN", amount=1) %>%
                    rollback(target=17, times=Inf),
            
                  #Event 2: True Positive / False Positive (biopsy to confirm)
                  trajectory() %>%
                    branch(option=function() fn_biopsy(get_attribute(sim, "result")), continue=c(T,T), 
                           #negative biopsy - rollback to FUP.event 
                           trajectory() %>%
                             seize(resource="FP", amount=1) %>%
                             set_attribute(keys = "cost", values = c_biopsy, mod = "+") %>% #TO DO from distribution?
                             timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                             release(resource="FP", amount=1) %>%
                             rollback(target=19, times=Inf),
                           
                           #positive biopsy 
                           trajectory() %>%
                             seize(resource="TP", amount=1) %>% 
                             set_attribute(keys = "cost", values = c_biopsy, mod = "+") %>% #TO DO from distribution
                             timeout(task = function() round(rtnorm(1, mean = 1, sd = 1, a = 0, b = 7))) %>% #time between biopsy and result #TO DO VERIFY
                             release(resource="TP", amount=1)
                    )
           ), 
         #Event 2: False Negative
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
  seize(resource="Treatment", amount=1) 

           
         
  
         
         
         
      

  
#branch #rollback to DF or DM

 
# Visualize 
plot(fup.trj, fill = scales::brewer_pal("qual", palette = "Set1"))


model %>%
  export_svg() %>%
  charToRaw %>%
  rsvg_pdf("graph.pdf")

