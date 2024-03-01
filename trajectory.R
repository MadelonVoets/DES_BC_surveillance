### TRAJECTORY
library(simmer);
library(simmer.plot);
library(dplyr)
library(DiagrammeRsvg)
library(rsvg)

t_img <- 0
t_biopsy <- 0
t_fn <- 0


out.trj <- trajectory() %>% 
  set_attribute(
    keys   = c("time_of_death"), 
    values = function() fn_time_to_events(
      currenttime = now(.env = sim), 
      attrb       = get_attribute(.env = sim, keys = c("start_time"))
    )
  )

fup.trj <- trajectory() %>% 
  set_attribute(keys = "start_time", values = function() now(.env = sim)) %>% 
  renege_in(t = function() now(.env = sim) + t_to_death_oc, out = out_trj) %>% 
  #set_attribute(key="state", "DF") %>%
  set_attribute(key="state_DF", values=1) %>%
  
   # Time to first imaging event
  #timeout(task = t_first_fup) %>% #distribution to follow-up event
  
  # Follow-up imaging event
  seize(resource = "Imaging") %>%
  set_attribute(
    keys = c("mod", "cost"),
    values = function() fn_img_mod(at = now(.env = sim)),
    mod = '+'
  ) %>%
  #timeout(task = t_fup) %>%
  release(resource = "Imaging") %>%
  
  #TO DO: insert counter years of surveillance
  set_attribute(key="surv.year", value=0) %>%

  #first branch, based on what the outcome is of the imaging event
  branch(option = function() fn_img_event(get_attribute(sim, "mod")), continue = c(T,T,T,T), #,F,T), 
       #Event 1: True Negative
       trajectory() %>%
         set_attribute(key="surv.year", mod="+", value=function() 1) %>%
         seize(resource="TN", amount=1) %>% 
         timeout(task = t_img) %>% #wachttijd imaging?
         release(resource="TN", amount=1) %>%
         rollback(target=9, times=Inf),
       
       #Event 2: True Positive / False Positive
       trajectory() %>%
         branch(option=function() get_attribute(sim, "biopt.DF"), continue=c(T,T), #make it a function??
                #negative biopsy - rollback to FUP.event 
                trajectory() %>%
                  set_attribute(key="surv.year", mod="+", value=function() 1) %>%
                  seize(resource="FP", amount=1) %>% 
                  timeout(task = t_img) %>% 
                  timeout(task  = t_biopsy) %>%
                  release(resource="FP", amount=1) %>%
                  rollback(target=11, times=Inf),
                
                #positive biopsy 
                trajectory() %>%  
                  set_attribute(key="surv.year", mod="+", value=function() 1) %>%
                  seize(resource="TP", amount=1) %>% 
                  timeout(task = t_img) %>% 
                  timeout(task = t_biopsy) %>% 
                  release(resource="TP", amount=1) %>%
                  set_attribute(key="state_DU", values=1)
         ),
       
       #Event 3: Additional imaging?
       trajectory() %>%
         timeout(task = t_img) %>%
         rollback(target=6, times=Inf),
       
       #Event 4: False Negative? 
       trajectory() %>%
         seize(resource="FP", amount=1) %>% 
         timeout(task = t_img) %>%
         release(resource="FP", amount=3) %>%
         set_attribute(key="state_UU", values=1, mod = "+") %>%
         #set_attribute(key="surv.year", mod="+", value=function() 1) %>%
         
         branch(option = function() fn_img_event_UU(get_attribute(sim, "mod")), continue = c(T,T), #different fn_img_event function?? Denk het wel om van UU naar US te gaan // needs to be function that keeps track of time to move to state US
                #0 to skip branch to undetected symptomatic
                #undetected unsymptomatic UU
                trajectory() %>%
                  #set_attribute(key="state_UU", mod="+", values = 1) %>%
                  timeout(task = t_img) %>%
                  set_attribute(key="surv.year", mod="+", values = 1) %>%
                  rollback(target=4),
                
                #undetected symptomatic
                trajectory() %>%
                  timeout(task = t_img) %>%
                  set_attribute(key="state_US", values=1, mod = "+")
                ) %>%
         
         #detected (un)symptomatic
         set_attribute(key="state_DS", values=1, mod = "+") %>% 
         branch(option=function() get_attribute(sim, "biopt.US"), continue=c(T,T), #different biopsy value?? #make it a function based on probability?
                #negative biopsy - should this even be possible?
                trajectory() %>%
                  #set_attribute(key="state_US", mod="+", values = 1) %>%
                  timeout(task  = t_biopsy) %>%
                  rollback(target=5),
                
                #positive biopsy = 
                trajectory() %>%  
                  set_attribute(key="state_DS", values = 1, mod = "+") %>%
                  timeout(task = t_biopsy)
         ) %>%
         timeout(task = t_fn)
  ) %>%
  seize(resource="Treatment", amount=1)

 
# Visualize 
plot(fup.trj)

model %>%
  export_svg() %>%
  charToRaw %>%
  rsvg_pdf("graph.pdf")

