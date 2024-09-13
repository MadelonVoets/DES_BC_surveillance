## 5. SIMULATION ----

# Simulation settings
n.patients <- 100;    # number of patients to simulate 
mon.patients <- 2;    # level of monitoring (see add_generator)

# Run simulation for the best standard care (bsc)
sim <- simmer() %>%
  #main
  add_resource(name="Imaging", capacity=Inf, mon=T) %>%
  add_resource(name="TN", capacity=Inf, mon=T) %>%
  add_resource(name="A.Imaging", capacity=Inf, mon=T) %>%
  add_resource(name="FP", capacity=Inf, mon=T) %>%
  add_resource(name="TP", capacity=Inf, mon=T) %>%
  add_resource(name="FN", capacity=Inf, mon=T) %>%
  add_resource(name="UU", capacity=Inf, mon=T) %>%
  add_resource(name="US", capacity=Inf, mon=T) %>%
  add_resource(name="DS", capacity=Inf, mon=T) %>%
  #treat.trj
  add_resource(name="LRR.Treatment", capacity=Inf, mon=T) %>%
  #DM
  add_resource(name="WB.Imaging", capacity=Inf, mon=T) %>%
  add_resource(name="NC.treatment", capacity=Inf, mon=T) %>%
  add_generator(name_prefix="Patient", trajectory=fup.trj, distribution=at(rep(x=0, times=n.patients)), mon=mon.patients);

start <- proc.time()
sim %>% run();
print( proc.time() - start )

sim.out <- get_mon_attributes(sim);       # retrieve the monitor object

output <- getMultipleAttributes(c("name", "Age","Grade","Stage","Nstatus","Multif","Chemo", "Sur", "Radio", "Horm", "Antiher2", "t_LRR", "vdt_lrr", "t_DM", "treat.d", "treat.s", "t_symp_lrr", "surv.year", "start.imaging"), sim.out)
View(output)

