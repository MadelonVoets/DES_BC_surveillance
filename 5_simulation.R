## 5. SIMULATION ----

# Simulation settings
n.patients <- 10;    # number of patients to simulate 
mon.patients <- 2;    # level of monitoring (see add_generator)

# Run simulation for the best standard care (bsc)
sim <- simmer() %>%
  #main
  add_resource(name="Imaging", capacity=Inf, mon=F) %>%
  add_resource(name="TN", capacity=Inf, mon=F) %>%
  add_resource(name="A.Imaging", capacity=Inf, mon=F) %>%
  add_resource(name="FP", capacity=Inf, mon=F) %>%
  add_resource(name="TP", capacity=Inf, mon=F) %>%
  add_resource(name="FN", capacity=Inf, mon=F) %>%
  add_resource(name="UU", capacity=Inf, mon=F) %>%
  add_resource(name="US", capacity=Inf, mon=F) %>%
  add_resource(name="DS", capacity=Inf, mon=F) %>%
  #treat.trj
  add_resource(name="LRR.treatment", capacity=Inf, mon=F) %>%
  #DM
  add_resource(name="WB.Imaging", capacity=Inf, mon=F) %>%
  add_resource(name="NC.treatment", capacity=Inf, mon=F) %>%
  add_generator(name_prefix="Patient", trajectory=fup.trj, distribution=at(rep(x=0, times=n.patients)), mon=mon.patients);

sim %>% run();

sim.out <- get_mon_attributes(sim);       # retrieve the monitor object

View(getMultipleAttributes(c("Age","Grade","Stage","Nstatus","Multif","Chemo", "Sur", "Radio", "Horm", "Antiher2", "surv.year", "start.imaging"), sim.out))
