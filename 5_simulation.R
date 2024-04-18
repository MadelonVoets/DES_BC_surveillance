## 5. SIMULATION ----

# Simulation settings
n.patients <- 10;    # number of patients to simulate 
mon.patients <- 2;    # level of monitoring (see add_generator)

# Run simulation for the best standard care (bsc)
sim <- simmer() %>%
  add_resource(name="Imaging", capacity=Inf, mon=F) %>%
  #add_resource(name="FU1", capacity=Inf, mon=F) %>%
  #add_resource(name="Tx2", capacity=Inf, mon=F) %>%
  #add_resource(name="FU2", capacity=Inf, mon=F) %>%
  add_generator(name_prefix="Patient", trajectory=fup.trj, distribution=at(rep(x=0, times=n.patients)), mon=mon.patients);

sim %>% run();

sim.out <- get_mon_attributes(sim);       # retrieve the monitor object


#View(getMultipleAttributes(c("Tx1.Event","Tx1.Cycles","Tx2.Event","Tx2.Cycles","C","E"), bsc.out)); 
