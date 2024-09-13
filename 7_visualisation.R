## 7. VISUALISATION ----

#resource utilisation
resources <- get_mon_resources(sim)
plot(resources, metric = "utilization")

#resources’ activity during the simulation
plot(resources, metric = "usage", c("Imaging", "TN", "A.Imaging", "FP", "TP", "FN", "UU", "US", "DS", "LRR.Treatment", "WB.Imaging", "NC.treatment"), items = "server")









 