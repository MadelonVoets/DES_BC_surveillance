## 2. PARAMETERS ----

# These are the parameters that define and are used in the trajectory. The prefix of the 
# parameter name indicates what type of parameter it is:
#   d_    distribution parameter
#   n_    count
#   p_    probability
#   t_    time duration
#   c_    cost
#   u_    utility

#### PATIENT INITIALIZATION #####
p.female <- 1 #no males
# AGE                               <60 0 | 60-69 1 | 70-79 2 | >= 80 4
m.age <- m.age
sd.age <- sd.age
# GRADE                             G1 0 | G2 1 | G3 2
p.gr1 <- p.gr1
p.gr2 <- p.gr2
p.gr3 <- p.gr3
# STAGE                             S1 0 | S2 1 | S3 2 
p.st1 <- p.st1
p.st2 <- p.st2
p.st3 <- p.st3
# NODAL                             N0 0 | N1 1 | N2 2 | N3 3
p.n0 <- p.n0
p.n1 <- p.n1
p.n2 <- p.n2
p.n3 <- p.n3
# MULTI                             No 0 | Yes 1
p.multi.n <- p.multi.n
p.multi.y <- p.multi.y
# SUR                               BCS 0 | MST 1
p.bcs <- p.bcs
p.mst <- p.mst
# CHEMO                             No 0 | Yes 1
p.chemo.n <- p.chemo.n
p.chemo.y <- p.chemo.y
# RADIO                             No 0 | Yes 1
p.rt.y <- p.rt.y
p.rt.n <- p.rt.n
# HORM                              HR- 0 | HR+ THER- 1 | HR+ THER+ 2
p.hr.n <- p.hr.n
p.hr.y.ther.n <- p.hr.y.ther.n
p.hr.y.ther.y <- p.hr.y.ther.y
# ANTIHER2                          HER2- 0 | HER2+ THER- 1 | HER2+ THER+ 2
p.her2.n <- p.her2.n
p.her2.y.ther.n <- p.her2.y.ther.n
p.her2.y.ther.y <- p.her2.y.ther.y

# Tumour model parameters  
# Define parameters for the normal distribution
mean.norm.vdt <- 178.75
sd.norm.vdt <- 75.75
d_0 <- 0.1                                    #initial tumour diameter (mm)
d_t <- 5                                      #detection threshold (mm)

V_d <-((4/3)*pi*(d_t/2)^3)/1000               #volume in cm^3 based on diameter in mm
V_0 <-((4/3)*pi*(d_0/2)^3)/1000               #volume in cm^3 based on diameter in mm

#imaging sensitivities and specificities
#MRI provided an overall sensitivity and specificity of 94.6% (range 85.7%–100%) and 74.2% (range 25%–100%) respectively, 
#while mammography showed that the overall sensitivity was at 54.5% (range 27%–86.8%) and specificity was 85.5% (range 62.9%–98.8%). 
#The overall sensitivity and specificity of ultrasound was 67.2% (range 26.9%–87.5%) and 76.8% (range 18.8%–96.9%). https://doi.org/10.1016/j.radi.2022.01.006

p.sens.mammo <- 0.75 #https://doi.org/10.2214/AJR.20.24204,  https://doi.org/10.1148/radiol.2019182394
p.spec.mammo <- 0.90
p.sens.dbt <- 0
p.spec.dbt <- 0
p.sens.us <- 0.80 #https://doi.org/10.1200/JGO.19.00127
p.spec.us <- 0.88
p.sens.mri <- 0.95
p.spec.mri <- 0.74
# TO DO: mammo/us/mri sens en spec
  
# Parameters imaging
#probabilities during annual surveillance 
#TO DO probs uitzoeken
p.s.mammo <- 0
p.s.us <- 0.
p.s.mri <- 0

#probabilities during additional imaging
#TO DO probs uitzoeken
p.a.mammo <- 0
p.a.us <- 0
p.a.mri <- 0

#TO DO costs uitzoeken
c_mammo <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
}
c_us <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
}
c_mri <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
}
c_pet <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
}
c_biopsy <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
} #dependent on image-guided?

#treatment costs
c_horm <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
  }
c_radio <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
}
c_chemo <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
}
c_MST <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
}

c_tar <- function(n){
  cost <- round(rnorm(n, mean = 1, sd = 1))
  return(cost)
}
#treatment timeouts
t_horm <- function(n){
  timeout <- round(rnorm(n, mean = 1, sd = 1))
  return(timeout)
}
t_radio <- function(n){
  timeout <- round(rnorm(n, mean = 1, sd = 1))
  return(timeout)
}
t_chemo <- function(n){
  timeout <- round(rnorm(n, mean = 1, sd = 1))
  return(timeout)
}
t_MST <- function(n){
  timeout <- round(rnorm(n, mean = 1, sd = 1))
  return(timeout)
}
t_tar <- function(n){
  timeout <- round(rnorm(n, mean = 1, sd = 1))
  return(timeout)
}
#Parameters probabilities of DM or oligo disease
p.oligo <- 0.165
p.noligo <- 0.835

#Probabilities type of therapy  for oligo
p.o.l.rt <- 0.28
p.o.s.horm <- 0.43
p.o.s.chemo <- 0.12
p.o.s.ch.tar <- 0.06
p.o.s.tar <- 0.33
p.o.s.n <- 0.06
#Probabilities type of therapy non-oligo (oligo3 study)
p.l.rt <- 0.30
p.s.horm <- 0.57
p.s.chemo <- 0.07
p.s.ch.tar <- 0.05
p.s.tar <- 0.27
p.s.n <- 0.04
#survival parameters oligo dm (<3)
oligo.scale = 0.9248594
oligo.coef = 7.434267 
#survival parameters dm (>3)
dm.scale = 0.8820885
dm.coef = 7.089314 



