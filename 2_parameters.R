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
#t_min <- fn_time_to_LRR() - 365               #year of recurrence -1 * 365
#t_max <- fn_time_to_LRR()                     #year of recurrence * 365
  
#duration parameters
t_first_fup <-   #between x and y days #how to handle adherence? 
t_fup <- 0  
t_img <- 0
t_biopsy <- 0
t_fn <- 0

p.sens.test <- 
p.spec.test <- 
# TO DO: differentiate between mammo/us/mri
  
# Parameters patient characteristics 
d_pt_age <-
d_LRR <-   #if time to recurrence is smaller than time past, LRR occurs
d_DM <-   #if time to dm is smaller than time past, dm occurs 
  

