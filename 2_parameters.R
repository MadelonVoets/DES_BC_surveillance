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
# AGE                               <60 0 | 60-69 1 | 70-79 2 | >= 80 4
# GRADE                             G1 0 | G2 1 | G3 2
# STAGE                             S1 0 | S2 1 | S3 2 
# NODAL                             N0 0 | N1 1 | N2 2 | N3 3
# MULTI                             No 0 | Yes 1
# SUR                               BCS 0 | MST 1
# CHEMO                             No 0 | Yes 1
# RADIO                             No 0 | Yes 1
# HORM                              HR- 0 | HR+ THER- 1 | HR+ THER+ 2
# ANTIHER2                          HER2- 0 | HER2+ THER- 1 | HER2+ THER+ 2

# Parameters for tumour characteristics // INFLUENCE parameters
p.grade1 <- 0.34 #F
p.grade2 <- 0.33 #F
p.grade3 <- 0.33 #F
  
p.stage1 <- 0.54 #F
p.stage2 <- 0.33 #F
p.stage3 <- 0.13 #F  

p.nstatus   <- 1 #stage 1/2: no N2 and N3
p.multif    <- 0.5 #F
p.sur       <- 1
p.chemo     <- 1 
p.rad       <- 1 
p.hrstat    <- 1
p.her2stat  <- 1 
  
p.female <- 1 #no males

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
  

