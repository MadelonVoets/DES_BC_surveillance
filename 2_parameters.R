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
p_grade     <- 
p_stage     <- 
p_nstatus   <- #stage 1/2: no N2 and N3
p_multif    <- 
p_sur       <- 
p_chemo     <- 
p_rad       <-  
p_hrstat    <- 
p_her2stat  <- 
  
p.female <- 1

#make patient characterics dataframe  
df.char <- data.frame(ID = 1:n.i)  #n.i number of individuals
df.char$age <- fn_age()  #distribution or sample?
df.char$grade <- 
df.char$stage <- 
df.char$nstatus <- 
df.char$multif <- 
df.char$sur <- 
df.char$chemo <- 
df.char$radio <- 
df.char$horm <- 
df.char$antiher2 <- 
  
# Parameter for background mortality (time to death other causes)
#time to death other causes
d_death_shape <- 
d_death_rate  <- 
  
t_to_death_oc <- #background mortality
  
t_to_LRR <-
t_to_DM <- 
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
  

