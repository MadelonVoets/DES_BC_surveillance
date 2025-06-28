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
#p.bsc.rt.n <- p.bsc.rt.n 
#p.bsc.rt.y <- p.bsc.rt.y
#p.mst.rt.n <- p.mst.rt.n
#p.mst.rt.y <- p.mst.rt.y
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
p.s.mammo <- 1 #iedereen als eerste mammo
p.s.us <- 0
p.s.mri <- 0

#probabilities during additional imaging
#TO DO probs uitzoeken
p.a.mammo <- 0.15
p.a.us <- 0.8
p.a.mri <- 0.05
#Parameters probabilities of DM or oligo disease
p.oligo <- 0.165
p.noligo <- 0.835
#survival parameter bc death after lrr diagnosis
lrr_exp_rate <- 5.781796e-05
#survival parameters oligo dm (<3)
oligo.scale = 0.9248594
oligo.coef = 7.434267 
#survival parameters dm (>3)
dm.scale = 0.8820885
dm.coef = 6.989314            #OG: 7.089314 
#Routine visits 
routine_visit_times <- c(0, 365, 730, 1095, 1460, 1825)

#UTILITIES
u_df_m <- 0.824  #disease free
u_df_sd <- 0.824 

u_lrr1_m <-  0.73 #first year lrr
u_lrr1_sd <- 0.02

u_lrr_m <-  0.71 #after 1 yr lrr
u_lrr_sd <- 0.09

u_lrr_srt_m <-  0.80 #treatment surgery  rt LRR
u_lrr_srt_sd <- 0.27

u_lrr_rt_m <-  0.77 #treatment rt LRR
u_lrr_rt_sd <- 0.02

u_lrr_ch_m <-  0.77 #treatment chemotherapy LRR
u_lrr_ch_sd <- 0.02

u_lrr_h_m <-  0.82 #treatment hormonal LRR
u_lrr_h_sd <- 0.02

u_lrr_mst_m <- 0.76 #treatment surgery alone LRR
u_lrr_mst_sd <- 0.02

u_dm1_m <-  0.58 #1st year dm
u_dm1_sd <- 0.06

u_dm_m <-  0.60 #after 1st yr DM
u_dm_sd <- 0.05

u_dm_t_m <-  0.602 #DM on treatment
u_dm_t_sd <- 0.31

#disutilities
u_FP_b_m <- -0.058 #FP biopsy
u_FP_b_sd <- 0.079

u_FP_mri_m <- -0.067 #FP MRI
u_FP_mri_sd <- 0.083

u_sdet_m <- -0.033 #symptomatic detection
u_sdet_sd <- 0.077

#COSTS
#outpatient visit
c_poli_m <- 128.67
c_poli_sd <- c_poli_m * 0.2

#cost conventional OR, per minute
c_or_m <- 11.89
c_or_sd <- c_or_m * 0.2

#mammography
c_mammo_m <- 109.20
c_mammo_sd <- c_mammo_m * 0.2

#ultrasound
c_us_m <- 102.70
c_us_sd <- c_us_m * 0.2

#mri
c_mri_m <- 286.22
c_mri_sd <- c_mri_m * 0.2

#whole body pet
c_pet_wb_m <- 1040.91 
c_pet_wb_sd <- c_pet_wb_m * 0.2

#partial pet
c_pet_pt_m <- 857.03
c_pet_pt_sd <- c_pet_pt_m * 0.2

#biopsy
c_biopsy_m <- 176.65
c_biopsy_sd <- c_biopsy_m * 0.2

#hormonal therapy
c_horm_m <- 3241.63
c_horm_sd <- c_horm_m * 0.2

#radiotherapy
c_radio_m <- 8668.12
c_radio_sd <- c_radio_m * 0.2 

#chemotherapy
c_chemo_m <- 4498.59
c_chemo_sd <- c_chemo_m * 0.2

#mastectomy
c_MST_m <- 2043.76
c_MST_sd <- c_MST_m * 0.2

#targeted therapy
c_tar_m <- 1
c_tar_sd <- 1

#treatment dm, per month
c_dm_m <- 1621
c_dm_sd <- 1254.45



































