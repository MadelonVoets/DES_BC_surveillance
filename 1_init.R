## 1. INITIALIZATION ----

# Clear the workspace
rm(list=ls()); gc();

# Load the required packages 
library(simmer);                 #Version 4.4.6.3
library(simmer.plot);            #Version 0.1.18
library(dplyr);                  #Version 1.1.4
library(rsvg);                   #Version 2.6.0
library(readr)
library(contsurvplot)
library(survival)
#library(DiagrammeRsvg)
#library(readxl)

sessionInfo()

# Load funtions for extracting monitored attributes
source("getSingleAttribute.R", echo=TRUE);
source("getMultipleAttributes.R", echo=TRUE);

#rel_path <= "~/AMICUS/Stukken/4_Simulation model/model/"

# Load the INFLUENCE risk matrix
inf_matrix <- read.csv("INFLUENCE_matrix.csv", sep=";")

# Load the CBS mortaility data (based on 2022) - STERFTEKANS PER leeftijd
mortality_data <- read.csv("cbs_sterftekans18-100.csv",sep = ";")

# Load the NKR variables from 2017-2019
nkr_variables <- read.csv("nkr_variables.txt", sep=";")
# Iterate over each column in the dataframe
for (col_name in names(nkr_variables)) {
  # Assign the column as a separate object in the environment
  assign(col_name, nkr_variables[[col_name]])
}

#Load NCR follow-up data symptomatic LRR
load("data_AEP.Rda")

#symptomatic LRR 
symp_cox_model <- coxph(Surv(time = dfi, event = symp) ~ vdt, data=df_patient, x=T)
