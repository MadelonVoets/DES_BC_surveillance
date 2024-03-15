## 1. INITIALIZATION ----

# Clear the workspace
rm(list=ls()); gc();

# Load the required packages 
library(simmer);                 #Version 4.4.6.3
library(simmer.plot);            #Version 0.1.18
library(dplyr);                  #Version 1.1.4
library(rsvg);                   #Version 2.6.0
library(readxl)
library(readr)
#library(DiagrammeRsvg)

sessionInfo()

# Load funtions for extracting monitored attributes
source("getSingleAttribute.R", echo=TRUE);
source("getMultipleAttributes.R", echo=TRUE);

#rel_path <= "~/AMICUS/Stukken/4_Simulation model/model/"

# Load the INFLUENCE risk matrix
inf_matrix <- read_excel("INFLUENCE_matrix.xlsx")
mortality_data <- read_delim("cbs_sterftekans18-100.csv", 
                             + delim = ";", escape_double = FALSE, trim_ws = TRUE)
mortality_data <- as.data.frame(mortality_data)
