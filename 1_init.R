## 1. INITIALIZATION ----

# Clear the workspace
rm(list=ls()); gc();

# Load the required packages 
library(simmer);                 #Version 4.4.6.3
library(simmer.plot);            #Version 0.1.18
library(dplyr);                  #Version 1.1.4
library(rsvg);                   #Version 2.6.0
#library(DiagrammeRsvg)

sessionInfo()

# Load funtions for extracting monitored attributes
source("getSingleAttribute.R", echo=TRUE);
source("getMultipleAttributes.R", echo=TRUE);

# Load the dataset
load("")