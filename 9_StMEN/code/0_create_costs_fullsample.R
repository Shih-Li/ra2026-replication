#######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  CEA Analysis: Create shots/dollar variables for the different outcomes
# AUTHOR:		Anirudh Sankar
# CREATED:	long ago (2017?)
# MODIFIED: 
# STATUS: 	Draft

#Name in old directory: create_costs_fullsample.R

#######################################################################################

#This code file builds on the CEA spreadsheet file that can be found in the Smart Pooling / Haryana folder
#The key inputs from this sheet are:

# ASHA incentives for mobilization:	100	2017INR
# ASHA incentives for FIC	150:	2017INR
# AVD: 75	2017INR
# Cost per BCG vaccine	3	2017INR
# Cost per Penta vaccines	137	2017INR
# Cost per measles vaccine	33	2017INR
# Cost per 5 mL syringe	2	2017INR
# Cost per 0.5 mL syringe	2	2017INR
# Cost per 0.1 mL syringe	3	2017INR
# Low slope incentives, vaccines 1-3	10 2017INR
# Low slope incentives, vaccine 4	60	2017INR
# Low slope incentives, vaccine 5	160	2017INR
# High slope incentives, vaccines 1-3	50	2017INR
# High slope incentives, vaccine 4	100	2017INR
# High slope incentives, vaccine 5	200	2017INR
# Cost per SMS (regular)	0,48	2017 INR
# Cost per SMS (seed)	0,72	2017 INR
# Cost per voice call 	0,35	2017 INR

#################################################################################
#
# 0.ENVIRONMENT SET UP
#
#################################################################################

rm(list = ls())

library('data.table')
library('dplyr')
library('clubSandwich')
library('stargazer')
library('Hmisc')
library('readstata13')
library('car')
library('miceadds')
library('multiwayvcov')
library('estimatr')
library('lme4')
library('this.path')

script_dir <- dirname(normalizePath(this.path::this.path(), winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "00_paths.R"))
#path_LM = "~/Dropbox (MIT)/Smart Pooling and Pruning/Haryana"

#path = path_LM    #change this
#setwd(path)


#################################################################################
#
# I.Analysis setup
#
#################################################################################


#--READ VILLAGEXMONTH dataset
villagexmonth_level <- fread(file.path(path_data_raw, "Tablet_VillageXMonth.csv"), header = TRUE, sep = ",", data.table = FALSE)


#--expanded dummies
villagexmonth_level$random[villagexmonth_level$seedsrisk == 0] <- 0
villagexmonth_level$gossip[villagexmonth_level$seedsrisk == 0] <- 0
villagexmonth_level$trusted[villagexmonth_level$seedsrisk == 0] <- 0
villagexmonth_level$trustgossip[villagexmonth_level$seedsrisk == 0] <- 0
villagexmonth_level$commtreat_arm[villagexmonth_level$seedsrisk == 0] <- 0


villagexmonth_level$SMSblast <- as.numeric(villagexmonth_level$trfrac33_first | villagexmonth_level$trfrac66_first)

villagexmonth_level <- villagexmonth_level %>% filter(!is.na(shot_Measles1))


#################################################################################
#
# II.CEA Functions
#
#################################################################################


find_fixed_cost <- function(row, at_scale = FALSE){
  if (row["commtreat_arm"] != 0) { #networks expt 
    if (row["commtreat_arm"] != 1) { #nominated seed
      if (at_scale == TRUE) {
        return(3.66)
      }
      return(14.79)
    }
    #else, a random seed experiment
    if (at_scale == TRUE) {
      return(3.62)
    }
    return(14.75)
  }
  #else, not a networks experiment
  if(row["commtreat_arm"] == 0 & row["inctreat_arm"] == 0 & row["reminder_control_first"] == 1){ #control group
    if (at_scale == TRUE) {
      return(3.48)
    }
    return(14.69)
  } 
  #else, not in control group
  if (at_scale == TRUE) {
    return(3.56)
  }
  return(14.69)
}

find_variable_cost <- function(row, usd = TRUE){
#This function returns the variable cost for measles shots delivered in a certain
#village, in a certain month, assuming that Penta1, Penta2, Penta3 were also
#delivered (no BCG: We should discount BCG because they 
# get that at birth. Thats why I add up 4 vaccine costs rather than 5)
  
  exchange_rate = 65.12
  variable = 0 #going to be adding to this 
  num_measles = as.integer(row["shot_Measles1"])
  
  if (row["inctreat_arm"] == 1) { #incentives = highslope 
    variable = variable + 200 * num_measles + 50*num_measles*2 + 100 * num_measles #assuming they got 1 of each previous vaccine
  }
  
  #if incentives = highflat
  if (row["inctreat_arm"] == 2 ) {
    variable = variable + 90 * num_measles *4
  }
  
  #if incentives = lowslope
  if (row["inctreat_arm"] == 3 ) {
    variable = variable + 160 * num_measles + 10* num_measles*2 + 60 * num_measles
  }
  # if incentives = lowflat
  
  if (row["inctreat_arm"] == 4 ) {
    variable = variable + 50 * num_measles*4
  }
  
  if (row["trfrac33_first"] == 1) { #if SMS33 == 1 
    variable = variable + (0.48 + 0.35) * num_measles * 0.33 * 3 #SMS + voice call
  }
  
  if (row["trfrac66_first"] == 1) { #if SMS66 == 1
    variable = variable + (0.48 + 0.35) * num_measles * 0.66 * 3
  }
  
  #syringes
  variable = variable + 2 * num_measles * 4
  
  #vaccines
  variable = variable + 33 * num_measles + 137 * num_measles*3
  
  #ASHA mobilization
  variable = variable + 100 * num_measles * 4
  
  #ASHA incentive for FIC
  variable = variable + 150 * num_measles
  
  #AVD 
  variable = variable + 75 * num_measles * 4
  
  if (usd == TRUE) {
    variable = variable/exchange_rate
  }
  
  return(variable)
}


#################################################################################
#
# III. Add variables to data
#
#################################################################################

#Measles
villagexmonth_level$fixed_cost <- apply(villagexmonth_level, 1, FUN = function(x) find_fixed_cost(x, at_scale = TRUE))
villagexmonth_level$variable_cost <- apply(villagexmonth_level, 1, FUN = function(x) find_variable_cost(x, usd = TRUE))
villagexmonth_level$total_cost <- villagexmonth_level$fixed_cost + villagexmonth_level$variable_cost
villagexmonth_level$shots_per_dollar <- villagexmonth_level$shot_Measles1/ villagexmonth_level$total_cost
write.csv(villagexmonth_level, file.path(path_processed_data, "Tablet_VillageXMonth_Costs.csv"))

