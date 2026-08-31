#Author: Laure Heidmann
#Name: 4_Evaluating_Substitution
#Description: Evaluating if the treatment effect is a true effect or a substitution effect
#Created: LH 17/07/2018
#Updated: LH 17/07/2018

#Name in old directory: 4_Evaluating_Substitution.R 

rm(list=ls())

library('this.path')
library('clubSandwich')
library('data.table')
library('stargazer')

script_dir <- dirname(normalizePath(this.path::this.path(), winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "00_paths.R"), local = TRUE)


source(file.path(path_functions, "AppendixI_Regression_Functions.R"), local = TRUE)
source(file.path(path_functions, "AppendixI_2_Treatments_Effects_New_Outcomes.R"), local = TRUE)

#1. Simple linear regression
#2. PROBIT regression


# using the observations from endline survey
endline_data <- fread(file.path(path_data_cleaned, "Prepared_Endline.csv"),header = TRUE, sep = ",", data.table = FALSE)

# selecting outcomes
outcomes <- c(paste0("atleast",c(2,3,4,5,6,7)), "atleastMeasles")
outcomes_label <- c(paste0("At Least ",c(2,3,4,5,6,7)), "Measles 1")
regression_title <- "Immunization Outcomes Restricted to Unmatched Children"

# taking into account only children that are old enough to get the number of vaccines in consideration
for (l in c(1:7,"Measles")){
  endline_data[which(endline_data[,paste0("age_vacc_",l)] != 1), paste0("atleast",l)] <- NA
}

#### 1. ALL SUB-TREATMENTS
directory <- paste0(path_tables, .Platform$file.sep)
invisible(running_new_outcomes(endline_data[which(endline_data$matched==0),], outcomes, outcomes_label, regression_title, directory, 1))
