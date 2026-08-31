#Author: Laure Heidmann
#Name: 2_Treatments_Effects_New_Outcomes
#Description: Run the analysis of the treatments effects on the new outcomes
#Created: LH 16/07/2018
#Updated: LH 25/07/2018

#Organization
#1. Function to compute regressions
#2. Exhaustive list of outcomes
#3. Selected outcomes according to summary stats

source(file.path(path_functions, "AppendixI_Regression_Functions.R"), local = TRUE)


### all outcomes are expressed in 0 No and 1 Yes except:
### symptom_last: from 1 (few hours) to 5 (more than 5 days)
### inconvenient_get_immunized: from 1 (not at all incovenient) to 6 (very inconvenient)
### vaccine_benefits: from 1 (very harmful) to 5 (very beneficial)
### last_camp and next_camp: from 1 (days) to 3 (months) (plus 4 no camp for next_camp)



# using the observations from endline survey
#endline_data <- fread(paste0(path_input_data,"/Prepared_Endline.csv"),header = TRUE, sep = ",", data.table = FALSE)



#### 1. FUNCTION TO COMPUTE REGRESSIONS

# writing the functions to run regressions
running_new_outcomes <- function(endline_data, outcomes, outcomes_label, regression_title, directory, number){
  
  treatments <- c("highslope", "highflat", "lowslope", "lowflat")
  controls <-  c("random", "gossip", "trusted", "trustgossip", "trfrac33_first", "trfrac66_first")
  controls_label <- "Communication and Reminders"
  
  regression(endline_data, outcomes, outcomes_label, "id_village_grp", "Village (SC) Level", controls, controls_label,
             treatments, sapply(treatments, give_label), regression_title, directory, number, ".1", "District")
  
  treatments <- c("random", "gossip", "trusted", "trustgossip")
  controls <-  c("highslope", "highflat", "lowslope", "lowflat", "trfrac33_first", "trfrac66_first")
  controls_label <- "Incentives and Reminders"
  
  regression(endline_data[which(endline_data$seedsrisk == 1),], outcomes, outcomes_label, "id_village_grp", "Village (SC) Level", 
             controls, controls_label, treatments, sapply(treatments, give_label), regression_title, directory, number, ".2", "District")
  
  treatments <- c("trfrac33_first", "trfrac66_first")
  controls <-  c("highslope", "highflat", "lowslope", "lowflat", "random", "gossip", "trusted", "trustgossip")
  controls_label <- "Incentives and Communication"
  
  regression(endline_data, outcomes, outcomes_label, "id_village_grp", "Village (SC) Level", controls, controls_label, 
             treatments, sapply(treatments, give_label), regression_title, directory, number, ".3", "District")
  
  
}

# writing the functions to run regressions
running_new_outcomes_no_control <- function(endline_data, outcomes, outcomes_label, regression_title, directory, number){
  
  endline_data$fes <- 1 
  
  treatments <- c("highslope", "highflat", "lowslope", "lowflat")
  controls <-  c("")
  controls_label <- "Communication and Reminders"
  
  regression(endline_data, outcomes, outcomes_label, "id_village_grp", "Village (SC) Level", controls, controls_label,
             treatments, sapply(treatments, give_label), regression_title, directory, number, ".1", "No")
  
  treatments <- c("random", "gossip", "trusted", "trustgossip")
  controls <-  c("")
  controls_label <- "Incentives and Reminders"
  
  regression(endline_data[which(endline_data$seedsrisk == 1),], outcomes, outcomes_label, "id_village_grp", "Village (SC) Level", 
             controls, controls_label, treatments, sapply(treatments, give_label), regression_title, directory, number, ".2", "No")
  
  treatments <- c("trfrac33_first", "trfrac66_first")
  controls <-  c("")
  controls_label <- "Incentives and Communication"
  
  regression(endline_data, outcomes, outcomes_label, "id_village_grp", "Village (SC) Level", controls, controls_label, 
             treatments, sapply(treatments, give_label), regression_title, directory, number, ".3", "No")
  
  
}



# 
# #### 2. EXHAUSTIVE LIST OF OUTCOMES
# 
# directory <- paste0(globdir,"/Analysis/Endline_Analysis/4_Results/Exhaustive_Results/")
# 
# # selecting new outcomes
# inconvenience_immunization <- colnames(select(endline_data, ends_with("_after_vaccination"), ends_with("symptom_last")))
# reason_no_immunization <- colnames(select(endline_data, ends_with("_issue"), ends_with("inconvenient_get_immunized")))
# awareness_camp <- colnames(select(endline_data, starts_with("camp_aware"), ends_with("last_camp"), ends_with("next_camp")))
# information_camp <- colnames(select(endline_data, ends_with("inform_camp"), ends_with("have_informed_people_camp")))
# knowledge_immunization <- colnames(select(endline_data, ends_with("inform_immunization"), ends_with("number_vaccines_recommended")))
# attitude_immunization <- colnames(select(endline_data, starts_with("vaccine_")))
# shared_attitude_immunization <- colnames(select(endline_data, ends_with("share_view"), ends_with("people_around_immunized")))
# 
# new_outcomes <- list(inconvenience_immunization, reason_no_immunization, awareness_camp, information_camp, 
#                      knowledge_immunization, attitude_immunization, shared_attitude_immunization)
# new_outcomes_names <- c("Inconvenience of Immunization", "Reasons against Immunization", "Awareness of Immunization Camp",
#                         "Information about Immunization Camp", "Knowledge about Immunization", "Attitude towards Immunization",
#                         "Surrounding Opinions on Immunization")
# 
# # running regression for every outcome
# i <- 1
# for (outcomes in new_outcomes){
#   
#   # checking that every outcome is numeric
#   for (col in outcomes){
#     print(col)
#     print(typeof(endline_data[,col]))
#     print(table(endline_data[,col]))
#   }
#   
#   regression_title <- new_outcomes_names[round((i+0.2)/2,0)]
#   outcomes_label <- str_ucfirst(gsub("_"," ",outcomes))
#   outcomes_label <- gsub("Immunization","Immun.",outcomes_label)
#   outcomes_label <- gsub("Recommended","Recomm.",outcomes_label)
#   outcomes_label <- paste0("\\thead{",gsub(" ","\\\\\\\\",outcomes_label),"}")
# 
#   running_new_outcomes(endline_data, outcomes, outcomes_label, regression_title, directory, i)
#   running_new_outcomes(endline_data[which(endline_data$matched==1),], outcomes, outcomes_label, 
#                        paste(regression_title,"Restricted to Matched Children"), directory, i+1)
#   i <- i+2
# }


# 
# 
# #### 3. SELECTED OUTCOMES ACCORDING TO SUMMARY STATS
# 
# directory <- paste0(globdir,"/Analysis/Endline_Analysis/4_Results/Summarized_Results/")
# # if you want to get the tables in html, uncomment the following line and the two last functions of Regression_Functions.R
# #directory <- paste0(globdir,"/Analysis/Endline_Analysis/4_Results/Summarized_Results/In_html/")
# 
# information_camp <- c("camp_frequency", "camp_aware_someone", "camp_aware_anm", "have_informed_people_camp", "asha_inform_camp", "aganwadi_inform_camp", "neighbor_inform_camp")
# knowledge_immunization <- c("experience_inform_immunization", "asha_inform_immunization", "anm_inform_immunization", "number_vaccines_recommended", "neighbor_share_view")
# attitude_immunization <- c("no_issue","child_discomfort_issue","inconvenient_get_immunized","vaccine_protect", "vaccine_immune","vaccine_good_for_health", "vaccine_no_harm", "vaccine_benefits")
# 
# new_outcomes <- list(information_camp, knowledge_immunization, attitude_immunization)
# new_outcomes_names <- c("Information about Immunization Camp", "Knowledge about Immunization", "Attitude towards Immunization")
# 
# # running regression for every outcome
# i <- 1
# j <- 1
# for (outcomes in new_outcomes){
#   
#   # checking that every outcome is numeric
#   for (col in outcomes){
#     print(col)
#     print(typeof(endline_data[,col]))
#     print(table(endline_data[,col]))
#   }
#   
#   regression_title <- new_outcomes_names[j]
#   outcomes_label <- str_ucfirst(gsub("_"," ",outcomes))
#   outcomes_label <- gsub("Immunization","Immun.",outcomes_label)
#   outcomes_label <- gsub("Recommended","Recomm.",outcomes_label)
#   outcomes_label <- paste0("\\thead{",sub(" ","\\\\\\\\",outcomes_label),"}")
#   outcomes_label <- gsub("Have\\\\\\\\Informed","Have Informed\\\\\\\\",outcomes_label)
#   
#   
#   # running regressions on different sets of data
#   running_new_outcomes(endline_data, outcomes, outcomes_label, regression_title, directory, i)
#   #running_new_outcomes(endline_data[which(endline_data$matched==1),], outcomes, outcomes_label, paste(regression_title,"Restricted to Matched Children"), directory, i+1)
#   #running_new_outcomes(endline_data[which(endline_data$id_district!=3),], outcomes, outcomes_label, paste(regression_title,"Restricted to non Mewat"), directory, i+2)
#   #i <- i+3
#   i <- i+1
#   j <- j+1
# }
# 
# 