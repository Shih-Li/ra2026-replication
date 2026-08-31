#######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Perform main analysis on bootstrapped samples
# AUTHOR:		Louis-Mael Jean & Elsa Trezeguet
# CREATED:	08/11/2021
# MODIFIED: 
# STATUS: 	Draft

#Name in old directory: 05_bootstrap_main.R 
#######################################################################################



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
library('hdm')
library('lme4')
library('stringr')
library('hash')
library('this.path')

script_dir <- dirname(normalizePath(this.path::this.path(), winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "00_paths.R"), local = TRUE)
set.seed(NULL)
set.seed(534)

#################################################################################
#
# I.ANALYSIS SET UP
#
#################################################################################
source(file.path(path_functions, "pooling_functions.R"), local = TRUE)
source(file.path(path_functions, "map_key_policy_names.R"), local = TRUE)
source(file.path(path_functions, "inference_on_winners_functions.R"), local = TRUE)

outcomes <- c("shot_Measles1", "shots_per_dollar")

for (outcome in outcomes) {
  cat("\n\n", paste(rep("=", 78), collapse = ""), "\n", sep = "")
  cat("BOOTSTRAP OUTCOME:", outcome, "\n")
  cat(paste(rep("=", 78), collapse = ""), "\n", sep = "")
  
  pval_cutoff = 5 * 10^(-13)
  
  # Prepare data
  villagexmonth_level <- fread(file.path(path_processed_data, "Tablet_VillageXMonth_Costs.csv"),header = TRUE, sep = ",", data.table = FALSE)
  villagexmonth_level <- villagexmonth_level %>% filter(seedsrisk == 1)
  villagexmonth_level <- villagexmonth_level %>% filter(first_implementation == 1)
  
  #Create all policies
  source(file.path(path_functions, "create_sp_variables.R"), local = TRUE)
  
  #Create District-Time FE and add their dummies to dataset
  villagexmonth_level <- villagexmonth_level %>%
    group_by(id_district, created_year, created_month) %>%
    mutate(fes = cur_group_id()) %>%
    ungroup()
  fes_dummies <- data.frame(lme4::dummy(villagexmonth_level$fes))
  villagexmonth_level <- cbind(villagexmonth_level, fes_dummies)
  
  variables <- grep("^SP", names(villagexmonth_level), value = TRUE)
  variables <- variables[variables != "SP_noSeedXnoIncentiveXnoReminder"]
  variables_expanded <- c(variables, colnames(fes_dummies))
  
  #This is used in the get_Haryana_pooled_policies function
  treatment_profiles <- list(c("random", "noIncentive", "noReminder"), #seeds only
                             c("trusted", "noIncentive", "noReminder"),
                             c("gossip", "noIncentive", "noReminder"),
                             
                             c("noSeed", "slope", "noReminder"), #incentives only
                             c("noSeed", "flat", "noReminder"),
                             
                             c("noSeed", "noIncentive", "SMS"), #SMS only
                             
                             c("random", "slope", "noReminder"), #seeds and incentives (slopes) only
                             c("trusted", "slope", "noReminder"),
                             c("gossip", "slope", "noReminder"),
                             
                             c("random", "flat", "noReminder"), #seeds and incentives (flats) only
                             c("trusted", "flat", "noReminder"),
                             c("gossip", "flat", "noReminder"),
                             
                             c("random", "noIncentive", "SMS"), #seeds and SMS only
                             c("trusted", "noIncentive", "SMS"),
                             c("gossip", "noIncentive", "SMS"),
                             
                             c("noSeed", "slope", "SMS"), #incentives (slope) and SMS only
                             c("noSeed", "flat", "SMS"), #incentives (flats) and SMS only
                             
                             c("random", "slope", "SMS"), #seeds and incentives (slopes) and SMS
                             c("trusted", "slope", "SMS"),
                             c("gossip", "slope", "SMS"),
                             
                             c("random", "flat", "SMS"), #seeds and incentives (flats) and SMS
                             c("trusted", "flat", "SMS"),
                             c("gossip", "flat", "SMS"))
  
  
  #################################################################################
  #
  # II.FUNCTIONS: This section is a functionalisation of main.R
  #
  #################################################################################
  
  smart_pooling_and_pruning <- function(df){
    
    #Initialise Backward Elimination Procedure
    current_variables <- variables_expanded
    deselect_list <- c()
    deselect_pval <- c()
    
    current_sp_formula <- as.formula(paste0(outcome,"~",paste0(current_variables,collapse = "+")))
    current_model_ols <- estimatr::lm_robust(formula = current_sp_formula, data = df, weights = village_population, se_type = "classical")
    current_max_pval <- max(current_model_ols$p.value)
    cat("\tInitial max p_val = ", current_max_pval)
    
    # Get support
    while (current_max_pval > pval_cutoff) {
      deselect_name <- names(current_model_ols$p.value[which.max(current_model_ols$p.value)])
      deselect_list <- c(deselect_list, deselect_name)
      deselect_pval <- c(deselect_pval, current_model_ols$p.value[which.max(current_model_ols$p.value)])
      
      current_variables <- current_variables[current_variables != deselect_name]
      current_sp_formula <- as.formula(paste0(outcome,"~",paste0(current_variables,collapse = "+")))
      current_model_ols <- estimatr::lm_robust(formula = current_sp_formula, data = villagexmonth_level, weights = village_population, se_type = "classical")
      current_max_pval <- max(current_model_ols$p.value)
    }
    cat("\n\tNb variables in support = ",length(current_variables))
    support_SP <- variables_expanded[!(variables_expanded %in% deselect_list)]
    
    toRemove <- grep(pattern = "ntercept",x = support_SP) #don't want intercept to be lasso selected!
    if (length(toRemove) > 0) {
      support_SP <- support_SP[-toRemove]
    }
    
    fes_chosen <- grep("^X", support_SP, value = TRUE)
    support_SP_policies <- grep("^X", support_SP, value = TRUE, invert = TRUE)
    
    # Get pooled policies
    final_data <- df
    
    for (tp_name in treatment_profiles) {
      tp_support <- get_relevant_sp_in_tp(support_SP, tp_name)
      final_data <- add_pooled_policies_tp(final_data, tp_support, tp_name)
    }
    
    pooled_policies <- grep("^POOLED_", colnames(final_data), value = TRUE)
    
    # Get best coef and andrew estimator
    variables_pooled_expanded <- c(pooled_policies,fes_chosen)
    
    formula_pl <- as.formula(paste0(outcome,"~",paste0(variables_pooled_expanded ,collapse = "+")))
    model_pl <- estimatr::lm_robust(formula = formula_pl, data = final_data, clusters = id_sc, weights = village_population, se_type = "CR0")
    
    pl_names <- c()
    for (policy in pooled_policies) {
      pl_names <- c(pl_names, policy_name_mapping[[policy]])
    }
    
    pl_effects <- model_pl$coefficients[pooled_policies]
    pl_pval <-model_pl$p.value[pooled_policies]
    
    pl_disp <- data.frame(pl_names, pl_effects, pl_pval)
    #control mean
    
    pure_control <- final_data %>% filter(incentive_control == 1 & communication_control == 1 & reminder_control_first == 1)
    
    control <- final_data
    for (pol in support_SP_policies) {
      control <- control %>% filter(UQ(as.name(pol))  == 0)
    }
    
    pure_control_mean <- weighted.mean(pure_control[,outcome], pure_control$village_population)
    control_mean <- weighted.mean(control[,outcome], control$village_population)
    
    # Andrew estimator
    alpha = 0.05
    beta = 0.005
    
    ntreat <- length(pooled_policies) + 1
    
    pol_best_name <- names(which(pl_effects == max(pl_effects)))
    pol_2nd_name <- names(which.max(pl_effects[pl_effects!=max(pl_effects)]))
    best_scaled_effect <- sqrt(nobs(model_pl)) * pl_effects[pol_best_name]
    trunc_scaled_effect <- max(0, sqrt(nobs(model_pl)) * pl_effects[pol_2nd_name]) #in case
    
    var_around_best <- nobs(model_pl) * (model_pl$std.error[pol_best_name])^2
    
    hybrid_results_scaled <- get_hybrid_Y_alpha_beta_custom(best_scaled_effect, trunc_scaled_effect, var_around_best, ntreat, alpha, beta)
    unbiased_results_scaled <- get_perfectly_unbiased_custom(best_scaled_effect, trunc_scaled_effect, var_around_best, ntreat, alpha)
    
    hybrid_results <- (1/sqrt(nobs(model_pl))) * hybrid_results_scaled
    unbiased_results <- (1/sqrt(nobs(model_pl))) * unbiased_results_scaled
    
    output <- list("support_SP_policies" = support_SP_policies, "post_lasso_data" = pl_disp, "best_coef" = hybrid_results, "best_policy" = policy_name_mapping[[pol_best_name]])
    return(output)
  }
  
  
  
  #################################################################################
  #
  # III.MAIN ANALYSIS
  #
  #################################################################################
  
  # #--------------------------#
  #A) -- PREPARING INITIAL DATA
  # #--------------------------#
  
  initial_output          <- smart_pooling_and_pruning(villagexmonth_level)
  initial_support_indices <- initial_output$support_SP_policies %>% match(variables)
  initial_post_lasso_data <- initial_output$post_lasso_data %>% mutate("sample" = 0, "support_indices" = paste(initial_support_indices,collapse=" "))
  initial_best_coef       <- initial_output$best_coef
  initial_best_policy     <- initial_output$best_policy
  
  #Create a variable policy_strata tagging all distinct policies
  villagexmonth_level$strata_name <- NA
  for(var in variables){
    villagexmonth_level[villagexmonth_level[,var] == 1,]$strata_name <- var
  }
  
  villagexmonth_level <- villagexmonth_level %>%
    group_by(across(all_of(variables))) %>%
    mutate(policy_strata = cur_group_id()) %>%
    ungroup()
  
  # #--------------------------#
  #B) -- BOOTSTRAPPING
  # #--------------------------#
  
  nsamples = 200
  
  # This technique uses the slice_sample() function implemented in dplyr in order
  # to sample with replacement the dataset by group (each policy combination corresponds to one group)
  
  bootstrapped_datasets = list()
  bootstrapped_best_coef = c()
  bootstrapped_best_policy_name = c()
  simulations_data <- initial_post_lasso_data
  best_pol_count = 0
  best_efficient_pol_count = 0 
  
  for (n in 1:nsamples){
    cat("\n\nBootstrapped sample ", n, "\n")
    #bootstrapped_datasets[[n]] = stratified(villagexmonth_level, "policy_strata", 0.999999, replace = TRUE)                     #Technique 1
    bootstrapped_datasets[[n]] = villagexmonth_level %>% group_by(policy_strata) %>% slice_sample(prop = 1, replace = TRUE)      #Technique 2  
    
    output          <- data.frame(bootstrapped_datasets[[n]]) %>% smart_pooling_and_pruning()
    support_indices <- output$support_SP_policies %>% match(variables)
    post_lasso_data <- output$post_lasso_data %>% mutate("sample" = n, "support_indices" = paste(support_indices,collapse=" "))
    best_coef       <- output$best_coef[1]
    best_policy     <- output$best_policy
    
    simulations_data <- rbind(simulations_data,post_lasso_data) #append to dataset
    bootstrapped_best_coef = c(bootstrapped_best_coef,best_coef)
    bootstrapped_best_policy_name = c(bootstrapped_best_policy_name,best_policy)
    
    if (identical(best_policy, initial_best_policy)){
      best_pol_count = best_pol_count + 1
    }
    
  }
  
  best_policies_data <- data.frame(bootstrapped_best_policy_name,bootstrapped_best_coef)
  best_policy_accuracy = best_pol_count / nsamples
  
  
  
  # #--------------------------#
  #C) -- EXPORT RESULTS
  # #--------------------------#
  
  #This is creating the support_category variable used for plots
  
  simulations_data = simulations_data  %>% mutate("support_category" = 0) #create support_category variable
  
  #The next steps are here to make sure the bootstrapped samples are numbered in frequency order (happy to get more efficient code here !)
  support_categories <- as.data.frame(simulations_data %>% group_by(support_indices) %>% tally(sort=TRUE))
  distinct_supports = dim(support_categories)[1]
  for (j in 1:distinct_supports){
    support = support_categories[j,1]
    simulations_data[simulations_data$support_indices == support,]$support_category <- j
  }
  simulations_data[simulations_data$sample == 0,]$support_category <- 0 #Set initial support_category to be 0
  
  #This line is just adding the true winner's curse adjusted to the dataset and creating a dummy flagging it
  best_policies_data = best_policies_data %>% 
    mutate("Is_true" = 0) %>% 
    rbind(data.frame("bootstrapped_best_policy_name" = c(initial_best_policy), "bootstrapped_best_coef" = c(initial_best_coef[1])) %>% 
            mutate("Is_true" = 1)) %>%
    mutate("best_policy_accuracy" = best_policy_accuracy,
           "outcome" = outcome)
  
  
  # -- Export Data
  write.csv(simulations_data, file.path(path_intermediate_data, paste0("Bootstrap_simulations_data_", outcome, ".csv")))
  write.csv(best_policies_data, file.path(path_intermediate_data, paste0("_best_policies_data_", outcome, ".csv")))
  
  cat("\nCompleted bootstrap outcome:", outcome, "\n")
}





#################################################################################
#
# END
#
#################################################################################