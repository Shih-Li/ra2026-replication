######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Functions for the saturated_OLS.R file
# AUTHOR:		Anirudh Sankar & Louis-Mael Jean & Elsa Trezeguet
# CREATED:	02/11/2021

#######################################################################################



#################################################################################
#
# This file contains functions that are used in Heatmap.R:
#
#     - final_pooled_policies():  retrieves the final vector of policies pooled together by the algorithm
#                                 given data and an outcome variable
#
#     - clean_pooled_policies():  takes as input the vector of pooled policies and returns a list of lists
#                                 were policies are organized in pooling regions and displayed in the form
#                                 (seed, incentive, reminder)                 
#
#     - tag_pooled_policies():    creates a new variable to the results data called legend indicating wether
#                                 the policy was pruned or pooled
#
#     - get_plot_title():         given seed, incentive and reminder outputs the title of the plot
#
#     - lollipop_plot():          performs the coefficient plot
#
#
#################################################################################


final_pooled_policies <- function(villagexmonth_level, outcome){
  #This function just runs Main.R up to the point of creating pooled_policies
  #backward selection procedure
  #--CREATE ALL POLICIES -- 
  source(file.path(path_functions, "create_sp_variables.R"), local = TRUE)
  
  
  #District-Time FE
  villagexmonth_level <- villagexmonth_level %>%
    group_by(id_district, created_year, created_month) %>%
    mutate(fes = cur_group_id()) %>%
    ungroup()
  
  #create FES dummies
  
  fes_dummies <- data.frame(lme4::dummy(villagexmonth_level$fes))
  
  villagexmonth_level <- cbind(villagexmonth_level, fes_dummies)
  
  #-- LASSO SELECTION
  
  variables <- grep("^SP", names(villagexmonth_level), value = TRUE)
  variables <- variables[variables != "SP_noSeedXnoIncentiveXnoReminder"]
  
  variables_expanded <- c(variables, colnames(fes_dummies))
  
  
  current_variables <- variables_expanded
  
  deselect_list <- c()
  deselect_pval <- c()
  
  pval_cutoff <- 5 * 10^(-13)
  
  current_sp_formula <- as.formula(paste0(outcome,"~",paste0(current_variables,collapse = "+")))
  #print(current_sp_formula)
  current_model_ols <- estimatr::lm_robust(formula = current_sp_formula, data = villagexmonth_level, weights = village_population, se_type = "classical")
  current_max_pval <- max(current_model_ols$p.value)
  
  print(current_max_pval)
  
  
  while (current_max_pval > pval_cutoff) {
    
    deselect_name <- names(current_model_ols$p.value[which.max(current_model_ols$p.value)])
    deselect_list <- c(deselect_list, deselect_name)
    deselect_pval <- c(deselect_pval, current_model_ols$p.value[which.max(current_model_ols$p.value)])
    
    #print(current_model_ols$p.value[which.max(current_model_ols$p.value)])
    #print(deselect_name)
    
    current_variables <- current_variables[current_variables != deselect_name]
    current_sp_formula <- as.formula(paste0(outcome,"~",paste0(current_variables,collapse = "+")))
    #print(current_sp_formula)
    current_model_ols <- estimatr::lm_robust(formula = current_sp_formula, data = villagexmonth_level, weights = village_population, se_type = "classical")
    
    current_max_pval <- max(current_model_ols$p.value)
    print(current_max_pval)
  }
  
  support_SP <- variables_expanded[!(variables_expanded %in% deselect_list)]
  
  toRemove <- grep(pattern = "ntercept",x = support_SP) #don't want intercept to be lasso selected!
  if (length(toRemove) > 0) {
    support_SP <- support_SP[-toRemove]
  }
  
  
  fes_chosen <- grep("^X", support_SP, value = TRUE)
  support_SP_policies <- grep("^X", support_SP, value = TRUE, invert = TRUE)
  
  # 
  # 
  # # -- AUTOMATED POOLING
  # 
  source(file.path(path_functions, "pooling_functions.R"), local = TRUE)
  
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
                             
                             c("random", "flat", "SMS"), #seeds and incnetives (flats) and SMS
                             c("trusted", "flat", "SMS"),
                             c("gossip", "flat", "SMS"))
  
  final_data <- villagexmonth_level
  
  for (tp_name in treatment_profiles) {
    tp_support <- get_relevant_sp_in_tp(support_SP, tp_name)
    final_data <- add_pooled_policies_tp(final_data, tp_support, tp_name)
  }
  
  pooled_policies <- grep("^POOLED_", colnames(final_data), value = TRUE)
  
  return(pooled_policies)
}


#------------------------------------------------------------------------------#

clean_pooled_policies <- function(pooled_policies) {
  #This function takes the pooled_policy output of "Main.R" and creates a list where policies are named following the pattern
  #(Seed,Incentive, Reminder)
  
  pooled_policies_cleaned <- pooled_policies %>% str_remove("POOLED_") 
  pooled_policies_cleaned <- pooled_policies_cleaned %>% str_split("OR")
  for (i in 1:length(pooled_policies_cleaned)){
    pieces = pooled_policies_cleaned[[i]]  %>% str_split("X")
    for (j in 1:length(pieces)){
      specific_policy = pieces[[j]]
      specific_policy = specific_policy %>% str_replace_all("^(.*)([0-9])", "\\2\\1") #This line puts numbers in first position
      specific_policy = specific_policy %>% str_replace("3","high") %>% str_replace("2","low") %>% str_replace("1","") %>% str_replace("0","")
      specific_policy = specific_policy %>% str_replace("highgossip","trustgossip") %>% str_replace("lowgossip","gossip") %>% 
        str_replace("noSMS","noReminder") %>% str_replace("noslope","noIncentive") %>% str_replace("noflat","noIncentive") %>% 
        str_replace("notrusted","noSeed") %>% str_replace("nogossip","noSeed") %>% str_replace("norandom","noSeed")
      pieces[[j]] = paste0("(",specific_policy[1], ", ", specific_policy[2],", ",specific_policy[3],")")
    }
    pooled_policies_cleaned[[i]] = pieces
  }
  return(pooled_policies_cleaned)
}

#------------------------------------------------------------------------------#


tag_pooled_policies <- function(results, pooled_policies_cleaned){
  #This function takes an OLS results dataset and creates a variable "Legend" that is either "Pruned" or
  #"Pooled n" where n is used to distinguish different poolings within the dataset
  
  #This first section creates a temporary variable temp to store policy names of the form "(seed,incentive,reminder)"
  for(i in 1:length(results$term)){
    s <- results$term[i]
    results$temp[i] <- paste0("(",unlist(strsplit(substr(s,5,100),"X"))[1],", ",unlist(strsplit(substr(s,5,100),"X"))[2],", ",unlist(strsplit(substr(s,5,100),"X"))[3], ")")
  }
  results$Legend = "Pruned"     #create Legend variable
  pooled = "no"                 #currently no pooling has been spotted
  pool_count= 1                 #initialize pooling counter to 1
  
  for (c in 1:length(pooled_policies_cleaned)){       #loop over all pooling  regions
    for (policy in pooled_policies_cleaned[[c]]){     #loop over all policies within a pooling region
      if(policy %in% results$temp){                   #if pooled policy is found in dataset
        results["Legend"][results["temp"] == policy] = paste0("Pooling ",pool_count)    #update Legend to Pooling n 
        pooled = "yes"                                #pooling has been found so counter will be increased
      }
    }
    if (pooled=="yes"){
      pool_count = pool_count + 1   #increase counter
      pooled = "no"
    }
  }
  results <- subset(results, select = -c(temp))   #delete temporary naming
  return(results)
}


#------------------------------------------------------------------------------#

get_plot_title <-function(seed,incentive,reminder){
  if(reminder == "noReminder"){
    if(incentive == "noIncentive"){
      if(seed == "gossip"){       #Case 1: No reminder, no Incentive, any gossip
        plot_title <- paste0("Results of the naive OLS for the treatment profile (no Incentive, any ",seed, ", no Reminder)")
      }
      else{     #Case 2: No reminder, no Incentive, seed (other than gossip)
        plot_title <- paste0("Results of the naive OLS for the treatment profile (no Incentive, ",seed, ", no Reminder)")
      }
    }
    else if (incentive == "flat"){
      if(seed == "gossip"){#Case 3: No reminder, Any flat incentive, Any gossip
        plot_title <- paste0("Results of the naive OLS for the treatment profile (any flat Incentive, any ",seed, ", no Reminder)")
      }
      else {        #Case 4: No reminder; Any flat incentive, seed (other than gossip)
        plot_title <- paste0("Results of the naive OLS for the treatment profile (any flat Incentive, ",seed, ", no Reminder)")
      }
    }
    else if(incentive == "slope"){
      if(seed=="gossip"){#Case 5: No reminder, Any slope incentive, Any gossip
        plot_title <- paste0("Results of the naive OLS for the treatment profile (any slope Incentive, any ",seed, ", no Reminder)")
      }
      else{#Case 6: No reminder, Any slope incentive, seed (other than gossip)
        plot_title <- paste0("Results of the naive OLS for the treatment profile (any slope Incentive, ",seed, ", no Reminder)")
      }
    }
  }
  else if (incentive =="noIncentive"){
    if(seed=="gossip"){#Case 7: Any reminder, no incentive, any gossip
      plot_title <- paste0("Results of the naive OLS for the treatment profile (no Incentive, any ",seed, ", any Reminder)")
    }
    else{#Case 8: Any reminder, no incentive, seed (other than gossip)
      plot_title <- paste0("Results of the naive OLS for the treatment profile (no Incentive, ",seed, ", any Reminder)")
    }
  }
  else if (incentive == "flat"){
    if(seed=="gossip"){#Case 9: Any reminder, Any flat incentive, Any gossip
      plot_title <- paste0("Results of the naive OLS for the treatment profile (any flat Incentive, any ",seed, ", any Reminder)")
    }
    else{#Case 10: Any reminder, Any flat incentive, seed (other than gossip)
      plot_title <- paste0("Results of the naive OLS for the treatment profile (any flat Incentive, ",seed, ", any Reminder)")
    }
  }
  else if (incentive == "slope"){
    if(seed=="gossip"){#Case 11: Any reminder, Any slope incentive, Any gossip
      plot_title <- paste0("Results of the naive OLS for the treatment profile (any slope Incentive, any ",seed, ", any Reminder)")
    }
    else{#Case 12: Any reminder, Any slope incentive, Seed (other than gossip)
      plot_title <- paste0("Results of the naive OLS for the treatment profile (any slope Incentive, ",seed, ", any Reminder)")
    }
  }
  return(plot_title)
}


#------------------------------------------------------------------------------#


lollipop_plot <- function(res_profile, plot_title){
  p <- ggplot(res_profile, aes(x=fct_inorder(term), y=estimate, color = Legend)) +
    geom_point() +
    geom_segment(aes(x = term, y = estimate, xend = term, yend = 0), colour = "darkgray")+
    geom_errorbar(aes(ymin= conf.low, ymax= conf.high), width=.1,
                  position=position_dodge(0.05)) +
    colScale +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
          panel.background = element_rect(fill = "white", colour = "grey50"),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8)) +
    ylim(-12,12) +             #uncomment for Measles1 outcome
    xlab("Policy combinations") +
    ylab("Estimated effect of the policy combination") +
    ggtitle(plot_title)
  return(p)
}


######################################################################################
#
# END
#
#######################################################################################   

