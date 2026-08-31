#Author: Laure Heidmann
#Name: Regression_Functions
#Description: Compute regression functions useful for analysis
#Created: LH 17/05/2018
#Updated: LH 25/07/2018
#Updated: AS 25/07/2018

# writing the table in LateX
writing_in_latex <- function(model_list, se_list, cmean_list, nna_list, nzeros_list, outcomes_label, cluster_label, controls, 
                             controls_label, treatments_label, regression_title, directory, number, panel, fixed_effects){
  
  controls_note <- "\\\\ - All specifications include a full Set of Controls for"
  # in case of no control from other treatment
  if (controls[1] == ""){
    controls <- "Constant"
    controls_note <- ""
  }
  
  # managing the column width depending on the number of variable
  column_width <- "0.25pt"
  if (length(outcomes_label)<4){
    column_width <- "17pt"
  }
  
  writeLines(capture.output(stargazer(model_list,
                                      se = se_list,
                                      title=regression_title,
                                      dep.var.labels=outcomes_label,
                                      covariate.labels = c(treatments_label),
                                      single.row=(length(treatments_label)>13),
                                      multicolumn = FALSE,
                                      column.sep.width = column_width,
                                      font.size = "scriptsize",
                                      omit = c(controls, "fes", "Constant", "population"),
                                      omit.stat = "all",
                                      add.lines = list(c("Control Mean", cmean_list),
                                                       c("Total Obs.", nna_list),
                                                       c("Zeros Replaced", nzeros_list)),
                                      notes.align = "r",
                                      notes = paste("\\parbox[t]{\\textwidth}{- All specifications include", fixed_effects ,"Fixed Effects", 
                                                    controls_note, controls_label,
                                                    "\\\\ - For outcomes expressed in logs, -Inf replaced with 0
                                                    \\\\ - Control mean shown in levels
                                                    \\\\ - Standard Errors Clustered at the", cluster_label,"}"))), 
             paste0(directory, "table",number, panel,".tex"))
  
}

# Variant of writing_in_latex() if using the Double Lasso procedure to select controls
writing_in_latex_bhc <- function(model_list, se_list, cmean_list, nna_list, nzeros_list, outcomes_label, cluster_label, controls, 
                                 controls_label, treatments, treatments_label, regression_title, directory, number, panel, fixed_effects){
  
  controls_note <- "\\\\ - All specifications include a full Set of Controls for"
  # in case of no control from other treatment
  if (controls[1] == ""){
    controls <- "Constant"
    controls_note <- ""
  }
  
  # managing the column width depending on the number of variable
  column_width <- "0.25pt"
  if (length(outcomes_label)<7){
    column_width <- "17pt"
  }
  
  writeLines(capture.output(stargazer(model_list,
                                      se = se_list,
                                      title=regression_title,
                                      dep.var.labels=outcomes_label,
                                      covariate.labels = c(treatments_label),
                                      single.row=(length(treatments_label)>13),
                                      multicolumn = FALSE,
                                      column.sep.width = column_width,
                                      font.size = "scriptsize",
                                      keep = treatments, 
                                      keep.stat = "n",
                                      omit.stat = "n",
                                      add.lines = list(c("Control Mean", cmean_list),
                                                       c("Total Obs.", nna_list),
                                                       c("Zeros Replaced", nzeros_list)),
                                      notes.align = "r",
                                      notes = paste("\\parbox[t]{\\textwidth}{- All specifications include", fixed_effects, "fixed effects and BHC double post lasso selected village controls", 
                                                    controls_note, controls_label,
                                                    "\\\\ - For outcomes expressed in logs, -Inf replaced with 0
                                                    \\\\ - Control mean shown in levels
                                                    \\\\ - Standard Errors Clustered at the", cluster_label,"}"))), 
             paste0(directory, "table",number, panel,".tex"))
  
}


# writing in xlsx the coefficients of the regressions
writing_xlsx <- function(regression, data_level, treatments, outcomes, directory, regression_title){
  
  table_results <- data.frame(matrix(0, ncol = (2+length(treatments)), nrow = length(outcomes)))
  colnames(table_results) <- c("coefficient","control_mean",treatments)
  table_results$coefficient <- outcomes
  
  for (i in 1:length(outcomes)){
    table_results[i,"control_mean"] <- computing_control_mean(data_level, outcomes[i], treatments)
    for (treat in treatments){
      u <- model$model[[1]]
      table_results[i,treat] <- round(regression$model[[i]]$coefficients[treat],4)
    }
  }
  
  table_name <- gsub(" ","_",regression_title)
  write.xlsx(table_results, paste0(directory,table_name,".xlsx"))
}



# finding the right control group to compute the control mean and the number of observations in the control group
computing_control_mean <- function(data_level, y, treatments){
  
  # keeping 2 decimal digits for individual level outcomes mean and 0 for other
  #decimal_digit <- (!is.null(data_level$Gender) || !is.null(data_level$weight_endline)) * 2
  
  decimal_digit <- 2
  
  # using incentive control group by default
  cmean <- round(mean(data_level[which(data_level$incentive_control == 1), gsub("ln_","",y)], na.rm = TRUE), decimal_digit)
  ncontrol <- nrow(data_level[which((data_level$incentive_control == 1)&(!is.na(data_level[,y]))),])
  
  # using communication control group if the treatment under study is communication
  if (("random"%in%treatments)|("gossip"%in%treatments)){
    # using communication control group at the village level restricted to seed risk set
    if (identical(data_level,data_level[which(data_level$seedsrisk == 1),])){
      cmean <- round(mean(data_level[which(data_level$communication_control == 1), gsub("ln_","",y)], na.rm = TRUE), decimal_digit)
      ncontrol <- nrow(data_level[which((data_level$communication_control == 1)&(!is.na(data_level[,y]))),])
    }
    
    # using communication control group for the any seed restricted set
    if (identical(data_level,data_level[which(data_level$anyseed == 1),])){
      cmean <- round(mean(data_level[which(data_level$random == 1), gsub("ln_","",y)], na.rm = TRUE), decimal_digit)
      ncontrol <- nrow(data_level[which((data_level$random == 1)&(!is.na(data_level[,y]))),])
    }
  }
  
  # using global reminders control group if the treatment under study is reminders global effect
  if ("trfrac33_first"%in%treatments){
    cmean <- round(mean(data_level[which(data_level$reminder_control_first == 1), gsub("ln_","",y)], na.rm = TRUE), decimal_digit)
    ncontrol <- nrow(data_level[which((data_level$reminder_control_first == 1)&(!is.na(data_level[,y]))),])
  }
  
  # using individual reminders control group if the treatment under study is reminders individual effect
  if ("trindiv_treat_sub"%in%treatments){
    cmean <- round(mean(data_level[which(data_level$trindiv_treat_sub == 0), gsub("ln_","",y)], na.rm = TRUE), decimal_digit)
    ncontrol <- nrow(data_level[which((data_level$trindiv_treat_sub == 0)&(!is.na(data_level[,y]))),])
  }
  
  # if there is only one treatment using the mean when the treatment is zero
  if (length(treatments)==1){
    cmean <- round(mean(data_level[which(data_level[,treatments[1]] == 0), gsub("ln_","",y)], na.rm = TRUE), decimal_digit)
    ncontrol <- nrow(data_level[which((data_level[,treatments[1]] == 0)&(!is.na(data_level[,y]))),])
  }
  
  # not keeping ncontrol but we can change that
  control_results <- list("cmean" = cmean, "ncontrol" = ncontrol)
  return(cmean)
}



# computing the regression
running_regression <- function(data_level, y, variables, cluster, treatments){

  # writing the model
  formule <- as.formula(paste("data_level[,y]", variables, sep = "~"))
  
  print(formule)
  
  # running the regression
  # and weighting the observations for the endline analysis with weight_seeds
  # and weighting the observations for the village level communication experiment with weight_seeds
  if (is.null(data_level$weight_endline) & is.null(data_level$weight_seeds)){
    model <- lm(formule, data = data_level)
  }else if (!is.null(data_level$weight_endline)){
    model <- lm(formule, data = data_level, weights = data_level$weight_endline)
  } else if (!is.null(data_level$weight_seeds)) {
    model <- lm(formule, data = data_level, weights = data_level$weight_seeds)
  }
  
  # computing robust standard errors
  cluster_robust <- coef_test(model, vcov = "CR1", cluster = data_level[, cluster], test = "naive-t")$SE
  
  # computing control mean (taking always the mean of the outcomes in level)
  cmean <- computing_control_mean(data_level, y, treatments)

  # computing the number of replaced -Inf for the log outcomes
  nzeros <- grepl("ln_", y) * length(which(data_level[,gsub("ln_","",y)]==0))
  
  # computing number of non missing observations
  nna <- sum(!is.na(data_level[,y]))
  
  regression <- list("model" = model, "cluster_robust" = cluster_robust, "cmean" = cmean, "nna" = nna, "nzeros" = nzeros)
  return(regression)
}



# running the analysis by outcomes
regression <- function(data_level, outcomes, outcomes_label, cluster, cluster_label, controls, controls_label, 
                       treatments, treatments_label, regression_title, directory, number, panel, fixed_effects){
  
  # writing all the variables
  variables <- paste(c(treatments, controls, "factor(fes)") , sep = "", collapse = " + ")
  # removing fixed effects if they are constant trough all the observations
  if (length(unique(data_level$fes))==1){
    if(controls != c("")){
      variables <- paste(c(treatments, controls) , sep = "", collapse = " + ")}
    else{
      variables <- paste(c(treatments) , sep = "", collapse = " + ")}
  }
  
  # initializing the tools
  model_list <- list()
  se_list <- list()
  cmean_list <- c()
  nna_list <- c()
  nzeros_list <- c()
  
  # running the regression for each outcome
  for (y in outcomes){
    print(y)
    print(treatments)
    
    regression <- running_regression(data_level, y, variables, cluster, treatments)
    
    model_list <- append(model_list, list(regression$model))
    se_list <- append(se_list, list(regression$cluster_robust))
    cmean_list <- append(cmean_list, regression$cmean)
    nna_list <- append(nna_list, regression$nna)
    nzeros_list <- append(nzeros_list, regression$nzeros)
    
  }
  
  # writing the lateX file
  writing_in_latex(model_list, se_list, cmean_list, nna_list, nzeros_list, outcomes_label, cluster_label, controls, 
                   controls_label, treatments_label, regression_title, directory, number, panel, fixed_effects)
  
  # returning the coefficients
  results <- list("model" = model_list, "cluster_robust" = se_list)
  return(results)
}



# running the analysis by quantile
regression_by_quantile <- function(data_level, criteria, criteria_label, quantile, outcomes, outcomes_label, 
                                           cluster, cluster_label, controls, controls_label, treatments, treatments_label, 
                                           regression_title, directory, number, fixed_effects){
  
  # defining the values of the quantiles taken into account
  lower_quantile <- quantile(data_level[,criteria], quantile)
  upper_quantile <- quantile(data_level[,criteria], (1-quantile))
  
  # defining the regression title
  if (criteria %in% c("Previous_Vaccines", "Previous_Injections","Mother_Vaccines")){
    table_tile1 <- paste0(regression_title," for Low ",criteria_label," (",quantile*100, "\\% of observations: $\\leq$", round(lower_quantile,0),")")
    table_tile2 <- paste0(regression_title," for High ",criteria_label," (",quantile*100, "\\% of observations: $>$", round(upper_quantile,0),")")
  }else {
    table_tile1 <- paste0(regression_title," for Low ",criteria_label," (",quantile*100, "\\% of observations: $\\leq$", round(lower_quantile,2)*100,"\\%)")
    table_tile2 <- paste0(regression_title," for High ",criteria_label," (",quantile*100, "\\% of observations: $>$", round(upper_quantile,2)*100,"\\%)")
  }
  
  # running for the lower quantile
  low_quantile <- regression(data_level[which(data_level[,criteria] <= lower_quantile),], outcomes, outcomes_label, cluster, cluster_label,
                             controls, controls_label, treatments, treatments_label, table_tile1, directory, number, "low", fixed_effects)
  
  # running for the higher quantile
  high_quantile <- regression(data_level[which(data_level[,criteria] > upper_quantile),], outcomes, outcomes_label, cluster, cluster_label, 
                              controls, controls_label, treatments, treatments_label, table_tile2, directory, number, "high", fixed_effects)
  
  # returning the results
  results <- c("low_quantile" = low_quantile, "high_quantile" = high_quantile)
  return(results)
}



# writing outcomes and treatments labels
give_label <- function(variable){
  # writing outcomes labels
  if(variable == "ln_num_main_shots"){return("\\thead{Log\\\\ Vaccine Given}")}
  if(variable == "ln_fully_immunized"){return("\\thead{Log\\\\ Fully Immunized}")}
  if(variable == "ln_nchildren"){return("\\thead{Log\\\\ Children}")}
  if(variable == "ln_nkidsvillage"){return("\\thead{Log\\\\ Children}")}
  if(variable == "ln_shot_Penta1"){return("\\thead{Log\\\\ Shot Penta1}")}
  if(variable == "ln_shot_Penta2"){return("\\thead{Log\\\\ Shot Penta2}")}
  if(variable == "ln_shot_Penta3"){return("\\thead{Log\\\\ Shot Penta3}")}
  if(variable == "ln_shot_Measles1"){return("\\thead{Log\\\\ Shot Measles1}")}
  if(variable == "num_main_shots"){return("Vaccine Given")}
  if(variable == "fully_immunized"){return("Fully Immunized")}
  if(variable == "nchildren"){return("Children")}
  if(variable == "nkidsvillage"){return("Children")}
  if(variable == "shot_Penta1"){return("Shot Penta1")}
  if(variable == "shot_Penta2"){return("Shot Penta2")}
  if(variable == "shot_Penta3"){return("Shot Penta3")}
  if(variable == "shot_Measles1"){return("Shot Measles1")}
  # writing treatments labels
  if(variable == "highslope"){return("High Slope")}
  if(variable == "highflat"){return("High Flat")}
  if(variable == "lowslope"){return("Low Slope")}
  if(variable == "lowflat"){return("Low Flat")}
  if(variable == "slope"){return("Slope")}
  if(variable == "flat"){return("Flat")}
  if(variable == "incentive"){return("Incentive")}
  if(variable == "high"){return("High")}
  if(variable == "low"){return("Low")}
  if(variable == "random"){return("Random")}
  if(variable == "gossip"){return("Gossip")}
  if(variable == "trusted"){return("Trusted")}
  if(variable == "trustgossip"){return("Trusted Gossip")}
  if(variable == "nominated"){return("Nominated Seed")}
  if(variable == "trfrac33_first"){return("33\\% First Impl.")}
  if(variable == "trfrac66_first"){return("66\\% First Impl.")}
  if(variable == "trindiv_treat_sub"){return("Individual Treat.")}
  if(variable == "hindu_binary"){return("Hindu Majority")}
  if(variable == "has_hra"){return("HRA")}
  if(variable == "population"){return("population")}
  if(variable == "reminders"){return("Reminders")}
  else{return(variable)}
}



# writing controls labels
give_control_label <- function(controls){
  if(controls ==""){return("")}
  if(("highslope" %in% controls)&("trfrac33_first"%in% controls)){return("Incentives and Reminders")}
  if(("highslope" %in% controls)&("gossip"%in% controls)){return("Incentives and Communication")}
  if(c("highslope") %in% controls){return("Incentives")}
  if(c("trfrac33_first") %in% controls){return("Reminders")}
  if(c("gossip") %in% controls){return("Communication")}
}



# running different specifications
run_specifications <- function(data_level, outcomes, treatments, cluster, cluster_label, controls, controls_label, fixed_effects,
                               regression_title, directory, number, treatments_variations, controls_variations){
  
  # running baseline specification
  reg <- regression(data_level, outcomes, sapply(outcomes, give_label), cluster, cluster_label, controls, controls_label,
                    treatments, sapply(treatments, give_label), regression_title, directory, number, ".0", fixed_effects)
  
  # writing the results in a xlsx file for the baseline specification only
  #writing_xlsx(reg, data_level, treatments, outcomes, directory, regression_title)

  i <- 1
  # running variations with different groups of the treatment
  for (treatments_v in treatments_variations){
    regression(data_level, outcomes, sapply(outcomes, give_label), cluster, cluster_label, controls, controls_label,
               treatments_v, sapply(treatments_v, give_label), regression_title, directory, number, paste0(".",i), fixed_effects)
    i <- i+1
  }

  # running variations with different sets of controls
  for (controls_v in controls_variations){

    # removing PHC fixed effects
    if (controls_v[1]=="-phc"){
      if (is.null(data_level$created_year)){
        data_level$fes <- group_indices(data_level, id_district)
        fixed_effects <- "District"
      }else{
        data_level$fes <- group_indices(data_level, id_district, created_year, created_month)
        fixed_effects <- "District-Time"
      }
      controls_v <- controls
    }

    # adding PHC fixed effects
    if (controls_v[1]=="phc"){
      if (is.null(data_level$created_year)){
        data_level$fes <- group_indices(data_level, id_phc)
        fixed_effects <- "PHC"
      }else{
        data_level$fes <- group_indices(data_level, id_phc, created_year, created_month)
        fixed_effects <- "PHC-Time"
      }
      controls_v <- ""
    }

    regression(data_level, outcomes, sapply(outcomes, give_label), cluster, cluster_label, controls_v, give_control_label(controls_v),
               treatments, sapply(treatments, give_label), regression_title, directory, number, paste0(".",i), fixed_effects)
    i <- i+1
  }
}





# # if you want to export the tables in html, uncomment the following lines
# # writing the table in html
# writing_in_latex <- function(model_list, se_list, cmean_list, nna_list, nzeros_list, outcomes_label, cluster_label, controls, 
#                              controls_label, treatments_label, regression_title, directory, number, panel, fixed_effects){
#   
#   controls_note <- "- All specifications include a full Set of Controls for"
#   # in case of no control from other treatment
#   if (controls[1] == ""){
#     controls <- "Constant"
#     controls_note <- ""
#   }
#   
#   # managing the column width depending on the number of variable
#   column_width <- "0.25pt"
#   if (length(outcomes_label)<7){
#     column_width <- "17pt"
#   }
#   
#   stargazer(model_list,
#             se = se_list,
#             title=regression_title,
#             dep.var.labels=outcomes_label,
#             covariate.labels = c(treatments_label),
#             single.row=(length(treatments_label)>13),
#             multicolumn = FALSE,
#             column.sep.width = column_width,
#             type = "html",
#             font.size = "scriptsize",
#             omit = c(controls, "fes", "Constant", "population"), 
#             omit.stat = "all",
#             add.lines = list(c("Control Mean", cmean_list),
#                              c("Total Obs.", nna_list),
#                              c("Zeros Replaced", nzeros_list)),
#             notes.align = "r",
#             notes = paste("All specifications include", fixed_effects ,"Fixed Effects",
#                           controls_note, controls_label,
#                           ", For outcomes expressed in logs, -Inf replaced with 0, Control mean shown in levels, Standard Errors Clustered at the", 
#                           cluster_label),
#             out = paste0(directory, "table",number, panel,".htm"))
#   
# }
# 
# # giving the labels for html format
# give_label <- function(variable){
#   # writing outcomes labels
#   if(variable == "ln_num_main_shots"){return("Log Vaccine Given")}
#   if(variable == "ln_fully_immunized"){return("Log Fully Immunized")}
#   if(variable == "ln_nchildren"){return("Log Children")}
#   if(variable == "ln_nkidsvillage"){return("Log Children")}
#   if(variable == "ln_shot_Penta1"){return("Log Shot Penta1")}
#   if(variable == "ln_shot_Penta2"){return("Log Shot Penta2")}
#   if(variable == "ln_shot_Penta3"){return("Log Shot Penta3")}
#   if(variable == "ln_shot_Measles1"){return("Log Shot Measles1")}
#   if(variable == "num_main_shots"){return("Vaccine Given")}
#   if(variable == "fully_immunized"){return("Fully Immunized")}
#   if(variable == "nchildren"){return("Children")}
#   if(variable == "nkidsvillage"){return("Children")}
#   if(variable == "shot_Penta1"){return("Shot Penta1")}
#   if(variable == "shot_Penta2"){return("Shot Penta2")}
#   if(variable == "shot_Penta3"){return("Shot Penta3")}
#   if(variable == "shot_Measles1"){return("Shot Measles1")}
#   # writing treatments labels
#   if(variable == "highslope"){return("High Slope")}
#   if(variable == "highflat"){return("High Flat")}
#   if(variable == "lowslope"){return("Low Slope")}
#   if(variable == "lowflat"){return("Low Flat")}
#   if(variable == "slope"){return("Slope")}
#   if(variable == "flat"){return("Flat")}
#   if(variable == "incentive"){return("Incentive")}
#   if(variable == "high"){return("High")}
#   if(variable == "low"){return("Low")}
#   if(variable == "random"){return("Random")}
#   if(variable == "gossip"){return("Gossip")}
#   if(variable == "trusted"){return("Trusted")}
#   if(variable == "trustgossip"){return("Trusted Gossip")}
#   if(variable == "nominated"){return("Nominated Seed")}
#   if(variable == "trfrac33_first"){return("33% First Impl.")}
#   if(variable == "trfrac66_first"){return("66% First Impl.")}
#   if(variable == "trindiv_treat_sub"){return("Individual Treat.")}
#   if(variable == "hindu_binary"){return("Hindu Majority")}
#   if(variable == "has_hra"){return("HRA")}
#   if(variable == "population"){return("population")}
# }

