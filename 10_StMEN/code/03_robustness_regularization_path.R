#######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Perform Main Analysis: Smart Pooling and Pruning / Backward selection procedure
# AUTHOR:		Anirudh Sankar
# CREATED:	02/11/2021
# MODIFIED: 
# STATUS: 	Draft

#Name in old directory: 02_main.R

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
library('janitor')
library('tidyverse')
library('truncnorm')
library('this.path')

script_dir <- dirname(normalizePath(this.path::this.path(), winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "00_paths.R"), local = TRUE)
#################################################################################
#
# I.ANALYSIS SET UP
#
#################################################################################
#outcome = "shot_Measles1"
outcome = "shots_per_dollar"


#--READ VILLAGEXMONTH dataset
villagexmonth_level <- fread(file.path(path_processed_data, "Tablet_VillageXMonth_Costs.csv"),header = TRUE, sep = ",", data.table = FALSE)


#--SELECT SEEDS RISK EXPT
villagexmonth_level <- villagexmonth_level %>% filter(seedsrisk == 1)

#--SELECT ONLY FIRST IMPLEMENTATION
villagexmonth_level <- villagexmonth_level %>% filter(first_implementation == 1)


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

#################################################################################
#
# II.MAIN ANALYSIS
#
#################################################################################

# ######################
#A) -- LASSO SELECTION
# ######################

variables <- grep("^SP", names(villagexmonth_level), value = TRUE)
variables <- variables[variables != "SP_noSeedXnoIncentiveXnoReminder"]

variables_expanded <- c(variables, colnames(fes_dummies))


current_variables <- variables_expanded

deselect_list <- c()
deselect_pval <- c()

pval_cutoff <- 5 * 10^(-13) #cutoff used for the paper
#pval_cutoff <- 5*10^(-11)    #this cutoff selects the gossips (all), no incentive, SMS (all) policy

#source(file.path(path_functions, "pval_lambda_mapping_functions.R"))
#equivalent_lambda <- get_lambda_from_p(pval_cutoff)
#pval_cutoff <- 5 * 10^(-8)

current_sp_formula <- as.formula(paste0(outcome,"~",paste0(current_variables,collapse = "+")))
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
  current_model_ols <- estimatr::lm_robust(formula = current_sp_formula, data = villagexmonth_level, weights = village_population, se_type = "classical")
  current_max_pval <- max(current_model_ols$p.value)
  
  cat("\n\nCurrent max p-val = \t",current_max_pval)
  cat("\n Nb variables left = \t", length(current_variables))
}

support_SP <- variables_expanded[!(variables_expanded %in% deselect_list)]

toRemove <- grep(pattern = "ntercept",x = support_SP) #don't want intercept to be lasso selected!
if (length(toRemove) > 0) {
  support_SP <- support_SP[-toRemove]
}


fes_chosen <- grep("^X", support_SP, value = TRUE)
support_SP_policies <- grep("^X", support_SP, value = TRUE, invert = TRUE)

# ########################
#B) -- AUTOMATED POOLING
# ########################

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

source(file.path(path_functions, "map_key_policy_names.R"), local = TRUE)

# ######################
#C) -- POST LASSO
# ######################

variables_pooled_expanded <- c(pooled_policies,fes_chosen)

formula_pl <- as.formula(paste0(outcome,"~",paste0(variables_pooled_expanded ,collapse = "+")))
model_pl <- estimatr::lm_robust(formula = formula_pl, data = final_data, clusters = id_sc, weights = village_population, se_type = "CR0")

pl_names <- c()
for (policy in pooled_policies) {
  pl_names <- c(pl_names, policy_name_mapping[[policy]])
}
#pl_names <- 1:length(pooled_policies)
pl_effects <- model_pl$coefficients[pooled_policies]
pl_pval <-model_pl$p.value[pooled_policies]
pl_conf_high <- model_pl$conf.high[pooled_policies]
pl_conf_low <- model_pl$conf.low[pooled_policies]

#pl_disp <- data.frame(names(pl_effects), pl_effects, pl_pval)
pl_disp <- data.frame(pl_names, pl_effects, pl_pval, pl_conf_high, pl_conf_low)
colnames(pl_disp)[1] <- "pl_names"

pl_disp = pl_disp %>%
  mutate(pl_names = sub(",", ",\n",pl_disp$pl_names))

#Set plot parameters
y_expand = switch(outcome, "shot_Measles1" = 5, "shots_per_dollar" = 0.01)
y_lab = switch(outcome,"shot_Measles1" = "Effect on Measles Vaccination", 
               "shots_per_dollar" = "Effect on Measles Vaccination/$")
geom_text_round =  switch(outcome, "shot_Measles1" = 2, "shots_per_dollar" = 4)
geom_text_y = switch(outcome, "shot_Measles1" = 0.3, "shots_per_dollar" = 0.0007)
save_title = switch(outcome,  "shot_Measles1" = "postLASSO-immunizations.png", "shots_per_dollar" = "postLASSO-costeffectiveness.png")
save_title2 = switch(outcome,  "shot_Measles1" = "postLASSO-immunizations_COEF.png", "shots_per_dollar" = "postLASSO-costeffectiveness_COEF.png")
panel_title = switch(outcome, "shot_Measles1" = "A: Measles Shots", "shots_per_dollar" = "B: Shots per dollar")

# ######################
# -- PLOT RESULTS
# ######################

#Bar plot
pl_graph <- ggplot(data=pl_disp, aes(x=pl_names, y= pl_effects)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x=element_text(size=11, angle=90,hjust=0.99,vjust=0.3))+
  geom_bar(stat="identity", fill='#A4A4A4') +
  expand_limits(y = y_expand)+
  ggtitle("Individual Policy Effects") +
  xlab("Policy") +
  ylab(y_lab) +
  geom_text(aes(label= round(pl_effects,geom_text_round)), position=position_dodge(width=0.9), vjust=-0.30, fontface = "bold")+
  geom_text(aes(label= paste0("p = ",format(round(pl_pval,digits = 3), nsmall = 3)), y=geom_text_y), position=position_dodge(width=0.9), size = 3) +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90))

#ggsave(paste0(path_figures,"/",save_title), plot = pl_graph, width = 10, height = 8)





#Coef plot
pl_graph2 <- ggplot(data=pl_disp, aes(x=pl_names, y= pl_effects)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin= pl_conf_high, ymax= pl_conf_low), width=.2,lwd = 1.5) + 
  geom_hline(yintercept = 0, linetype = "dashed",color = "#A4A4A4",lwd = 1.5) + 
  xlab("Policy") +
  ylab(y_lab) +
  ggtitle(panel_title) +
  geom_text(aes(label= round(pl_effects,geom_text_round), x = pl_names),hjust = -0.3,vjust = -0.3, fontface = "bold", size = 5.5)+
  geom_text(aes(label= paste0("(p = ",format(round(pl_pval,digits = 3), nsmall = 3), ")"), x = pl_names),hjust = -0.1,vjust = 1.5,size = 5) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        axis.text.x = element_text(size=15, angle = 20, hjust=0.7, vjust = 0.8),
        axis.title.y = element_text(size = 15))+
  theme(plot.title = element_text(face = "bold", size = 17))

ggsave(file.path(path_figures, save_title2), plot = pl_graph2, width = 14, height = 8)



#control mean

pure_control <- final_data %>% filter(incentive_control == 1 & communication_control == 1 & reminder_control_first == 1)

control <- final_data
for (pol in support_SP_policies) {
  control <- control %>% filter(UQ(as.name(pol))  == 0)
}


pure_control_mean <- weighted.mean(pure_control[,outcome], pure_control$village_population)

control_mean <- weighted.mean(control[,outcome], control$village_population)

# #########################
#D) -- ANDREWS ESTIMATOR
# #########################

source(file.path(path_functions, "inference_on_winners_functions.R"), local = TRUE)


alpha = 0.05
beta = 0.005

ntreat <- length(pooled_policies) + 1

pol_best_name <- names(which(pl_effects == max(pl_effects)))
pol_2nd_name <- names(which.max(pl_effects[pl_effects!=max(pl_effects)]))

best_scaled_effect <- sqrt(nobs(model_pl)) * pl_effects[pol_best_name]
trunc_scaled_effect <- max(0, sqrt(nobs(model_pl)) * pl_effects[pol_2nd_name]) #in case

var_around_best <- nobs(model_pl) * (model_pl$std.error[pol_best_name])^2

hybrid_results_scaled <- get_hybrid_Y_alpha_beta_custom(best_scaled_effect, trunc_scaled_effect, var_around_best, ntreat, alpha, beta)

#printed values
pol_best_name_neat <- policy_name_mapping[[pol_best_name]]
hybrid_results <- (1/sqrt(nobs(model_pl))) * hybrid_results_scaled
nobs_bestpolicy <- sum(final_data[,pol_best_name] == 1)

writeLines(c(paste("best policy raw name:", pol_best_name), paste("best policy short name: ", pol_best_name_neat), paste("ATE:", hybrid_results[1]), paste("CI lower:", hybrid_results[2]),paste("CI upper:", hybrid_results[3]), paste("control mean: ", pure_control_mean), paste("nobs: ", nobs_bestpolicy)), file.path(path_tables, paste0(outcome, "_best_policy_WC_adjusted.txt")))




#################################################################################
#
# END
#
#################################################################################

