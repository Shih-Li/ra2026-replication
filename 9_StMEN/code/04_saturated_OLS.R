######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Plot coefficients of saturated regression
# AUTHOR:		Anirudh Sankar & Louis-Mael Jean & Elsa Trezeguet
# CREATED:	02/11/2021
# MODIFIED: 04/11/2021
# STATUS: 	Draft

#Name in old directory: 04_saturated_OlS.R
#######################################################################################


#################################################################################
#
# 0.SET UP
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
library('ggplot2')
library('tidyverse')
library('pracma')
library('gtools')
library('glmnet')
library('purrr')
library('hdm')
library('pracma')
library('RColorBrewer')
library('scales')
library('ggpubr')
library('this.path')
library('broom')

script_dir <- dirname(normalizePath(this.path::this.path(), winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "00_paths.R"), local = TRUE)
source(file.path(path_functions, "simulation_functions.R"), local = TRUE)
source(file.path(path_functions, "crossprod_functions.R"), local = TRUE)
source(file.path(path_functions, "saturated_OLS_functions.R"), local = TRUE)



#################################################################################
#
# II.RUN ANALYSIS
#
#################################################################################

#Open Dataset
villagexmonth_level <- fread(file.path(path_processed_data, "Tablet_VillageXMonth_Costs.csv"),header = TRUE, sep = ",", data.table = FALSE)
villagexmonth_level_2 <- fread(file.path(path_processed_data, "Tablet_VillageXMonth_Costs.csv"),header = TRUE, sep = ",", data.table = FALSE)

#Select seeds risk expt
villagexmonth_level <- filter(villagexmonth_level, seedsrisk == 1)
villagexmonth_level_2 <- villagexmonth_level_2 %>% filter(seedsrisk == 1)

#Select only first implementation
villagexmonth_level <- filter(villagexmonth_level, first_implementation == 1)
villagexmonth_level_2 <- villagexmonth_level_2 %>% filter(first_implementation == 1)

#Create and add FES dummies
villagexmonth_level_2 <- villagexmonth_level_2 %>%
  group_by(id_district, created_year, created_month) %>%
  mutate(fes = cur_group_id()) %>%
  ungroup()
fes_dummies <- data.frame(lme4::dummy(villagexmonth_level_2$fes))
villagexmonth_level_2 <- cbind(villagexmonth_level_2, fes_dummies)

# Create the treatment matrix
villagexmonth_level_2$noReminder <- villagexmonth_level_2$reminder_control_first
villagexmonth_level_2$noSeed <- villagexmonth_level_2$communication_control
villagexmonth_level_2$noIncentive <- villagexmonth_level_2$incentive_control

villagexmonth_level_2$SMSblast = as.numeric(villagexmonth_level_2$trfrac33_first | villagexmonth_level_2$trfrac66_first)
villagexmonth_level_2$highSMS = villagexmonth_level_2$trfrac66_first
villagexmonth_level_2$lowSMS = villagexmonth_level_2$trfrac33_first

for(seed in c("noSeed", "gossip", "random", "trusted", "trustgossip")){
  for(incentive in c("noIncentive","highflat","highslope", "lowflat", "lowslope")){
    for(reminder in c("noReminder", "lowSMS", "highSMS")){
      policy_string <- paste0("pol_",seed,"X",incentive, "X",reminder)
      villagexmonth_level_2[,policy_string] <- villagexmonth_level_2[,seed]* villagexmonth_level_2[,incentive]*villagexmonth_level_2[,reminder]  
    }
  }
}

variable_policies <- grep("^pol",names(villagexmonth_level_2), value = TRUE)

#Remove control: 
variable_policies <- variable_policies[variable_policies != "pol_noSeedXnoIncentiveXnoReminder"]
variable_policies_expanded <- c(variable_policies, colnames(fes_dummies))

###################################
######## Perform the OLS ##########
###################################
outcome = "shot_Measles1"
#outcome = "shots_per_dollar"

formula_raw <- as.formula(paste0(outcome,"~",paste0(variable_policies_expanded ,collapse = "+")))
model_raw <- estimatr::lm_robust(formula = formula_raw, data = villagexmonth_level_2, clusters = id_sc, weights = village_population, se_type = "CR0")
res <- tidy(model_raw)


pooled_policies <- final_pooled_policies(villagexmonth_level,outcome)
pooled_policies_cleaned = clean_pooled_policies(pooled_policies)




#################################################################################
#
# III. Create Graphs
#
#################################################################################

#### A. INDIVIDUAL LOLLIPOP GRAPHS ####
#Setup color legend that will be used to flag policies that were pooled together
myColors <- brewer.pal(5,"Set1")
#myColors <- pal_uchicago("default")(5)
names(myColors) <- c("Pruned", "Pooling 1", "Pooling 2", "Pooling 3", "Pooling 4")
colScale <- scale_colour_manual(name = "Legend",values = myColors)

# Loop across all treatment profiles and create graphs:
for(seed in c("noSeed", "gossip", "random", "trusted")){
  for(incentive in c("noIncentive","flat","slope")){
    for(reminder in c("noReminder", "SMS")){
      if(reminder == "noReminder" & incentive == "noIncentive" & seed == "noSeed"){ next }
      
      plot_title <- get_plot_title(seed,incentive,reminder)
      res_profile = res %>% filter(str_detect(term,seed)) %>% filter(str_detect(term,incentive)) %>%filter(str_detect(term,reminder)) #results just for given profile
      res_profile = tag_pooled_policies(res_profile, pooled_policies_cleaned) #create variable saying whether policy was pruned or pooled (and number different poolings)
      
      #Rename policies to have the form (seed,incentive,reminder)
      for(i in 1:length(res_profile$term)){
        s <- res_profile$term[i]
        res_profile$term[i] <- paste0("(",unlist(strsplit(substr(s,5,100),"X"))[1],", ",unlist(strsplit(substr(s,5,100),"X"))[2],",\n",unlist(strsplit(substr(s,5,100),"X"))[3], ")")
      }
      
      #Plot
      pdf(file.path(path_figures, paste0("pol_", incentive, "X", seed, "X", reminder, "_", outcome, ".pdf")), width = 10, height =8)
      plot(lollipop_plot(res_profile, plot_title))
      dev.off()
    }#close the reminder loop
  }#close the incentive loop
}#close the seed loop





##### B. COMBINED LOLLIPOP GRAPH #####

# Loop across all treatment profiles and create graphs:
for(seed in c("noSeed", "gossip", "random", "trusted")){
  
  plot_title <- switch(seed,
                       "noSeed" = "No Seed",
                       "gossip" = "Gossip Seed",
                       "random" = "Random Seed",
                       "trusted" = "Trusted Seed")
  
  res_seed = res %>% filter(str_detect(term,seed)) %>% mutate("Profile" = 0)
  res_seed = tag_pooled_policies(res_seed, pooled_policies_cleaned)#create variable saying whether policy was pruned or pooled (and number different poolings)
  
  #Group the policies by profile
  p = 1
  for(incentive in c("noIncentive","flat","slope")){
    for(reminder in c("noReminder", "SMS")){
      if(reminder == "noReminder" & incentive == "noIncentive" & seed == "noSeed"){ next }
      res_profile = res %>% filter(str_detect(term,seed)) %>% filter(str_detect(term,incentive)) %>%filter(str_detect(term,reminder)) #results just for given profile
      res_seed$Profile[match(res_profile$term,res_seed$term)] = p
      p = p + 1
    }
  }
  
  #Rename policies to have the form (seed,incentive,reminder)
  for(i in 1:length(res_seed$term)){
    s <- res_seed$term[i]
    res_seed$term[i] <- paste0("(",unlist(strsplit(substr(s,5,100),"X"))[1],", ",unlist(strsplit(substr(s,5,100),"X"))[2],",\n",unlist(strsplit(substr(s,5,100),"X"))[3], ")")
  }
  
  res_seed = res_seed[order(res_seed$Profile),]
  
  xlines = switch(seed,
                  "noSeed" = c(2.5,4.5,8.5,10.5),
                  "gossip" = c(2.5,6.5,10.5,18.5,22.5),
                  "random" = c(1.5,3.5,5.5,9.5,11.5),
                  "trusted" = c(1.5,3.5,5.5,9.5,11.5))
  
  ylims = switch(outcome,
                 "shot_Measles1" = c(-12,12),
                 "shots_per_dollar" = c(-0.032, 0.01))
  
  
  #Plot
  plot_name = paste0("p_",seed)
  
  
  
  p <- ggplot(res_seed, aes(x=fct_reorder(term, Profile), y=estimate, color = Legend)) +
    geom_point(size = 2.5) +
    geom_segment(aes(x = term, y = estimate, xend = term, yend = 0), colour = "darkgray")+
    geom_errorbar(aes(ymin= conf.low, ymax= conf.high), width=.2, position=position_dodge(0.05), lwd = 0.8) +
    geom_vline(xintercept=xlines, linetype = "dashed", color = "#FFA319FF", lwd = 1) +
    scale_colour_manual(name = "Legend",values = myColors[1:length(unique(res_seed$Legend))]) + 
    xlab("Policy combinations") +
    ylab("Estimated effect") +
    ylim(ylims) + 
    ggtitle(plot_title) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 10), 
          panel.background = element_rect(fill = "white", colour = "grey50"),
          plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(size = 12),
          plot.margin = margin(t=5, r = 5, b = 5, l=20))
  
  
  ggsave(file.path(path_figures, paste0("OLS_Combined_seed=", seed, outcome, ".pdf")),plot=p, width = 18, height =10)
  assign(plot_name, p)
}#close the seed loop



combined <- ggarrange(p_noSeed,p_random,p_trusted,p_gossip, labels = c("A", "B", "C", "D"),nrow = 4)
ggsave(file.path(path_figures, paste0("OLS_Combined_", outcome, ".pdf")),plot=combined, width = 14, height =19)




##### C. HEATMAP OF COEFFICIENTS IN TWO DIMENSIONS #####

#myColors2 <- pal_uchicago("default")(5)
myColors2 <- c("#767676FF", "#800000FF","#FFA319FF", "#350E20FF", "#8A9045FF")
names(myColors2) <- c("Pruned", "Pooling 1", "Pooling 2", "Pooling 3", "Pooling 4")


create_heatmap <- function(seed){
  res_seed = res %>% filter(str_detect(term,seed))
  res_seed = tag_pooled_policies(res_seed, pooled_policies_cleaned)
  res_seed$term = res_seed$term %>% str_remove(paste0("pol_",seed,"X"))
  res_seed = res_seed %>% separate(term, c("Incentive", "Reminder"), sep = "X")
  res_seed$sig = ifelse(res_seed$p.value <= 0.05, 1,0)
  
  
  p <- ggplot(res_seed, aes(x=fct_relevel(Reminder, "noReminder", "lowSMS", "highSMS"), 
                            y=fct_relevel(Incentive, "noIncentive", "lowflat","highflat", "lowslope", "highslope"))) +
    geom_tile(aes(fill = estimate, colour = factor(Legend)), size = 1) +
    geom_point(data=filter(res_seed,sig==1),aes(x=fct_inorder(Reminder), y=fct_relevel(Incentive, "noIncentive", "lowflat","highflat", "lowslope", "highslope")),shape=8) +
    scale_fill_distiller(name = "Estimate", palette = "YlGnBu") +
    scale_colour_manual(name = "Legend",values = myColors2[1:length(unique(res_seed$Legend))]) + 
    #ggtitle(paste0("Estimates Heatmap for ",seed," Seed")) + 
    xlab("Reminder") +
    ylab("Incentive") +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))
  
  ggsave(file.path(path_figures, paste0("Heatmap_", seed, "_Seed_", outcome, ".pdf")), plot=p, width = 10, height =8)
}

create_heatmap("random")
create_heatmap("trusted")
create_heatmap("noSeed")
#create_heatmap("gossip")



##### D. COMBINED HEATMAP OF COEFFICIENTS IN TWO DIMENSIONS #####


for(seed in c("noSeed", "random", "trusted", "_gossip", "trustgossip")){
  res_seed = res %>% filter(str_detect(term,seed))
  if (seed == "_gossip"){
    seed= "gossip"
  }
  res_seed = tag_pooled_policies(res_seed, pooled_policies_cleaned)
  res_seed$term = res_seed$term %>% str_remove(paste0("pol_",seed,"X"))
  res_seed = res_seed %>% separate(term, c("Incentive", "Reminder"), sep = "X")
  res_seed$sig = ifelse(res_seed$p.value <= 0.05, 1,0)
  res_seed$seed = seed
  assign(paste0("res_",seed), res_seed)
}

res_seeds <- rbind(res_noSeed, res_random, res_trusted, res_gossip, res_trustgossip) %>% mutate(seed = factor(seed ,levels=c('trustgossip','gossip','trusted','random','noSeed')))

p_combined <- ggplot(res_seeds, aes(x=fct_relevel(Reminder, "noReminder", "lowSMS", "highSMS"), 
                                    y=fct_relevel(Incentive, "noIncentive", "lowflat","highflat", "lowslope", "highslope"))) +
  geom_tile(aes(fill = estimate, colour = factor(Legend)), size = 1) +
  geom_point(data=filter(res_seeds,sig==1),aes(x=fct_inorder(Reminder), y=fct_relevel(Incentive, "noIncentive", "lowflat","highflat", "lowslope", "highslope")),shape=8) +
  scale_fill_distiller(name = "Estimate", palette = "YlGnBu") +
  scale_colour_manual(name = "Legend",values = myColors2[1:length(unique(res_seed$Legend))], labels = c("Pruned", "Pooled")) + 
  xlab("Reminder") +
  ylab("Incentive") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        strip.text.y = element_text(size = 12, face = "bold")) +
  facet_grid(seed ~ .,labeller = labeller(seed = c("noSeed" = "No Seed", "random" = "Random", "trusted" = "Trusted", "gossip" = "Gossip", "trustgossip" = "Trust Gossip")))



ggsave(file.path(path_figures, paste0("Heatmap_Combined_", outcome, ".pdf")), plot=p_combined, width = 8, height =18)

#################################################################################
#
# END
#
#################################################################################