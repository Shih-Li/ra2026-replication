#######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Perform Main Analysis: Smart Pooling and Pruning
# AUTHOR:		Anirudh Sankar
# CREATED:	02/11/2021
# MODIFIED: 
# STATUS: 	Draft

#Name in old directory: 01_uninteracted_analysis.R
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
library('this.path')
library('ggplot2')

script_dir <- dirname(normalizePath(this.path::this.path(), winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "00_paths.R"))
#################################################################################
#
# I.ANALYSIS SET UP
#
#################################################################################

samples = c("full", "ambassador")

#villagexmonth_level <- fread(paste0(path_data,"/Tablet_VillageXMonth.csv"),header = TRUE, sep = ",", data.table = FALSE)
villagexmonth_level <- fread(file.path(path_processed_data, "Tablet_VillageXMonth_Costs.csv"),header = TRUE, sep = ",", data.table = FALSE)

villagexmonth_level <- villagexmonth_level %>% filter(first_implementation == 1)

#--DISTRICT-TIME FE
villagexmonth_level$fes <- villagexmonth_level %>%
  group_by(id_district, created_year, created_month) %>%
  group_indices()

villagexmonth_level$random<- villagexmonth_level$random
villagexmonth_level$random[villagexmonth_level$seedsrisk == 0] <- 0

villagexmonth_level$gossip <- villagexmonth_level$gossip
villagexmonth_level$gossip[villagexmonth_level$seedsrisk == 0] <- 0

villagexmonth_level$trusted <- villagexmonth_level$trusted
villagexmonth_level$trusted[villagexmonth_level$seedsrisk == 0] <- 0

villagexmonth_level$trustgossip <- villagexmonth_level$trustgossip
villagexmonth_level$trustgossip[villagexmonth_level$seedsrisk == 0] <- 0

villagexmonth_level$commtreat_arm<- villagexmonth_level$commtreat_arm
villagexmonth_level$commtreat_arm[villagexmonth_level$seedsrisk == 0] <- 0

for (sample in samples){
  
  if (sample == "full"){
    variables <- c("seedsrisk","factor(fes)")
    title = "Full Sample"
    
  } else if (sample == "ambassador"){
    villagexmonth_level <- villagexmonth_level %>% filter(seedsrisk == 1) #--SELECT ONLY SEEDS RISK EXPT
    variables <- c("factor(fes)")
    
    title = "Ambassador Sample"
  }

  treatments <- c("trfrac33_first","trfrac66_first","highflat", "lowflat", "highslope", "lowslope", "random","gossip","trusted","trustgossip")
  treatment_names <- c("SMS \n33%-level", "SMS \n66%-level","High \nFlat", "Low \nFlat", "High \nSlope", "Low \nSlope","Random \nPerson", "Information \nHub", "Trusted \nPerson", "Trusted \nInformation \nHub")
  
  rhs <- paste(c(treatments,variables) , sep = "", collapse = " + ")
  
  formule <- as.formula(paste0("shot_Measles1~",rhs))
  
  model <- estimatr::lm_robust(formula = formule, data = villagexmonth_level, clusters = id_sc, weights = village_population, se_type = "CR0")
  
  #modifying from CR0 to CR1
  m <- as.double(length(unique(villagexmonth_level$id_sc))) #number of clusters -- need for finite sample correctisn
  model$vcov <- model$vcov * (m/(m-1))

  treatment_effects <- model$coefficients[treatments]
  treatment_pval <- model$p.value[treatments]
  treatment_CI_low <- model$conf.low[treatments]
  treatment_CI_high <- model$conf.high[treatments]
  
  experiment <- c("Reminders","Reminders","Incentives","Incentives","Incentives","Incentives","Seeds", "Seeds", "Seeds", "Seeds")
  
  keyframe <- data.frame(treatment_names, treatment_effects, treatment_pval, treatment_CI_low, treatment_CI_high)
  keyframe$treatment_names <- factor(keyframe$treatment_names, levels = keyframe$treatment_names)
  
  #control mean
  control <- villagexmonth_level %>% filter(incentive_control == 1 & reminder_control_first == 1)
  control <- control %>% filter(seedsrisk == 0 | (seedsrisk == 1 & communication_control == 1))
  weighted.mean(control$shot_Measles1,control$village_population,na.rm= TRUE)
  
  
  #################################################################################
  #
  # II. PLOT
  #
  #################################################################################
  
  
  #Bar graph
  p <- ggplot(data=keyframe, aes(x=as.factor(treatment_names), y= treatment_effects, fill = experiment)) + 
    geom_bar(stat="identity") +
    scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) +
    scale_y_continuous(breaks = seq(-1.5, 2.5, 0.5)) + 
    expand_limits(y = c(-1,2))+
    scale_fill_manual(values = c("#23a7ff", "#fcee97", "#ff6666")) +
    ggtitle("Aggregated Policy Effects") +
    xlab("Policy") +
    ylab("Effect on Measles Vaccination") +
    geom_text(aes(label= paste0("p = ",round(treatment_pval,2))), position=position_dodge(width=0.9), vjust=-0.25) +
    theme(legend.position = c(0.9, 0.9), panel.background = element_rect(fill = 'white', colour = 'black'))
  
  assign(paste0("p_",sample),p)
  
  #Coefplot
  label_p1 = paste0("p = ",filter(keyframe, treatment_names == "High \nSlope")$treatment_pval %>% round(3))
  y1 = filter(keyframe, treatment_names == "High \nSlope")$treatment_effects
  label_p2 = paste0("p = ",filter(keyframe, treatment_names == "Information \nHub")$treatment_pval %>% round(3))
  y2 = filter(keyframe, treatment_names == "Information \nHub")$treatment_effects
  
  p2 <- ggplot(keyframe, aes(x=as.factor(treatment_names), y=treatment_effects, colour = experiment)) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin= treatment_CI_low, ymax= treatment_CI_high), width=.1,lwd = 1) + 
    geom_hline(yintercept = 0, colour = "black", linetype = "dashed") + 
    scale_colour_manual(name = "Treatment", values = c('#155F83FF','#FFA319FF',"firebrick")) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
          panel.background = element_rect(fill = "white", colour = "grey50"),
          axis.title = element_text(size = 8),
          plot.margin = margin(t=5, r = 5, b = 5, l=20)) +
    xlab("Treatment arm") +
    ylab("Effects on Measles Vaccination ") +
    ggtitle("Interventions Average Effects") +
    annotate("text", x = 5.4, y = y1, label = label_p1, size = 3, fontface = "bold") + 
    annotate("text", x = 8.4, y = y2, label = label_p2, size = 3, fontface = "bold") + 
    theme_bw() + 
    theme(plot.title = element_text(face = "bold"))
  
  assign(paste0("p2_",sample),p2)
  ggsave(file.path(path_figures, paste0("Uninteracted_", sample, ".pdf")), plot=p2, width = 10, height = 6)
}


