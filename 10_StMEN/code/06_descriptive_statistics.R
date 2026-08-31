
#Name in old directory: descriptive_statistics_ecma

rm(list=ls(all=T))

library(dplyr)
library(data.table)
library('this.path')

script_dir <- dirname(normalizePath(this.path::this.path(), winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "00_paths.R"))

villagexmonth_level <- fread(file.path(path_data_raw, "Tablet_VillageXMonth.csv"),header = TRUE, sep = ",", data.table = FALSE)

village_level <- villagexmonth_level %>% group_by(id_village_grp) %>% summarise(
  trfrac33_first = mean(trfrac33_first), 
  trfrac66_first = mean(trfrac66_first),
  slope = mean(slope),
  gossip = mean(gossip),
  seedsrisk = mean(seedsrisk),
  village_population = mean(village_population))

village_level$SMSblast = as.numeric(village_level$trfrac33_first | village_level$trfrac66_first)
village_level$packagetreat <- village_level$slope * village_level$SMSblast * village_level$gossip

#keep for later
originalcolumns <- colnames(village_level)

#PART 0: Join Baseline to Village Data

lasso_var<-fread(file.path(path_data_cleaned, "Baseline_Village.csv"),header = TRUE, sep = ",", data.table = FALSE)
#lasso_var<-fread(paste0(globdir,"/Baseline_Village.csv"),header = TRUE, sep = ",", data.table = FALSE)

colnames(lasso_var)[2:ncol(lasso_var)]<-paste("Baseline_",colnames(lasso_var)[2:ncol(lasso_var)],sep="")
lasso_var<-lasso_var[!is.na(lasso_var$id_village_grp),]
baseline_var<-colnames(lasso_var)[2:ncol(lasso_var)]


#generate missing variable
# there is no missing value
for(i in baseline_var){
  if(sum(is.na(lasso_var[,i]))>0){
    print("missing baseline variables") #this never actually happens
    lasso_var[,paste(i,"m",sep="_")]<-is.na(lasso_var[,i])
    lasso_var[is.na(lasso_var[,i]),i]<-0
  }
}

village_level<-left_join(village_level,lasso_var)

#filter to seedsrisk - which is also same as Baseline (Baseline covariates not available anywhere else)

village_level <- village_level %>% filter(seedsrisk == 1)

controls     <- grep(pattern = "Baseline_",x = colnames(village_level),value = TRUE)
toRemove <- grep(pattern = "_inj",x = controls) #don't want injections
controls <- controls[-toRemove]

names_affected <- c("Fraction participating in Employment Generating Schemes",
                    "Fraction Below Poverty Line (BPL)",
                    "Household financial status (on 1-10 scale)",
                    "Fraction Scheduled Caste-Scheduled Tribes (SC/ST) ",
                    "Fraction Other Backward Caste (OBC) ",
                    "Fraction Minority caste",
                    "Fraction General caste",
                    "Fraction No caste",
                    "Fraction Other caste",
                    "Fraction Dont know caste",
                    "Fraction Hindu",
                    "Fraction Muslim",
                    "Fraction Christian",
                    "Fraction Buddhist",
                    "Fraction Sikh",
                    "Fraction Jain",
                    "Fraction Other Religion",
                    "Fraction Don't know religion",
                    "Fraction Literate",
                    "Fraction unmarried",
                    "Fraction of adults Married (living with spouse)",
                    "Fraction of adults Married (not living with spouse)",
                    "Fraction of adults Divorced or Seperated",
                    "Fraction Widow or Widower",
                    "Fraction Marriage status unknown",
                    "Fraction Marriage status NA",
                    "Fraction who received Nursery level education or less",
                    "Fraction who received Class 4 level education",
                    "Fraction who received Class 9 level education",
                    "Fraction who received Class 12 level education",
                    "Fraction who received Graduate or Other Diploma level education ",
                    "Fraction Other Level Education Altogether",
                    "Number of vaccines administered to pregnant mother",
                    "Number of vaccines administered to child since birth",
                    "Fraction of children who received polio drops",
                    "Number of polio drops administered to child",
                    "Fraction of children who received immunized card",
                    "Fraction of children who receive more than 5 vaccines",
                    "Fraction of children who receive Measles vaccine by 15 months of age", 
                    "Fraction of children who receive Measles vaccine at credible locations",
                    "(usless statistic, ignore)"
)

stats_table <- list()

for (i in c(1:length(controls))) {
  base_var <- controls[i]
  j1 <- weighted.mean(village_level[,base_var], village_level[,"village_population"]) %>% round(3)
  stats_table <- append(stats_table, list(c(names_affected[i],j1)))
}

stats_table <- do.call(rbind, stats_table)
colnames(stats_table) <- c("Baseline Covariate", "Population-Weighted Average")

print(stats_table)

relevant_indices_paper <- c(1,2,3,4,5,11,12,13,14,19,20,21,22,23,24,27,28,29,30,31,33,34,35,36,37)


write.table(stats_table[relevant_indices_paper,], file = file.path(path_tables, "descriptive_statistics.txt"), sep = ",", quote = FALSE, row.names = FALSE, col.names = TRUE)
