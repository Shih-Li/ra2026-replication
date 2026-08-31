######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Create all marginal space variables
# AUTHOR:		Anirudh Sankar

#######################################################################################

villagexmonth_level$noReminder <- villagexmonth_level$reminder_control_first
villagexmonth_level$noSeed <- villagexmonth_level$communication_control
villagexmonth_level$noIncentive <- villagexmonth_level$incentive_control

villagexmonth_level$atleastgossip <- as.numeric(villagexmonth_level$gossip | villagexmonth_level$trustgossip)
villagexmonth_level$marginalgossip <- villagexmonth_level$trustgossip

villagexmonth_level$SMSblast = as.numeric(villagexmonth_level$trfrac33_first | villagexmonth_level$trfrac66_first)
villagexmonth_level$highSMS = villagexmonth_level$trfrac66_first
villagexmonth_level$lowSMS = villagexmonth_level$trfrac33_first


villagexmonth_level$atleastSMS <- villagexmonth_level$SMSblast
villagexmonth_level$marginalSMS = villagexmonth_level$trfrac66_first

villagexmonth_level$atleastslope <- villagexmonth_level$slope
villagexmonth_level$marginalslope <- villagexmonth_level$highslope

villagexmonth_level$atleastflat <- villagexmonth_level$flat
villagexmonth_level$marginalflat <- villagexmonth_level$highflat

villagexmonth_level$lowgossip <- villagexmonth_level$gossip
villagexmonth_level$highgossip <- villagexmonth_level$trustgossip


for (seed in c("atleastgossip", "marginalgossip","random","trusted","noSeed")) {
  for (incentive in c("atleastslope","marginalslope", "atleastflat","marginalflat", "noIncentive")) {
    for (reminder in c("atleastSMS", "marginalSMS", "noReminder")) {
      sp_string <- paste0("SP_",seed,"X",incentive, "X",reminder)
      print(sp_string)
      villagexmonth_level[,sp_string] <- villagexmonth_level[,seed]* villagexmonth_level[,incentive]*villagexmonth_level[,reminder]  
    } 
  }
}


#################################################################################
#
# END
#
#################################################################################

