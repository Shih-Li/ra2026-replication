######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE: p-value <-> lambda mapping functions
# AUTHOR:		Anirudh Sankar

#######################################################################################


e_bar =villagexmonth_level[,outcome] - full_model_ols$fitted.values

sigma_squared_est <- sum(e_bar^2)/(dim(villagexmonth_level)[1] - length(variables_expanded)) #http://lukesonnet.com/teaching/inference/200d_standard_errors.pdf

normalized_weights = (villagexmonth_level$village_population/sum(villagexmonth_level$village_population))*dim(villagexmonth_level)[1] #normalized to have sum N = dim(villagexmonth_level)[1]

sigma_squared_est_weighted <- mean(sigma_squared_est/sqrt(normalized_weights))

#------------------------------------------------------------------------------#


get_lambda_from_p <- function(p_cut) {
  
  lambda = (sqrt(sigma_squared_est_weighted)*qnorm(1-(p_cut/2)))/sqrt(dim(villagexmonth_level)[1]) #lam 
  
  #lambda = lambda * dim(villagexmonth_level)[1] #rescaling
  
  return(lambda)
}


#------------------------------------------------------------------------------#


get_p_from_lambda <- function(lambda_choice) { #based on Rohe 2014
  
  #lambda_choice = lambda_choice / dim(villagexmonth_level)[1] #de-rescaling
  
  p_cut =  2*(1 - pnorm((lambda_choice/sqrt(sigma_squared_est_weighted)) * sqrt(dim(villagexmonth_level)[1])))
  
  return(p_cut)
}

######################################################################################
#
# END
#
#######################################################################################   

