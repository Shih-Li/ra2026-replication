######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Functions for inference on winners (Andrews et al)
# AUTHOR:		Anirudh Sankar

#######################################################################################


library('dplyr')
library('ggplot2')
library('truncnorm')
library('stats')
library('purrr')
library('TruncatedNormal')

set.seed(16)

#------------------------------------------------------------------------------#

get_hybrid_Y_alpha_beta_custom <- function(Y_best_theta, truncation_point, var_Y_best_theta, ntreat, alpha, beta) {
  
  #Kitagawana-Tetenov z-score
  
  num_simulations = 10000
  
  eta_sims = c()
  
  for (i in 1:num_simulations) {
    
    eta_i_list <- c()
    
    for (j in 1:ntreat) {
      eta_i_list <- c(eta_i_list, abs(rnorm(n=1, sd = sqrt(var_Y_best_theta))))
    }
    eta = max(eta_i_list)/sqrt(var_Y_best_theta)
    
    eta_sims <- c(eta_sims, eta)
    
  }
  
  if (beta == 0) {
    kt_zscore = Inf #theoretical it would be this, only because we're using simulations its not
  } else {
    kt_zscore = quantile(eta_sims, 1 - beta)
  }
  
  #Simultaneous confidence band at level beta  
  kt_CI <- c(Y_best_theta - kt_zscore * sqrt(var_Y_best_theta), Y_best_theta + kt_zscore * sqrt(var_Y_best_theta))
  
  #Hybrid interval
  gamma <- (alpha - beta)/(1- beta)
  
  
  hybrid_median_mu = get_hybrid_mu_alpha(Y_statistic = Y_best_theta, truncation_point_original = truncation_point, c_B = kt_zscore, CS_P_B = kt_CI, cov_Y = var_Y_best_theta , alpha = 0.50)
  
  unbiased_ET_higher_gamma = get_hybrid_mu_alpha(Y_statistic = Y_best_theta, truncation_point_original = truncation_point, c_B = kt_zscore, CS_P_B = kt_CI, cov_Y = var_Y_best_theta , alpha = gamma/2)
  unbiased_ET_lower_gamma = get_hybrid_mu_alpha(Y_statistic = Y_best_theta, truncation_point_original = truncation_point, c_B = kt_zscore, CS_P_B = kt_CI, cov_Y = var_Y_best_theta , alpha = 1 - (gamma/2))
  
  CI_lower <- max(kt_CI[1], unbiased_ET_higher_gamma[[1]])
  CI_upper <- min(kt_CI[2], unbiased_ET_lower_gamma[[1]])
  
  hybrid_CI <- c(CI_lower,CI_upper)
  hybrid_estimate <- hybrid_median_mu[[1]]
  
  return(c(hybrid_estimate,hybrid_CI))
  
}

#------------------------------------------------------------------------------#

get_hybrid_mu_alpha <- function(Y_statistic, truncation_point_original, c_B, CS_P_B, cov_Y, alpha) {
  rhs <- 1 - alpha
  if (c_B != Inf) { #CS_P_B is not trivially all of R
    return(uniroot((function (x) ptmvnorm_extended(q = Y_statistic, mu = x, sigma = cov_Y, lb=max(truncation_point_original,x - c_B*sqrt(cov_Y)), ub=x + c_B*sqrt(cov_Y)) - rhs), lower = floor(CS_P_B[1]), upper = ceiling(CS_P_B[2]), extendInt = "yes")[1] %>% unname())
  }
  else { #same formula, just set different limits
    return(uniroot((function (x) ptmvnorm_extended(q = Y_statistic, mu = x, sigma = cov_Y, lb=max(truncation_point_original,x - c_B*sqrt(cov_Y)), ub=x + c_B*sqrt(cov_Y)) - rhs), lower = truncation_point_original, upper = 2*Y_statistic, extendInt = "yes")[1] %>% unname())
  }
}

#------------------------------------------------------------------------------#

ptmvnorm_extended <- function(q, mu, sigma, lb, ub) {
  
  if (q > ub) {
    return(1)
  }
  
  if (q < lb) {
    return(0)
  }
  
  sigma <- as.numeric(sigma)
  
  if (
    length(sigma) != 1L ||
    !is.finite(sigma) ||
    sigma <= 0
  ) {
    stop(
      "sigma must be one finite positive variance; received: ",
      paste(sigma, collapse = ", ")
    )
  }
  
  return(
    TruncatedNormal::ptmvnorm(
      q = as.numeric(q),
      mu = as.numeric(mu),
      sigma = matrix(sigma, nrow = 1L, ncol = 1L),
      lb = as.numeric(lb),
      ub = as.numeric(ub)
    )
  )
}

#------------------------------------------------------------------------------#

get_perfectly_unbiased_custom <- function(Y_best_theta, truncation_point, var_Y_best_theta, ntreat, alpha) {
  
  median_mu <- get_mu_alpha_flexible(Y_best_theta, truncation_point, var_Y_best_theta, 0.50)
  CI_lower_ET <- get_mu_alpha_flexible(Y_best_theta, truncation_point, var_Y_best_theta, alpha/2) #for \mu_0.05
  CI_upper_ET <- get_mu_alpha_flexible(Y_best_theta, truncation_point, var_Y_best_theta, 1 - (alpha/2)) #for \mu_0.95
  
  
  return(c(median_mu[[1]], CI_lower_ET[[1]], CI_upper_ET[[1]]))
  
}

#------------------------------------------------------------------------------#

get_mu_alpha_flexible <- function(Y_statistic, truncation_point, cov_Y, alpha) {
  mu_alpha <- try(get_mu_alpha(Y_statistic, truncation_point, cov_Y, alpha), silent = TRUE)
  #print(class(mu_alpha))
  if (class(mu_alpha) == "try-error") {
    if (alpha < 0.5) {
      #mu_alpha <- get_mu_alpha(Y_statistic, truncation_point, cov_Y, 0)
      mu_alpha <- -Inf
    }
    else { #alpha >0.5
      #mu_alpha <- get_mu_alpha(Y_statistic, truncation_point, cov_Y, 1)
      mu_alpha <- Inf
    }
  }
  return(mu_alpha)
}

#------------------------------------------------------------------------------#

get_mu_alpha <- function(Y_statistic, truncation_point, cov_Y, alpha) {
  rhs <- 1 - alpha
  return(uniroot((function (x) cdf_to_Y_statistic(x, Y_statistic, truncation_point, Inf, cov_Y) - rhs), lower = truncation_point, upper = 2*Y_statistic, extendInt = "yes")[1] %>% unname())
}

#------------------------------------------------------------------------------#

cdf_to_Y_statistic <- function(mean_guess, Y_statistic, trunc_low, trunc_high, cov_Y) {
  # TruncatedNormal >= 2.x expects `sigma` to be a covariance matrix.
  # This is a one-dimensional problem, so route through the compatibility
  # wrapper above, which validates the scalar variance and supplies a 1x1 matrix.
  return(
    ptmvnorm_extended(
      q = Y_statistic,
      mu = mean_guess,
      sigma = cov_Y,
      lb = trunc_low,
      ub = trunc_high
    )
  )
}

#------------------------------------------------------------------------------#

#non -custom: Andrews general setting

get_hybrid_Y_alpha_beta_concise_nocluster <- function(sample_data, ntreat, alpha, beta) {
  # make sample X and Y from general setting
  
  X_long <- get_X_long(sample_data,ntreat)
  Y_long <- get_Y_long(sample_data, ntreat)
  
  best_theta_num <- which.max(X_long)
  Y_best_theta <- Y_long[best_theta_num]
  
  #for debugging: 2nd best
  
  Y_2ndbest = max(Y_long[Y_long!=max(Y_long)])
  
  
  #normal distribution for Y(\hat(theta)). Consistent estimator of population variance of Y_best_theta variance. 
  
  #no cluster
  section_bestpol <- sample_data %>% filter(treatments == best_theta_num)
  var_Y_best_theta <- var(section_bestpol$outcome) * (n_total/nrow(section_bestpol))
  
  truncation_point <- Y_2ndbest
  
  #Kitagawana-Tetenov z-score
  
  num_simulations = 10000
  eta_sims = c()
  
  for (i in 1:num_simulations) {
    
    eta_i_list <- c()
    
    for (j in 1:ntreat) {
      eta_i_list <- c(eta_i_list, abs(rnorm(n=1, sd = sqrt(var_Y_best_theta))))
    }
    eta = max(eta_i_list)/sqrt(var_Y_best_theta)
    
    eta_sims <- c(eta_sims, eta)
    
  }
  
  if (beta == 0) {
    kt_zscore = Inf #theoretical it would be this, only because we're using simulations its not
  } else {
    kt_zscore = quantile(eta_sims, 1 - beta)
  }
  
  #Simulataneous confidence band at level beta  
  kt_CI <- c(Y_best_theta - kt_zscore * sqrt(var_Y_best_theta), Y_best_theta + kt_zscore * sqrt(var_Y_best_theta))
  
  #Hybrid interval
  gamma <- (alpha - beta)/(1- beta)
  
  
  hybrid_median_mu = get_hybrid_mu_alpha(Y_statistic = Y_best_theta, truncation_point_original = truncation_point, c_B = kt_zscore, CS_P_B = kt_CI, cov_Y = var_Y_best_theta , alpha = 0.50)
  
  unbiased_ET_higher_gamma = get_hybrid_mu_alpha(Y_statistic = Y_best_theta, truncation_point_original = truncation_point, c_B = kt_zscore, CS_P_B = kt_CI, cov_Y = var_Y_best_theta , alpha = gamma/2)
  unbiased_ET_lower_gamma = get_hybrid_mu_alpha(Y_statistic = Y_best_theta, truncation_point_original = truncation_point, c_B = kt_zscore, CS_P_B = kt_CI, cov_Y = var_Y_best_theta , alpha = 1 - (gamma/2))
  
  CI_lower <- max(kt_CI[1], unbiased_ET_higher_gamma[[1]])
  CI_upper <- min(kt_CI[2], unbiased_ET_lower_gamma[[1]])
  
  hybrid_CI <- (1/sqrt(n_total)) * c(CI_lower,CI_upper)
  hybrid_estimate <- (1/sqrt(n_total)) *hybrid_median_mu[[1]]
  
  return(c(best_theta_num,hybrid_estimate,hybrid_CI,(1/sqrt(n_total))*Y_best_theta))
} 

#------------------------------------------------------------------------------#

get_X_long <- function(sample_data, ntreat) {
  X_long <- c()
  for (i in c(1:ntreat)) {
    X_long <- c(X_long, get_X_theta(sample_data,i)) #remember these are scaled
  }
  return(X_long)
}

#------------------------------------------------------------------------------#

get_Y_long <- function(sample_data, ntreat) {
  X_long <- get_X_long(sample_data, ntreat)
  Y_long <- X_long - get_X_theta(sample_data,1)
  return(Y_long)
}

#------------------------------------------------------------------------------#

get_X_theta <- function(sample_data, theta) {
  section <- sample_data %>% filter(treatments == theta)
  return(sqrt(n_total)*weighted.mean(section$outcome, section$weights)) #should be per treatment in order to hit the correct variance (sample variance)
}


######################################################################################
#
# END
#
#######################################################################################

