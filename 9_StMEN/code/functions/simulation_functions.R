######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Functions for simulations
# AUTHOR:		Anirudh Sankar (& Louis-Mael Jean & Elsa Trezeguet)
# CREATED:	02/11/2021
# MODIFIED: 12/08/2021
# STATUS: 	Draft

#######################################################################################

#################################################################################
#
# This file contains functions that are used across all simulations:
#
#     - create_policy_vectors(R,M):  given R,M creates all possible policy combinations
# 
#     - create_original_treatment_assignments(R, M, nobs): creates matrix of treatment assignments with nobs columns
#
#     - create_sp_matrix(R,M, original_treatments): creates marginal treatment assignment matrix X given original_treatments assignment
# 
#     - create_sp_to_unique_transformation(R,M): creates matrix G to retrieve betas from alphas (unique policy TE from marginal TE)
#
#     - retrieve_backward_elimination_support(y, sp_matrix, policy_vectors): given output y and matrix X retrieves alpha support
#
#     - retrieve_puffer_support_Victor(y, sp_matrix, policy_vectors): XXX
#
#     - retrieve_vanilla_support_Victor(y, sp_matrix, policy_vectors,get_ind = FALSE):  usual LASSO support
#
#     - retrieve_binned_support(support, R, M): organizes the alpha support into profiles 
#
#     - create_collapsed_policies(support_binned, R, M): given binned support, gives the beta support of pooled policies (intersections of regions of encirclement and complements)
#
#     - get_pop_treat_effects(collapsed_all, unique_treat_effects_pop, R, M): retrieves treatment effect of each pooled policy 
#
#     - get_collapsed_df(collapsed_all, y, beta_matrix, R, M): organizes pooled support into dataframe that can be fed to post LASSO OLS
#
#     - get_unique_policy_support_pop(collapsed_all): XXX
#
#     - get_Andrews_estimates_custom <- function(model_input, type = "hybrid", alpha,beta = 0.005, debiased = FALSE, x = NULL, y = NULL, M_JM = NULL, nobs_andrews = NULL)  {
#        get winner's curse adjusted estimate
#
#     - get_Andrews_hybrid_estimate(data_df): XXX
#
#     - combn_sub(x, m, nset = 5000, seed=123, simplify = TRUE, ...): XXX


#################################################################################


source(file.path(path_functions, "inference_on_winners_functions.R"), local = TRUE)

create_policy_vectors <- function(R,M) {
  #an exhaustive cross product, a list 1 1, 1 2 etc
  arms <- 1:M
  
  if(length(R) == 1){ # Then we are in the classic case, we do not change the code 
    intensities <- 1:R
    tocross <- rep(1,M) %*% t.default(intensities)
    
  }
  else{ # in this case, We have different intensities for each arms
    # now R is a vector
    # maximum power of arm: 
    R_max <- max(R)
    
    intensities <- list()
    
    
    for(i in 1:length(R)){
      intensities[[i]] <- c(1:R[i], rep(1,R_max-R[i]))
    }
    tocross <- matrix(unlist(intensities), nrow=length(intensities), byrow=TRUE)
  }
  
  tocross <- lapply(1:nrow(tocross), function(i) tocross[i,])
  policy_vectors <- unique(go(tocross))
  
  return(policy_vectors)
}

#------------------------------------------------------------------------------#

create_sp_matrix <- function(R,M, original_treatments, policy_vectors = NULL) {
  
  arms <- 1:M
  
  if(is.null(policy_vectors)){
    policy_vectors <- create_policy_vectors(R,M)
  }
  
  sp_matrix <- c()
  sp_names <- list() #names will look like "c(1,2,3,1)" etc
  
  for (j in 1:length(policy_vectors)) { #for each policy vector j, get the dummy for it and make it column of sp matrix
    #print(j)
    
    relevant_pooled_dummies <- c()
    
    for (m in arms) { #for each arm, get the pooled intensity for that arm. note this is NOT the overall pooled policy. 
      # The corresponding intensity of the arm
      if(length(R) == 1){
        R_m <- R
      }
      else{
        R_m <- R[m]
      }
      if (policy_vectors[[j]][m] == 1) { #if control at the index, keep as control
        intensities <- 1
      }
      else {
        intensities <- policy_vectors[[j]][m]:R_m #note the intensities to pool
      }
      
      relevant_treatment_names_m <- paste0(paste0("R",intensities),"M",m) #note the names of the treatment intensities to pool
      relevant_treatments_m <- original_treatments %>% dplyr::select(relevant_treatment_names_m) #retrieve those intensities
      sp_pooled_m <- apply(relevant_treatments_m,1,max) #apply OR logic to the relevant treatments
      relevant_pooled_dummies <- cbind(relevant_pooled_dummies, sp_pooled_m) #apply OR logic 
    }
    
    sp_policy <- apply(relevant_pooled_dummies,1,prod) #AND logic
    sp_matrix <- cbind(sp_matrix, sp_policy) #AND over the different arms
    sp_names[[j]] <- policy_vectors[[j]] #names will look like "c(1,2,3,1)" etc
    
  }
  
  colnames(sp_matrix) <- sp_names
  
  return(sp_matrix)
}

#------------------------------------------------------------------------------#

create_sp_to_unique_transformation <- function(R,M, policy_vectors = NULL) { # This function is the matrix of transformations from the betas to the alphas 
  
  arms <- 1:M
  
  #intensities <- 1:R
  if(is.null(policy_vectors)){
    policy_vectors <- create_policy_vectors(R,M)
  }
  
  G <- c()
  for (i in 1:length(policy_vectors)) { #get ith column of G
    
    pol_i <- policy_vectors[[i]]
    
    #store the indices from {1..M} where nonzero
    key_ind <- which(policy_vectors[[i]] != 1)
    non_ind <- which(policy_vectors[[i]] == 1)
    
    j_list <- c()
    
    for (j in 1:length(policy_vectors)) {
      
      #compare policy_vectors[[j]] to policy_vectors[[i]]
      
      pol_j <- policy_vectors[[j]]
      
      if ((length(key_ind) > 0) & all(pol_j[key_ind] >= pol_i[key_ind]) & all(pol_j[non_ind] == 1)) { #collect the ones that should be pooled
        j_list <- c(j_list, j) 
      }
    }
    
    col_i <- rep.int(0,length(policy_vectors))
    col_i[j_list] <- 1
    
    G <- cbind(G, col_i)
  }
  
  G[1,1] <- 1 #rectify top left column
  
  colnames(G) <- policy_vectors
  rownames(G) <- policy_vectors
  
  
  return(G)
}

#------------------------------------------------------------------------------#

create_original_treatment_assignments <- function(R, M, nobs) {
  original_treatment_names <- c()
  original_treatments <- c()
  
  #randomize the assignment into RIMJ:
  
  for (m in 1:M) { #for each arm, a cross randomized expt
    
    s <- runif(nobs)
    
    if(length(R) == 1){
      R_m <- R
    }
    else{
      R_m <- R[m]
    }
    
    for (j in 0:(R_m-1))  {   #split [0,1] interval into R subintervals and assign accordingly
      
      #for a subinterval j/R to j+1/R, generate a treatment variable that is 1 only in that interval
      
      low <- j/R_m
      high <- (j+1)/R_m
      
      ind <- which((s >= low) & (s < high))
      
      t <- rep(0, nobs) #this is the actual dummy
      t[ind] <- 1
      
      original_treatments <- cbind(original_treatments, t)
      original_treatment_names <- c(original_treatment_names, paste0("R", (j+1), "M", m))
    }
  }
  
  original_treatments <- as.data.frame(original_treatments)
  colnames(original_treatments) <- original_treatment_names
  
  return(original_treatments)
}

#------------------------------------------------------------------------------#

retrieve_backward_elimination_support <- function(y, sp_matrix, policy_vectors) {
  
  data_matrix <- cbind(y, sp_matrix)
  colnames(data_matrix)[1] <- "y"
  current_data_df <- as.data.frame(data_matrix)
  current_variables <- colnames(sp_matrix)
  
  sp_formula <- as.formula("y ~ .")
  current_model_ols <- estimatr::lm_robust(formula = sp_formula, data = current_data_df, se_type = "classical")
  current_pval_list <- current_model_ols$p.value[-1] #ignore intercept
  current_largest_pval <- max(current_pval_list)
  
  #continuous decay cutoff p_cut(n) = Ae^(-gamma n), where p_cut(1000) = 0.01, p_cut(10^4) = 10^-8  
  gamma = log(10^(-6))/(10^3 - 10^4)
  A = 0.01/exp(-1000*gamma)
  p_cut = A*exp(-nobs*gamma)
  
  while (current_largest_pval > p_cut) {
    
    #throw it out and refresh the current variables
    
    deselect_name <- names(current_pval_list[current_pval_list == current_largest_pval])
    #print(deselect_name)
    
    #print(current_largest_pval)
    #print(paste0("Nb variables left = ", length(current_variables)))
    
    current_variables <- current_variables[current_variables != gsub("`", "", deselect_name)]
    
    #refresh the data frame
    current_data_df <- current_data_df[ , names(current_data_df) != gsub("`", "", deselect_name)]
    
    #refresh the model and note the highest 
    current_model_ols <- estimatr::lm_robust(formula = sp_formula, data = current_data_df, se_type = "classical")
    current_pval_list <- current_model_ols$p.value[-1]
    current_largest_pval <- max(current_pval_list)
    
  }
  
  final_support <- current_variables
  
  support_ind <- match(final_support,colnames(sp_matrix)) #get indices of suppport
  support <- policy_vectors[support_ind] #this retrieves the actual support in a list of vectors (e.g. 2 2, 3 4 etc)
  
  return(support)
  
}


#------------------------------------------------------------------------------#

retrieve_vanilla_support_Victor <- function(y, sp_matrix, policy_vectors,get_ind = FALSE) {
  data_matrix <- cbind(y, sp_matrix)
  
  data_matrix <- as.data.frame(data_matrix)
  
  
  colnames(data_matrix)[1] <- "y"
  
  formule <- as.formula("y~ .")
  
  model_lasso <- rlasso(formula = formule, data = data_matrix,
                        penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE)) #you might adjust the penalty argument here
  
  support_vanilla <- names(model_lasso$coefficients[model_lasso$coefficients != 0])
  toRemove <- grep(pattern = "ntercept",x = support_vanilla) #don't want intercept to be lasso selected!
  support_vanilla <- support_vanilla[-toRemove]
  
  new_c = 1.1
  while (length(support_vanilla) == 0){
    new_c = new_c - 0.1
    model_lasso <- rlasso(formula = formule, data = data_matrix,
                          penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE, c = new_c))
    
    support_vanilla <- names(model_lasso$coefficients[model_lasso$coefficients != 0])
    toRemove <- grep(pattern = "ntercept",x = support_vanilla) #don't want intercept to be lasso selected!
    support_vanilla <- support_vanilla[-toRemove]
    cat("\n\t\t\t c = ", new_c)
  }
  
  support_vanilla <- gsub("`", '', support_vanilla)
  
  support_ind <- match(support_vanilla,colnames(sp_matrix)) #get indices of suppport
  support <- policy_vectors[support_ind] #this retrieves the actual support in a list of vectors (e.g. 2 2, 3 4 etc)
  
  if (get_ind == TRUE) {
    return(support_ind)
  }
  
  return(support)
}

#------------------------------------------------------------------------------#

get_debiased_lasso <- function(y, sp_matrix) {
  ## Naive lasso: 
  
  data_matrix <- cbind(y, sp_matrix) %>% as.data.frame()
  
  colnames(data_matrix)[1] <- "y"
  
  formule <- as.formula("y~ .")
  
  model_lasso <- rlasso(formula = formule, data = data_matrix,
                        penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE)) #you might adjust the penalty argument here
  
  # The coefficients (remove intercept): 
  coefficients_vanilla <- model_lasso$coefficients[-1]
  
  # Computing matrix M based on Javanmard and Montanari
  
  M_JM <- selectiveInference::debiasingMatrix(sp_matrix, is_wide = TRUE, nsample = nobs, rows = 1:dim(sp_matrix)[2])
  # The new estimator: 
  coefficients_debiased_support <- coefficients_vanilla + (1/nobs) * M_JM %*% t(sp_matrix) %*% (y - sp_matrix %*% coefficients_vanilla)
  
  res <- list()
  res[[1]] <- coefficients_debiased_support
  #res [[2]] <- support_ind
  res[[2]] <- model_lasso
  res[[3]] <- M_JM
  return(res)
}

#------------------------------------------------------------------------------#

retrieve_binned_support <- function(support, R, M) { # R is not used in this function
  
  #bin the support into types: M choose 1 (nonzero) M choose 2 nonzero,... M choose M nonzero, and then within each the placements
  
  support_binned <- list(list()) #linked list
  
  bin_counter = 1
  
  for (k in 1:M) {
    positions_k <- combinations(M,k)
    #print(positions_k)
    
    for (r in 1:dim(positions_k)[1]) { # go through each combination, each one of these k_r is a TYPE
      positions_k_r <- positions_k[r,]
      
      selected_k_r <- list()
      
      q = 1
      
      for (l in 1:length(support)) { # go through support and bin
        if(all(support[[l]][positions_k_r] != 1) & all(support[[l]][-positions_k_r] == 1)) { #the controls have to match exactly, and the other ones have to be nonzero at the right places
          #print(support[[l]])
          selected_k_r[[q]] <- support[[l]]
          q = q+1
        } 
        
      }
      
      support_binned[[bin_counter]] <- selected_k_r
      bin_counter = bin_counter + 1
    }
    
  }
  
  
  #CLEAN BINS: throw away empty bins
  
  empty_bin_ind <- c()
  
  for (i in 1:length(support_binned)) {
    if (length(support_binned[[i]]) == 0) {
      empty_bin_ind <- c(empty_bin_ind, i) 
    } 
  }
  
  if (length(empty_bin_ind) > 0) {
    support_binned <- support_binned[-empty_bin_ind]
  }
  
  return(support_binned)
  
}

#------------------------------------------------------------------------------#

create_collapsed_policies <- function(support_binned, R, M) {
  collapsed_all <- list()
  
  collapsed_num <- 1
  
  for (i in 1: length(support_binned)) {
    
    #first, construct a full list of applicable policies in the bin type
    #bin type full is a list of vectors e.g. 1 (type level 2 ) (type level 2 ) 1 ....1 (type level R) (type level R) 1
    
    #WLOG just take first element to observe type
    
    bin_type_ind_i <- which(support_binned[[i]][[1]] != 1) #identifies the "type" of the bin by taking to account nonzero indices
    
    total_vec <- list()
    for (m in 1:M) {
      if(length(R)==1){
        R_m <- R
      }else{
        R_m <- R[m]
      }
      if (m %in% bin_type_ind_i) {
        total_vec[[m]] <- 2:R_m
      }
      else {
        total_vec[[m]] <- 1
      }
    }
    
    fullcross <- purrr::cross(total_vec)
    
    bin_type_full_i <- list() #flatten inside
    
    for (j in 1: length(fullcross)) {
      bin_type_full_i[[j]] <- unlist(fullcross[[j]])
    }
    
    atleast_ray_list_i <- list()
    
    for (j in 1:length(support_binned[[i]])) {
      
      atleast_ray_i_j <- list() #these are the individual rays 
      
      k = 1
      for (l in 1:length(bin_type_full_i)) {
        if (all(bin_type_full_i[[l]] >= support_binned[[i]][[j]])) { #go through the full list in that type and check if its part fo the ray
          atleast_ray_i_j[[k]] = bin_type_full_i[[l]]
          k = k + 1
        }
      }
      
      atleast_ray_list_i[[j]] <- atleast_ray_i_j
      #contruct full "ray" in a lsit
    }
    
    #collapsing policies according to a disjoint union A U B U C = A^{1,c} int B^{1,c} int C^ {1,c}  / (A^c \int B^c \int C^c )
    #collasped_type_i is a dataframe capturing the collapsed policies relevant to bin type i 
    
    #create binary flags for the {1,c}^number of policies in that bin
    binary_switch <- expand.grid(replicate(length(support_binned[[i]]), 0:1, simplify = FALSE)) 
    binary_switch <- binary_switch[-1,] #delete (c, c, c). This is the one policy in the complement of A U B U C
    binary_switch <- as.matrix(binary_switch)
    
    collapsed_type_i <- list() #matrix capturing collapsed policies relevant to bin type i
    
    for (j in 1:dim(binary_switch)[1]) { #go through all intersection combinations
      
      collapsed_i_policy_j <- bin_type_full_i #start with the full list ("full space")
      
      for (k in 1:length(atleast_ray_list_i)) {
        
        if (binary_switch[j, k] == 1) { #take the intersection
          collapsed_i_policy_j <- intersect(collapsed_i_policy_j, atleast_ray_list_i[[k]])
        }
        else if (binary_switch[j, k] == 0) { #take the intersection with the complement
          collapsed_i_policy_j <- intersect(collapsed_i_policy_j, setdiff(bin_type_full_i, atleast_ray_list_i[[k]]))
        }
        
      }
      
      
      if (length(collapsed_i_policy_j) > 0 ) {
        collapsed_i_policy_j <- do.call(rbind, collapsed_i_policy_j) 
        collapsed_all[[collapsed_num]] <- collapsed_i_policy_j
        collapsed_num <- collapsed_num + 1
      }
      
    }
  }
  
  return(collapsed_all)
}

#------------------------------------------------------------------------------#

get_pop_treat_effects <- function(collapsed_all, unique_treat_effects_pop, R, M) { # R not used in this function
  
  collapsed_treat_effects <- c()
  
  for (o in 1:length(collapsed_all)) {
    
    matrix_type_o <- collapsed_all[[o]]
    
    #PART 1: obtain a treatment effect for each collapsed policy, meaning EACH ROW of EACH MATRIX in collasped _all
    
    treat_effect_i_list <- c()
    for (i in 1:dim(matrix_type_o)[1]) { # over the rows of the matrix, one policy for each row
      
      for (j in 1:length(policy_vectors)) {
        if (all(matrix_type_o[i,] == policy_vectors[[j]])) { #found a match!
          
          treat_effect_i_list <- c(treat_effect_i_list, unique_treat_effects_pop[j])
          #add treatment effects
        }
      }
    }
    
    treat_effect_o <- mean(treat_effect_i_list)
    
    collapsed_treat_effects <- c(collapsed_treat_effects, treat_effect_o)
    
  }
  
  return(collapsed_treat_effects)
  
}

#------------------------------------------------------------------------------#

get_collapsed_df <- function(collapsed_all, y, beta_matrix, R, M) { # R not used in this function
  
  collapsed_treat_data <- c()
  collapsed_treat_data_names <- c()
  
  for (o in 1:length(collapsed_all)) {
    
    matrix_type_o <- collapsed_all[[o]]
    
    #PART 1: obtain a treatment effect for each collapsed policy, meaning EACH ROW of EACH MATRIX in collasped _all
    
    relevant_or <- c()
    
    names_or <- c()
    for (i in 1:dim(matrix_type_o)[1]) { # over the rows of the matrix, one policy for each row
      
      for (j in 1:length(policy_vectors)) {
        if (all(matrix_type_o[i,] == policy_vectors[[j]])) { #found a match!
          
          relevant_or <- cbind(relevant_or, beta_matrix[,j])
          
          names_or <- c(names_or, colnames(beta_matrix)[j])
          #add treatment effects
        }
      }
    }
    
    combinedtreat_o <- apply(relevant_or,1, max)
    collapsed_treat_data <- cbind(collapsed_treat_data, combinedtreat_o)
    collapsed_treat_data_names <- c(collapsed_treat_data_names, paste0(names_or, collapse = "X"))
    
  }
  
  colnames(collapsed_treat_data) <- collapsed_treat_data_names
  
  data_df <- as.data.frame(cbind(y, collapsed_treat_data))
  
  return(data_df)
}

#------------------------------------------------------------------------------#

get_unique_policy_support_pop <- function(collapsed_all, get_ind = FALSE) {
  
  unique_policy_support_ind <- c()
  
  for (o in 1:length(collapsed_all)) {
    
    matrix_type_o <- collapsed_all[[o]]
    
    #PART 1: obtain a treatment effect for each collapsed policy, meaning EACH ROW of EACH MATRIX in collasped _all
    
    
    for (i in 1:dim(matrix_type_o)[1]) { # over the rows of the matrix, one policy for each row
      
      for (j in 1:length(policy_vectors)) {
        if (all(matrix_type_o[i,] == policy_vectors[[j]])) { #found a match!
          
          unique_policy_support_ind <- c(unique_policy_support_ind,j)
          #add treatment effects
        }
      }
    }
    
  }
  
  if (get_ind == TRUE) {
    return(unique_policy_support_ind)
  }
  
  return(policy_vectors[unique_policy_support_ind])
}

#------------------------------------------------------------------------------#

get_Andrews_estimates_custom <- function(model_input, type = "hybrid", alpha,beta = 0.005, debiased = FALSE, x = NULL, y = NULL, M_JM = NULL, nobs_andrews = NULL)  {
  
  if(debiased == FALSE){
    ntreat <- length(model_input$coefficients[-1])
    nobs_andrews <- nobs(model_input)
    pl_effects <- model_input$coefficients[-1]
    
    pol_best_ind <- which(pl_effects == max(pl_effects))
    pol_best_name <- names(pol_best_ind)
    pol_2nd_name <- names(which.max(pl_effects[pl_effects!=max(pl_effects)]))
    
    best_scaled_effect <- sqrt(nobs_andrews) * pl_effects[pol_best_name]
    #print("best scaled effect")
    #print(best_scaled_effect)
    
    trunc_scaled_effect <- max(0, sqrt(nobs_andrews) * pl_effects[pol_2nd_name]) #in case
    
  }else{
    ntreat <- length(model_input)
    nobs_andrews <- nobs_andrews
    pl_effects <- model_input
    
    pol_best_ind <- which(pl_effects == max(pl_effects))
    pol_best_name <- names(which(pl_effects == max(pl_effects)))
    pol_2nd_name <- names(which.max(pl_effects[pl_effects!=max(pl_effects)]))
    
    best_scaled_effect <- sqrt(nobs_andrews) * pl_effects[pol_best_ind]
    trunc_scaled_effect <- max(0, sqrt(nobs_andrews) * pl_effects[pol_2nd_name]) #in case
    
  }
  
  if(debiased == FALSE){
    
    var_around_best <- nobs_andrews * (model_input$std.error[pol_best_name])^2
    
  }else{# There are no standard errors in the model, hence we have to compute them 
    sigma <- estimateSigma(x,y)$sigmahat
    
    # We compute Q=M*SIGMA*M as defined in Javanmard and Montanari
    SIGMA = (t(x) %*% x)/nobs_andrews
    Q <- M_JM %*% SIGMA %*% M_JM
    
    st_dev_best_pol <- sigma*sqrt(Q[pol_best_ind,pol_best_ind]/nobs_andrews)
    var_around_best <- nobs_andrews * (st_dev_best_pol)^2
    
  }
  
  hybrid_results_scaled <- get_hybrid_Y_alpha_beta_custom(best_scaled_effect, trunc_scaled_effect, var_around_best, ntreat, alpha, beta)
  #unbiased_results_scaled <- get_perfectly_unbiased_custom(best_scaled_effect, trunc_scaled_effect, var_around_best, ntreat, alpha)
  
  hybrid_results <- (1/sqrt(nobs_andrews)) * hybrid_results_scaled
  #unbiased_results <- (1/sqrt(nobs_andrews)) * unbiased_results_scaled
  
  if (type == "hybrid") {
    return(hybrid_results)
  } else if (type == "unbiased") {
    return(unbiased_results)
  }
}


######################################################################################
#
# END
#
#######################################################################################


