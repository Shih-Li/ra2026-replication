######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Helper functions for pooling
# AUTHOR:		Anirudh Sankar

#######################################################################################

add_pooled_policies_tp <- function(data, tp_support, tp_name) {
  tp_support <- tp_support %>% str_remove("SP_") 
  
  R = 3
  M = 3
  
  tp_support_num <- list()
  
  if (length(tp_support) == 0) {
    return(data)
  }
  
  for (i in 1:length(tp_support)) {
    sp_policy <- tp_support[i]
    pieces <- strsplit(sp_policy, "X")
    pieces <- pieces[[1]]
    num_encoding <- rep.int(0,M)
    
    for (m in 1:M) {
      if (grepl("atleast",pieces[m])) {
        num_encoding[m] <- 2
      } 
      else if (grepl("marginal",pieces[m])) {
        num_encoding[m] <- 3
      }
      else if (grepl("no",pieces[m])) {
        num_encoding[m] <- 1
      } #if not all of this, must be a non-gossip seed! encode it as 0
    }
    
    tp_support_num[[i]] <- num_encoding
  }
  
  collapsed_all <- list()
  
  collapsed_num <- 1
  
  bin_type_ind <- which(tp_support_num[[1]] > 1)
  
  total_vec <- list()
  for (m in 1:M) {
    if (m %in% bin_type_ind) {
      total_vec[[m]] <- 2:R
    }
    else {
      total_vec[[m]] <- tp_support_num[[1]][m]
    }
  }
  
  # Cartesian product of treatment-intensity vectors.
  # Base R is used because purrr::cross() is deprecated.
  fullcross <- do.call(
    expand.grid,
    c(total_vec, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  )
  
  bin_type_full <- lapply(
    seq_len(nrow(fullcross)),
    function(j) unname(as.numeric(fullcross[j, ]))
  )
  
  #tp_support_num <- list()
  
  # tp_support_num[[1]] <- c(2, 2, 2)
  # tp_support_num[[2]] <- c(3, 2, 2)
  # tp_support_num[[3]] <- c(2, 3, 2)
  
  atleast_ray_list <- list()
  
  for (j in 1:length(tp_support_num)) {
    
    atleast_ray_j <- list() #these are the individual rays 
    
    k = 1
    for (l in 1:length(bin_type_full)) {
      if (all(bin_type_full[[l]] >= tp_support_num[[j]])) { #go through the full list in that type and check if its part fo the ray
        atleast_ray_j[[k]] = bin_type_full[[l]]
        k = k + 1
      }
    }
    
    atleast_ray_list[[j]] <- atleast_ray_j
    #contruct full "ray" in a lsit
  }
  
  binary_switch <- expand.grid(replicate(length(tp_support_num), 0:1, simplify = FALSE)) 
  binary_switch <- binary_switch[-1,] #delete (c, c, c). This is the one policy in the complement of A U B U C
  binary_switch <- as.matrix(binary_switch)
  
  collapsed_type <- list() #matrix capturing collapsed policies relevant to bin type i
  
  for (j in 1:dim(binary_switch)[1]) { #go through all intersection combinations
    
    collapsed_policy_j <- bin_type_full #start with the full list ("full space")
    
    for (k in 1:length(atleast_ray_list)) {
      
      if (binary_switch[j, k] == 1) { #take the intersection
        collapsed_policy_j <- intersect(collapsed_policy_j, atleast_ray_list[[k]])
      }
      else if (binary_switch[j, k] == 0) { #take the intersection with the complement
        collapsed_policy_j <- intersect(collapsed_policy_j, setdiff(bin_type_full, atleast_ray_list[[k]]))
      }
      
    }
    
    
    if (length(collapsed_policy_j) > 0 ) {
      collapsed_policy_j <- do.call(rbind, collapsed_policy_j) 
      
      
      collapsed_all[[collapsed_num]] <- collapsed_policy_j
      
      collapsed_num <- collapsed_num + 1
    }
    
  }
  
  control_list <- c("noSeed", "noIncentive", "noReminder")
  
  policy_names <- c()
  
  for (i in 1:length(collapsed_all)) {
    
    policy_names[i] <- ""
    
    S = dim(collapsed_all[[i]])[1]
    
    #naming it first
    for (s in 1:S) {
      if (s > 1) {
        policy_names[i] <-paste0(policy_names[i], "OR")
      }
      for (m in 1:M) {
        if (collapsed_all[[i]][s,m] != 1) {
          policy_names[i] <- paste0(policy_names[i],if (m>1) "X" else "",tp_name[m],collapsed_all[[i]][s,m])
        }
      }
    }
    
    policy_names[i] <- paste0("POOLED_", policy_names[i])
    
    data[,policy_names[i]] <- 0
    
    for (s in 1:S) {
      partial_pol <- as.numeric(rep(TRUE,dim(data)[1]))
      for (m in 1:M) {
        if (collapsed_all[[i]][s,m] == 0) { #non gossip seed
          partial_pol <- partial_pol * data[,tp_name[m]]
        }
        else if (collapsed_all[[i]][s,m] == 1) { #control
          partial_pol <- partial_pol * data[,control_list[m]]
        }
        else if (collapsed_all[[i]][s,m] == 2) {
          partial_pol <- partial_pol * data[,paste0("low",tp_name[m])]
        }
        else if (collapsed_all[[i]][s,m] == 3) {
          partial_pol <- partial_pol * data[,paste0("high",tp_name[m])]
        }
      }
      
      data[,policy_names[i]] <- as.numeric(data[,policy_names[i]] | partial_pol)
    }
  }
  
  return(data)
}

#------------------------------------------------------------------------------#


get_relevant_sp_in_tp <- function(support_SP, tp_name) {
  indicator <- !logical(length(support_SP))
  
  for (sel in tp_name) {
    indicator <- indicator & grepl(sel, support_SP)
  }
  
  return(support_SP[indicator])
}


######################################################################################
#
# END
#
#######################################################################################   
