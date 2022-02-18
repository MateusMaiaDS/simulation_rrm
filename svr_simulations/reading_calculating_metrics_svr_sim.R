  # Cleaning the working directory
  rm(list=ls())
  
  # Importing packages
  library(purrr)
  library(rmachines)
  # Selecting one scenario
  n <- 100
  scenario <- "eigth"
  
  mod_result <- readRDS(file = paste0("storing_results/n_",n,"_holdout_30_scenario_",scenario,".Rds"))
  
  
  kernel_lin_mod <- matrix(NA, nrow = 30, ncol = 21)
  kernel_pol_mod <- matrix(NA, nrow = 30, ncol = 21)
  kernel_rbf_mod <- matrix(NA, nrow = 30, ncol = 147)
  kernel_lap_mod <- matrix(NA, nrow = 30, ncol = 147)
  total_time_svr <- 0
  total_time_rm <- 0
  
  rm_mod <- numeric()
  for(i in 1:30){
    y_test <- mod_result[[i]]$test$y 
    
    kernel_lin_mod[i,] <- map_dbl(mod_result[[i]]$svr_lin, ~RMSE(predicted = .x$validation_pred_svr,observed = y_test))  
    kernel_pol_mod[i,] <- map_dbl(mod_result[[i]]$svr_pol, ~RMSE(predicted = .x$validation_pred_svr,observed = y_test))  
    kernel_rbf_mod[i,] <- map_dbl(mod_result[[i]]$svr_rbf, ~RMSE(predicted = .x$validation_pred_svr,observed = y_test))  
    kernel_lap_mod[i,] <- map_dbl(mod_result[[i]]$svr_lap, ~RMSE(predicted = .x$validation_pred_svr,observed = y_test))  
    
    rm_mod[i] <- RMSE(predicted = mod_result[[i]]$rm$rrm_predict,observed = y_test)
    
    total_time_svr_lin =+ sum(map_dbl(mod_result[[i]]$svr_lin, ~.x$time))
    total_time_svr_pol=+ sum(map_dbl(mod_result[[i]]$svr_pol, ~.x$time))
    total_time_svr_rbf =+ sum(map_dbl(mod_result[[i]]$svr_rbf, ~.x$time))
    total_time_svr_lap =+ sum(map_dbl(mod_result[[i]]$svr_lap, ~.x$time))
    
    total_time_rm =+ mod_result[[i]]$rm$time
  }
  
  best_lin <- kernel_lin_mod %>% apply(2,mean) %>% which.min
  best_pol <- kernel_pol_mod %>% apply(2,mean) %>% which.min
  best_rbf <- kernel_rbf_mod %>% apply(2,mean) %>% which.min
  best_lap <- kernel_lap_mod %>% apply(2,mean) %>% which.min
  
  
  # Plotting the RMSE for all data.sets
  
  boxplot(kernel_lin_mod[,best_lin],kernel_pol_mod[best_pol,],kernel_rbf_mod[,best_rbf],kernel_lap_mod[,best_lap],rm_mod,
          names = c("Lin.","Pol","Rbf","Lap","RM"))
  
  # Gathering the total time
  total_time_svr
  total_time_rm
  
  
  # Ratio between the tuning and the RM
  cat(" RM is ", round(total_time_svr/as.numeric(total_time_rm),2)," times faster than SVR grid.search tuning for simulation",scenario)
