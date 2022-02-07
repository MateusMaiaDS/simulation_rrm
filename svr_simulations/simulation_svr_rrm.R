# Loading packages
setwd("~/Documents/svr_simulations")
library(kernlab)
library(rmachines)
library(tidyverse)
source("simulations_gen_files.R")


# Setting a seed
set.seed(42)

# Generating the cross_validation object
n_rep <- 30

for (n in c(30,100,1000)) {

# Generating the model eigth
sim_data <- model_eigth(n = n,
          seed = 42)


# Creating the grid for svr
C_grid <- 2^seq(-3,3,by = 1)
gamma_grid <- 2^seq(-3,3,by = 1)
eps_grid <- c(10e-3,10e-2,10e-1)

# Creating the grid
tuning_grid <- expand.grid( C = C_grid,
                             gamma = gamma_grid,
                             eps = eps_grid)

# Saving the list in "i"
store_result_list <- list()

# Doing the same for regression random machines
result_tuning_rm <-list()

for(i in 1:n_rep) {
  
  
  # Creating all the cross validation objects
  
  cv <- cross_validation(data = sim_data,
                         training_ratio = 0.7,
                         validation_ratio = 0.2,seed = 42+i)
  
  train <- cv$train_sample
  validation <- cv$validation_sample
  test <- cv$test_sample
  
  # Starting by the linear first
  
  # Reducing the tuning grid for the linear case where there's no hypera-parameter
  tuning_lin <- tuning_grid %>% filter(gamma == 1)
  
  # Creating a list to save the model
  
  result_tuning_svr_lin <- list()
  result_tuning_svr_pol <- list()
  result_tuning_svr_rbf <- list()
  result_tuning_svr_lap <- list()
  
  # Iterating over the tuning 
  for(k in 1:nrow(tuning_lin)){
    
      
      # Saving the time for model init
      time_init <- Sys.time()
      
      mod_svr_lin <- kernlab::ksvm(y ~ ., data = train,
                               C = tuning_lin[k,"C"],
                               epsilon = tuning_lin[k,"eps"],
                               kernel = "vanilladot")
      # Saving those values
      validation_svr_lin <- predict(mod_svr_lin, newdata = validation)
      test_svr_lin <- predict(mod_svr_lin, newdata = test)
       
      time_end <- Sys.time()
      
      result_tuning_svr_lin[[k]] <- list(time = (time_end-time_init),
                                     validation_pred_svr = validation_svr_lin,
                                     test_svr_lin = test_svr_lin,
                                     C = tuning_lin[k,"C"],
                                     epsilon = tuning_lin[k,"eps"])
      
      # Saving the time for model init
      time_init <- Sys.time()
      
      mod_svr_pol <- kernlab::ksvm(y ~ ., data = train,
                                   C = tuning_lin[k,"C"],
                                   epsilon = tuning_lin[k,"eps"],
                                   kernel = "polydot")
      # Saving those values
      validation_svr_pol <- predict(mod_svr_pol, newdata = validation)
      test_svr_pol <- predict(mod_svr_pol, newdata = test)
      
      time_end <- Sys.time()
      
      result_tuning_svr_pol[[k]] <- list(time = (time_end-time_init),
                                         validation_pred_svr = validation_svr_pol,
                                         test_svr_pol = test_svr_pol,
                                         C = tuning_lin[k,"C"],
                                         epsilon = tuning_lin[k,"eps"])
  }
  
  # Iterating over the parameters of the RBF and Laplacian kernel
  
  for(k in 1:nrow(tuning_grid)){
    
    # Saving the time for model init
    time_init <- Sys.time()
    
    mod_svr_rbf <- kernlab::ksvm(y ~ ., data = train,
                                 C = tuning_grid[k,"C"],
                                 epsilon = tuning_grid[k,"eps"],
                                 kpar = list(sigma = tuning_grid[k,"gamma"]),
                                 kernel = "rbfdot"
                                 )
    # Saving those values
    validation_svr_rbf <- predict(mod_svr_rbf, newdata = validation)
    test_svr_rbf <- predict(mod_svr_rbf, newdata = test)
    
    time_end <- Sys.time()
    
    result_tuning_svr_rbf[[k]] <- list(time = (time_end-time_init),
                                       validation_pred_svr = validation_svr_rbf,
                                       test_svr_rbf = test_svr_rbf,
                                       C = tuning_grid[k,"C"],
                                       epsilon = tuning_grid[k,"eps"],
                                       gamma = tuning_grid[k,"gamma"])
    
    # Saving the time for model init
    time_init <- Sys.time()
    
    mod_svr_lap <- kernlab::ksvm(y ~ ., data = train,
                                 C = tuning_grid[k,"C"],
                                 epsilon = tuning_grid[k,"eps"],
                                 kpar = list(sigma = tuning_grid[k,"gamma"]),
                                 kernel = "laplacedot")
    # Saving those values
    validation_svr_lap <- predict(mod_svr_lap, newdata = validation)
    test_svr_lap <- predict(mod_svr_lap, newdata = test)
    
    time_end <- Sys.time()
    
    result_tuning_svr_lap[[k]] <- list(time = (time_end-time_init),
                                       validation_pred_svr = validation_svr_lap,
                                       test_svr_lap = test_svr_lap,
                                       C = tuning_grid[k,"C"],
                                       epsilon = tuning_grid[k,"eps"],
                                       tuning_grid[k,"gamma"])
      
      
    
  }
  
  
  
  
  # Creating a data.frame to store the results from svm.lin and so on
  time_init_rm <- Sys.time()
  rm_mod <- regression_random_machines(formula = y~.,
                                train = train,validation = validation,
                                test = test,loss_function = RMSE,
                                boots_size =  25)
  rrm_predict <- predict_rrm_model(mod = rm_mod)
  time_end_rm <- Sys.time()
  
  result_tuning_rm[[i]] <- list(time = (time_end_rm - time_init_rm),
                                rrm_predict = rrm_predict)
  
  
  
  # Storing all results
  store_result_list[[i]] <- list(train = train,
                                 validation = validation,
                                 test = test,
                                 svr_lin = result_tuning_svr_lin,
                                 svr_pol = result_tuning_svr_pol,
                                 svr_rbf = result_tuning_svr_rbf,
                                 svr_lap = result_tuning_svr_lap,
                                 rm = result_tuning_rm[[1]])
  
  # Printing the iteration number
  print(i)
}


# Saving models
saveRDS(store_result_list,
        file = paste0("storing_results/n_",n,"_holdout_",n_rep,"_scenario_eigth.Rds"))
}
