# Cleaning the working directory
rm(list=ls())


# Gathering new losses functions
MAE <- function(predicted, observed){
  mean(abs(predicted-observed))
}

COS_SIM <- function(predicted,observed){
  c(crossprod(predicted,observed)/(sqrt(crossprod(predicted))*sqrt(crossprod(observed))))
}

# Final dataset 
final_data_frame <- data.frame(dataset = NA,
                               n = NA,model = NA, time = NA, RMSE = NA, MAE = NA, COS = NA)


# Importing packages
library(purrr)
library(rmachines)
# Selecting one scenario
for(n in c(30,100,300,1000)){
  for(scenario in c("one","two","three","four","five","six","seven","eigth")){
  # for(scenario in "two"){
mod_result <- readRDS(file = paste0("storing_results/WITH_GAMMA_FIX_B_25_n_",n,"_holdout_30_scenario_",scenario,".Rds"))


kernel_lin_mod <- matrix(NA, nrow = 30, ncol = 21)
kernel_pol_mod <- matrix(NA, nrow = 30, ncol = 21)
kernel_rbf_mod <- matrix(NA, nrow = 30, ncol = 147)
kernel_lap_mod <- matrix(NA, nrow = 30, ncol = 147)
total_time_svr <- 0
total_time_rm <- 0

rm_mod <- numeric()
for(i in 1:30){
  y_validation <- mod_result[[i]]$validation$y
  y_test <- mod_result[[i]]$test$y 
  
  kernel_lin_mod[i,] <- map_dbl(mod_result[[i]]$svr_lin, ~RMSE(predicted = .x$validation_pred_svr,observed = y_validation))  
  kernel_pol_mod[i,] <- map_dbl(mod_result[[i]]$svr_pol, ~RMSE(predicted = .x$validation_pred_svr,observed = y_validation))  
  kernel_rbf_mod[i,] <- map_dbl(mod_result[[i]]$svr_rbf, ~RMSE(predicted = .x$validation_pred_svr,observed = y_validation))  
  kernel_lap_mod[i,] <- map_dbl(mod_result[[i]]$svr_lap, ~RMSE(predicted = .x$validation_pred_svr,observed = y_validation))  
  
  rm_mod[i] <- RMSE(predicted = mod_result[[i]]$rm$rrm_predict,observed = y_validation)
  
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


rmse_vec <- numeric()
# Plotting the RMSE for all data.sets
for(i in 1:30){
rmse_vec[i] <- RMSE(mod_result[[i]]$svr_lap[[19]]$validation_pred_svr,mod_result[[i]]$test$y)
}

final_data_frame <- add_row(final_data_frame,data.frame(dataset = scenario,
                                       n = n,model = "svr_lin",
                                       time = total_time_svr_lin,
                                       RMSE = mean(map_dbl(mod_result,
                                                           ~RMSE(predicted = .x$svr_lin[[best_lin]]$test_svr_lin,
                                                                observed = .x$test$y))),
                                       
                                       MAE = mean(map_dbl(mod_result,
                                                          ~MAE(predicted = .x$svr_lin[[best_lin]]$test_svr_lin,
                                                                observed = .x$test$y))),
                                       COS = mean(map_dbl(mod_result,
                                                          ~COS_SIM(predicted = .x$svr_lin[[best_lin]]$test_svr_lin,
                                                               observed = .x$test$y)))))

final_data_frame <- add_row(final_data_frame,data.frame(dataset = scenario,
                                                        n = n,model = "svr_pol",
                                                        time = total_time_svr_pol,
                                                        RMSE = mean(map_dbl(mod_result,
                                                                            ~RMSE(predicted = .x$svr_pol[[best_pol]]$test_svr_pol,
                                                                                  observed = .x$test$y))),
                                                        
                                                        MAE = mean(map_dbl(mod_result,
                                                                           ~MAE(predicted = .x$svr_pol[[best_pol]]$test_svr_pol,
                                                                                observed = .x$test$y))),
                                                        COS = mean(map_dbl(mod_result,
                                                                           ~COS_SIM(predicted = .x$svr_pol[[best_pol]]$test_svr_pol,
                                                                                    observed = .x$test$y)))))

final_data_frame <- add_row(final_data_frame,data.frame(dataset = scenario,
                                                        n = n,model = "svr_rbf",
                                                        time = total_time_svr_rbf,
                                                        RMSE = mean(map_dbl(mod_result,
                                                                            ~RMSE(predicted = .x$svr_rbf[[best_rbf]]$test_svr_rbf,
                                                                                  observed = .x$test$y))),
                                                        
                                                        MAE = mean(map_dbl(mod_result,
                                                                           ~MAE(predicted = .x$svr_rbf[[best_rbf]]$test_svr_rbf,
                                                                                observed = .x$test$y))),
                                                        COS = mean(map_dbl(mod_result,
                                                                           ~COS_SIM(predicted = .x$svr_rbf[[best_rbf]]$test_svr_rbf,
                                                                                    observed = .x$test$y)))))


final_data_frame <- add_row(final_data_frame,data.frame(dataset = scenario,
                                                        n = n,model = "svr_lap",
                                                        time = total_time_svr_lap,
                                                        RMSE = mean(map_dbl(mod_result,
                                                                            ~RMSE(predicted = .x$svr_lap[[best_lap]]$test_svr_lap,
                                                                                  observed = .x$test$y))),
                                                        
                                                        MAE = mean(map_dbl(mod_result,
                                                                           ~MAE(predicted = .x$svr_lap[[best_lap]]$test_svr_lap,
                                                                                observed = .x$test$y))),
                                                        COS = mean(map_dbl(mod_result,
                                                                           ~COS_SIM(predicted = .x$svr_lap[[best_lap]]$test_svr_lap,
                                                                                    observed = .x$test$y)))))

final_data_frame <- add_row(final_data_frame,data.frame(dataset = scenario,
                                                        n = n,model = "rm",
                                                        time = as.numeric(total_time_rm),
                                                        RMSE = mean(map_dbl(mod_result,
                                                                            ~RMSE(predicted = .x$rm$rrm_predict,
                                                                                  observed = .x$test$y))),
                                                        
                                                        MAE = mean(map_dbl(mod_result,
                                                                           ~MAE(predicted = .x$rm$rrm_predict,
                                                                                observed = .x$test$y))),
                                                        COS = mean(map_dbl(mod_result,
                                                                           ~COS_SIM(predicted = .x$rm$rrm_predict,
                                                                                    observed = .x$test$y)))))
}}
# Gathering the total time
total_time_svr
total_time_rm


# Ratio between the tuning and the RM
cat(" RM is ", round(total_time_svr/as.numeric(total_time_rm),2)," times faster than SVR grid.search tuning for simulation",scenario)

final_data_frame_clean <- final_data_frame[-1,]

final_data_frame_clean %>% group_by(model) %>% summarise(median(RMSE),median(MAE),median(COS))
saveRDS(final_data_frame_clean, file =  "New_Big_50_time_final_data_simulations.Rds")
