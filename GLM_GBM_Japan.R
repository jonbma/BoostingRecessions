
#####---------- Japan -----------------#######

#Transform and Season Japan#
zoo.JP_lag0_NO = transform_season_JP(include_TN = FALSE)
zoo.JP_lag0_TN = transform_season_JP(include_TN = TRUE, TN_SHORT = TRUE)


##################In-Sample############################

#Logit
glm.JP_h0d0 = glm.predict_roc(zoo.JP_lag0, forecast = 0, country = "JP")
glm.JP_h3d3 = glm.predict_roc(zoo.JP_lag0, forecast = 3, country = "JP")

glm.JP_h6d3 = glm.predict_roc(zoo.JP_lag0, forecast = 6, country = "JP")
glm.JP_h12d4 = glm.predict_roc(zoo.JP_lag0, forecast = 12, country = "JP")

#Best Logit Model
glm.JP_in_all_h6 = glm.out_roll_all(zoo.JP_lag0, h = 6, model = 0, c = "JP")
glm.JP_in_all_h12 = glm.out_roll_all(zoo.JP_lag0, h = 12, model = 0, c = "JP")

#Boosting
gbm.JP_h3d3 = gbm.forecast_lag(3,3,zoo.JP_lag0, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h6d3 = gbm.forecast_lag(6,3,zoo.JP_lag0, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h12d4 = gbm.forecast_lag(12,4,zoo.JP_lag0, "Japan", "bernoulli", train = 1.0, m = 1000) 

index = 2
print(list(gbm.JP_h0d3[index],gbm.JP_h3d3[index],gbm.JP_h6d3[index],gbm.JP_h12d4[index]))

###################Out-Sample##########################

#Logit Roll
glm.JP_h3_roc_roll = glm.roc_roll(zoo.JP_lag0, forecast = 3, country = "JP", varname = "OPTA", end_train = "1995-08-01")

#Logit Out-Of-Sample

#All GLM Out Of Sample STANDARD w/ ROC Score
glm.out_all_JP_h3 = glm.out_roll_all(zoo.JP_lag0_TN, h = 3, c = "JP", end = "1995-08-01", graph_param = FALSE, all_col = TRUE, model = 1)
glm.out_all_JP_h6 = glm.out_roll_all(zoo.JP_lag0_TN, h = 6, c = "JP", end = "1995-08-01", graph_param = FALSE, all_col = TRUE, model = 1)
glm.out_all_JP_h12 = glm.out_roll_all(zoo.JP_lag0_TN, h = 12, c = "JP", end = "1995-08-01", graph_param = FALSE, all_col = TRUE, model = 1)

#All GLM Out Of Sample ROLL w/ ROC Score
glm.out_all_JP_h3 = glm.out_roll_all(zoo.JP_lag0_TN, h = 3, c = "JP", graph = FALSE, all_col = TRUE, model = 2)
glm.out_all_JP_h6 = glm.out_roll_all(zoo.JP_lag0_TN, h = 6, c = "JP", graph = FALSE, all_col = TRUE, model = 2)
glm.out_all_JP_h12 = glm.out_roll_all(zoo.JP_lag0_TN, h = 12, c = "JP", graph = FALSE, all_col = TRUE, model = 2)

#Boost Full No Lags
gbm.JP_h3d3_roll_full = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0_TN, run.full = TRUE, country = "JP", max_m = 400, end_train = "1995-08-01")
gbm.JP_h6d3_roll_full = gbm.roc_roll(forecast = 6, lags = 0, zoo.JP_lag0_TN, run.full = TRUE, country = "JP", max_m = 400, end_train = "1995-08-01")
gbm.JP_h12d3_roll_full = gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0_TN, run.full = TRUE, country = "JP", max_m = 400, end_train = "1995-08-01")

#Boost Full No Lags
gbm.JP_h3d0_roll_full = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0_all, run.full = TRUE, country = "JP", max_m = 400, end_train = "1995-08-01")
gbm.JP_h6d0_roll_full = gbm.roc_roll(forecast = 6, lags = 0, zoo.JP_lag0_all, run.full = TRUE, country = "JP", m = 400, end_train = "1995-08-01")
gbm.JP_h12d0_roll_full = gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0_all, run.full = TRUE, country = "JP", m = 400, end_train = "1995-08-01")

gbm.JP_h3d0_roll_full_notankan = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0_no, run.full = TRUE, country = "JP", max_m = 4000, end_train = "1995-08-01")
gbm.JP_h3d0_roll_full_notankan400 = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0_no, run.full = TRUE, country = "JP", max_m = 400, end_train = "1995-08-01")
gbm.JP_h6d0_roll_full_notankan400 = gbm.roc_roll(forecast = 6, lags = 0, zoo.JP_lag0_no, run.full = TRUE, country = "JP", max_m = 400, end_train = "1995-08-01")
gbm.JP_h12d0_roll_full_notankan400 = gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0_no, run.full = TRUE, country = "JP", max_m = 400, end_train = "1995-08-01")

gbm.JP_h12d0_roll_full_tankan
gbm.JP_h3d0_roll_full_tankan = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0_all, run.full = TRUE, country = "JP", max_m = 400, end_train = "1995-08-01")




#Boost with Leading Index
CAB = c("JAPRECD","IPIRFG","IPIRFGMM","NEWJOB","NEWORD","NEWHOUSE","CONCONF","NIKKEICOM","INTSPREAD","GB10","IBORATE","STOCKPRIC","INVESTCLIM","OPTA","DISB", "JPNTI0015")
gbm.JP_h3d0_roll_lead400 = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 400)
gbm.JP_h3d0_roll_lead100 = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 100)
gbm.JP_h3d0_roll_lead800 = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 800)
gbm.JP_h3d0_roll_lead300 = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 300)
gbm.JP_h3d3_roll_lead361 = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0[,CAB], run.full = TRUE, m = 361)
gbm.JP_h3d0_roll_lead361 = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 361)

gbm.JP_h12d0_roll_lead400 = gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 400)
gbm.JP_h12d0_roll_lead27= gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 27, country = "JP")
gbm.JP_h12d4_roll_lead27= gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0[,CAB], run.full = TRUE, m = 27, country = "JP")
gbm.JP_h12d4_roll_lead400 = gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0[,CAB], run.full = TRUE, m = 400)

gbm.JP_h6d0_roll_lead400 = gbm.roc_roll(forecast = 6, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 400)
gbm.JP_h6d3_roll_lead400 = gbm.roc_roll(forecast = 6, lags = 3, zoo.JP_lag0[,CAB], run.full = TRUE, m = 400)
gbm.JP_h6d0_roll_lead399 = gbm.roc_roll(forecast = 6, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 399)
gbm.JP_h6d3_roll_lead399 = gbm.roc_roll(forecast = 6, lags = 3, zoo.JP_lag0[,CAB], run.full = TRUE, m = 399)

gbm.JP_h0d0_roll_lead400 = gbm.roc_roll(forecast = 0, lags = 0, zoo.JP_lag0[,CAB], run.full = TRUE, m = 400, )
gbm.JP_h0d3_roll_lead400 = gbm.roc_roll(forecast = 0, lags = 3, zoo.JP_lag0[,CAB], run.full = TRUE, m = 400)
gbm.JP_h3d3_roll_lead = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0[,CAB], run.full = TRUE)

##Analysis ####

#plot(gbm.US_h12d4_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 12 Months"), axes = TRUE)
plot(gbm.JP_h3d0_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("JP: Number of Positive Variables in Forecast 3 Months"), axes = TRUE)
plot(gbm.JP_h6d0_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("JP: Number of Positive Variables in Forecast 6 Months"), axes = TRUE)
plot(gbm.JP_h12d0_roll_full_tankan[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("JP: Number of Positive Variables in Forecast 12 Months"), axes = TRUE)

