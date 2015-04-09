
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
roc.glm.JP_h0_roll = glm.roc_roll(zoo.JP_lag0, forecast = 0, country = "JP")
roc.glm.JP_h3_roll = glm.roc_roll(zoo.JP_lag0, forecast = 3, country = "JP", varname = "PMNO")

roc.glm.JP_h6_roll = glm.roc_roll(zoo.JP_lag0, forecast = 6, country = "JP")
roc.glm.JP_h12_roll = glm.roc_roll(zoo.JP_lag0, forecast = 12, country = "JP")

#Logit Out-Of-Sample
glm.out.OPTA0 = glm.out(zoo.JP_lag0, forecast = 0, country = "JP", varname = "OPTA", end_train = "1985-08-01")
glm.out.OPTA3 = glm.out(zoo.JP_lag0, forecast = 3, country = "JP", varname = "OPTA", end_train = "1985-08-01")
glm.out.OPTA6 = glm.out(zoo.JP_lag0, forecast = 6, country = "JP", varname = "OPTA", end_train = "1985-08-01")
glm.out.OPTA12 = glm.out(zoo.JP_lag0_all, forecast = 12, country = "JP", varname = "OPTA", end_train = "1985-08-01")
glm.out.JPN0015_12 = glm.out(zoo.JP_lag0, forecast = 12, country = "JP", varname = "JPNTI0015", end_train = "1985-08-01")
glm.out.JPNTK0011 = glm.out(zoo.JP_lag0, forecast = 12, country = "JP", varname = "JPNTK0011", end_train = "1985-08-01")

glm.out.RAWMATERIAL_nomean = glm.out(zoo.JP_lag0, forecast = 12, country = "JP", varname = "JPNVT0060_NOMEAN", end_train = "1985-08-01")
glm.out.RAWMATERIAL = glm.out(zoo.JP_lag0, forecast = 12, country = "JP", varname = "JPNVT0060", end_train = "1985-08-01")

#glm.out.JPNTI0003_RAW = glm.out(zoo.JP_lag0, forecast = 3, country = "JP", varname = "JPNTI0003_RAW", end_train = "1985-08-01", graph = TRUE)
glm.out.JPNTI0003 = glm.out(zoo.JP_lag0, forecast = 3, country = "JP", varname = "JPNTI0003", end_train = "1985-08-01", graph = TRUE)

glm.out.TANKAN = glm.out(zoo.JP_lag0, forecast = 12, country = "JP", varname = "JPNTK1954", end_train = "1985-08-01", graph = TRUE)
glm.out.TANKAN3 = glm.out(zoo.JP_lag0, forecast = 3, country = "JP", varname = "JPNTK0118", end_train = "1985-08-01", graph = TRUE)

#Logit-Out-Of-Sample H = 12
glm.out.IR12 = glm.out(zoo.JP_lag0, forecast = 12, country = "JP", varname = "IRGSTBJ", end_train = "1990-08-01", graph = TRUE)



#All GLM Out Of Sample w/ ROC Score
glm.out_all_JP_h3_19950801 = glm.out_all_JP_h3
glm.out_all_JP_h6_19950801 = glm.out_all_JP_h6
glm.out_all_JP_h12_19950801 = glm.out_all_JP_h12


glm.out_all_JP_h0 = glm.out_all(zoo.JP_lag0, h = 0, c = "JP", end = "1995-08-01", graph_param = FALSE, all_col = TRUE)
glm.out_all_JP_h3 = glm.out_all(zoo.JP_lag0, h = 3, c = "JP", end = "1995-08-01", graph_param = FALSE, all_col = TRUE)
glm.out_all_JP_h6 = glm.out_all(zoo.JP_lag0, h = 6, c = "JP", end = "1995-08-01", graph_param = FALSE, all_col = TRUE)
glm.out_all_JP_h12 = glm.out_all(zoo.JP_lag0, h = 12, c = "JP", end = "1995-1-01", graph_param = FALSE, all_col = TRUE)


#Boost Mini
gbm.JP_h0d3_roll = gbm.roc_roll(forecast = 0, lags = 3, zoo.JP_lag0, run.full = FALSE, iter = 10)
gbm.JP_h3d3_roll = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0, run.full = FALSE, iter = 20)
gbm.JP_h6d3_roll = gbm.roc_roll(forecast = 6, lags = 3, zoo.JP_lag0, run.full = FALSE, iter = 80)
gbm.JP_h12d4_roll = gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0, run.full = FALSE, iter = 50)

#Boost Full
#gbm.JP_h0d3_roll_full = gbm.roc_roll(forecast = 0, lags = 3, zoo.JP_lag0, run.full = TRUE, country = "JP", m = 400)
gbm.JP_h3d3_roll_full = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0, run.full = TRUE, country = "JP", m = 400, train_end )
gbm.JP_h6d3_roll_full = gbm.roc_roll(forecast = 6, lags = 3, zoo.JP_lag0, run.full = TRUE, country = "JP", m = 400)
gbm.JP_h12d4_roll_full = gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0, run.full = TRUE, country = "JP", m = 400)

gbm.JP_h0d3_roll_full2 = gbm.roc_roll(forecast = 0, lags = 3, zoo.JP_lag0, run.full = TRUE)
gbm.JP_h3d3_roll_full2 = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0, run.full = TRUE, m = 400)
gbm.JP_h6d3_roll_full2 = gbm.roc_roll(forecast = 6, lags = 3, zoo.JP_lag0, run.full = TRUE, m = 400)
gbm.JP_h12d4_roll_full2 = gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0, run.full = TRUE, m = 400)

gbm.JP_h3d3_roll_full361 = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0, run.full = TRUE, m = 361)

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

