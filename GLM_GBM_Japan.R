#####---------- Japan -----------------#######

#Transform and Season Japan#
zoo.JP_lag0_noTN = transform_season_JP(include_TN = FALSE)
zoo.JP_lag0_medium = transform_season_JP(include_TN = TRUE, TN_SHORT = TRUE)
zoo.JP_lag0_big = transform_season_JP(include_TN = TRUE, TN_SHORT = FALSE)
zoo.JP_lag0 = transform_season_JP(include_TN = TRUE, TN_SHORT = TRUE)
zoo.JP_lag0_BIG_TN_ONLY <- zoo.JP_lag0_big[,c(1,94:362)]

SHORT_LIST = c('JAPRECD','IPIRFG','IPIRFGMM','NEWJOB','NEWORD','NEWHOUSE','CONCONF','NIKKEICOM','INTSPREAD','GB10','IBORATE','STOCKPRIC','INVESTCLIM','OPTA','DISB')
TANKAN_LIST = c('BC_ALL_A','BC_ALL_F','IL_ALL_A','IL_MAN_A','DS_MAN_A','DS_MAN_F','FIN_ALL_A','FIN_MAN_A','EMP_ALL_A','EMP_ALL_F','EMP_MAN_A','EMP_MAN_F')
SHORT_TN_LIST = c(SHORT_LIST, TANKAN_LIST)
zoo.JP_lag0_short = zoo.JP_lag0_medium[,SHORT_TN_LIST]

##################In-Sample############################

#Logit
glm.in_JP_h3 = glm.roc_in(zoo.JP_lag0_big, forecast = 3, country = "JP", varname = "JPNTK0118")
glm.in_JP_h6 = glm.roc_in(zoo.JP_lag0_big, forecast = 3, country = "JP", varname = "JPNTK0199")
glm.in_JP_h12 = glm.roc_in(zoo.JP_lag0_big, forecast = 3, country = "JP", varname = "JPNTK0118")
glm.in_JP_h12 = glm.roc_in(zoo.JP_lag0_big, forecast = 24, country = "JP", varname = "JPNTK0118")


#glm.roc_in(zoo.JP_lag0_big, forecast = 12, country = "JP", varname = "JPNTK0595")
#Best Logit Model
#glm.JP_in_all_h6 = glm.out_roll_all(zoo.JP_lag0, h = 6, model = 0, c = "JP")
#glm.JP_in_all_h12 = glm.out_roll_all(zoo.JP_lag0, h = 12, model = 0, c = "JP")

######Boosting GRAPH
gbm.JP_h3d0_big = gbm.forecast_lag(3,0,zoo.JP_lag0_big, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h3d0_short = gbm.forecast_lag(3,0,zoo.JP_lag0_short, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h6d0_big = gbm.forecast_lag(6,0,zoo.JP_lag0_big, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h6d0_short = gbm.forecast_lag(6,0,zoo.JP_lag0_short, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h12d0_big = gbm.forecast_lag(12,0,zoo.JP_lag0_big, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h12d0_short = gbm.forecast_lag(12,0,zoo.JP_lag0_short, "Japan", "bernoulli", train = 1.0) 
########

write.csv(gbm.JP_h3d0_big[[1]], file = "gbm.JP_h3d0_big_in.csv")
###################Out-Sample##########################

#Logit Standard
glm.out(zoo.JP_lag0_big, forecast = 12, country = "JP", varname = "JPNTK0187")
glm.roc_roll(zoo.JP_lag0_big, forecast = 12, country = "JP", varname = "JPNTK0595")


#Logit Standard Out
glm.JP_h3_out_best = glm.out(zoo.JP_lag0_big, forecast = 3, country = "JP", varname = "JPNTK0199")
glm.JP_h6_out_best = glm.out(zoo.JP_lag0_big, forecast = 6, country = "JP", varname = "JPNTK0199")
glm.JP_h12_out_best = glm.out(zoo.JP_lag0_big, forecast = 12, country = "JP", varname = "JPNTK0959")

#Logit Standard
glm.JP_h3_out = glm.out(zoo.JP_lag0_big, forecast = 3, country = "JP", varname = "OPTA")
glm.JP_h6_out = glm.out(zoo.JP_lag0_big, forecast = 6, country = "JP", varname = "OPTA")
glm.JP_h12_out = glm.out(zoo.JP_lag0_big, forecast = 12, country = "JP", varname = "OPTA")

######Logit Roll Graph
glm.JP_h3_roll_best = glm.roc_roll(zoo.JP_lag0_big, forecast = 3, country = "JP", varname = "JPNTK0096")
glm.JP_h6_roll_best = glm.roc_roll(zoo.JP_lag0_big, forecast = 6, country = "JP", varname = "JPNTK0199")
glm.JP_h12_roll_best = glm.roc_roll(zoo.JP_lag0_big, forecast = 12, country = "JP", varname = "JPNTK0959")
######

glm.JP_h24 = glm.roc_roll(zoo.JP_lag0_short, forecast = 24, country = "JP", varname = "OPTA")



#All GLM Out Of Sample STANDARD w/ ROC Score
glm.out_all_JP_h3 = glm.out_roll_all(zoo.JP_lag0_big, h = 3, c = "JP", graph_param = FALSE, all_col = TRUE, model = 1)
glm.out_all_JP_h6 = glm.out_roll_all(zoo.JP_lag0_big, h = 6, c = "JP", graph_param = FALSE, all_col = TRUE, model = 1)
glm.out_all_JP_h12 = glm.out_roll_all(zoo.JP_lag0_big, h = 12, c = "JP", graph_param = FALSE, all_col = TRUE, model = 1)

#All GLM Out Of Sample ROLL w/ ROC Score
glm.out_roll_all_JP_h3_big = glm.out_roll_all(zoo.JP_lag0_big, h = 3, c = "JP", all_col = TRUE, model = 2, graph_param = FALSE)
glm.out_roll_all_JP_h6_big = glm.out_roll_all(zoo.JP_lag0_big, h = 6, c = "JP", all_col = TRUE, model = 2, graph_param = FALSE)
glm.out_roll_all_JP_h12_big = glm.out_roll_all(zoo.JP_lag0_big, h = 12, c = "JP", all_col = TRUE, model = 2, graph_param = FALSE)

#glm.out_roll_all_JP_h12_big = glm.out_roll_all(zoo.JP_lag0_BIG_TN_ONLY, h = 12, c = "JP", graph = FALSE, all_col = TRUE, model = 2)

save(glm.out_roll_all_JP_h3, file = "~/Google Drive/Independent Work/Saved RData/glm.out_roll_all_JP_h3_4102015.RData")
save(glm.out_roll_all_JP_h6, file = "~/Google Drive/Independent Work/Saved RData/glm.out_roll_all_JP_h6_4102015.RData")
save(glm.out_roll_all_JP_h12, file = "~/Google Drive/Independent Work/Saved RData/glm.out_roll_all_JP_h12_4102015.RData")
save(glm.out_roll_all_JP_h12_big, file = "~/Google Drive/Independent Work/Saved RData/glm.out_roll_all_JP_h12_big_4112015.RData")

#Boost Full  Lags
gbm.JP_h3d3_roll_big = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0_big, run.full = TRUE, country = "JP", max_m = 400)
gbm.JP_h6d3_roll_big = gbm.roc_roll(forecast = 6, lags = 6, zoo.JP_lag0_big, run.full = TRUE, country = "JP", max_m = 400)
gbm.JP_h12d3_roll_big = gbm.roc_roll(forecast = 12, lags = 12, zoo.JP_lag0_big, run.full = TRUE, country = "JP", max_m = 400)

#Boost Full No Lags
gbm.JP_h3d0_roll_full = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0_all, run.full = TRUE, country = "JP", max_m = 400, end_train = "1995-08-01")
gbm.JP_h6d0_roll_full = gbm.roc_roll(forecast = 6, lags = 0, zoo.JP_lag0_all, run.full = TRUE, country = "JP", m = 400, end_train = "1995-08-01")
gbm.JP_h12d0_roll_full = gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0_all, run.full = TRUE, country = "JP", m = 400, end_train = "1995-08-01")

#####Boost Big No Lags Graph
gbm.JP_h3d0_roll_big = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0_big, run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
gbm.JP_h6d0_roll_big = gbm.roc_roll(forecast = 6, lags = 0, zoo.JP_lag0_big, run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
gbm.JP_h12d0_roll_big = gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0_big, run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
#########

write.csv(gbm.JP_h6d0_roll_big[[4]], file = "gbm.JP_h6d0_roll_big.csv")

#######Boost Short No Lags Graph
gbm.JP_h3d0_roll_short = gbm.roc_roll(forecast = 3, lags = 0, zoo.JP_lag0_short, run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
gbm.JP_h6d0_roll_short = gbm.roc_roll(forecast = 6, lags = 0, zoo.JP_lag0_short, run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
gbm.JP_h12d0_roll_short = gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0_short, run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
#######

write.csv(gbm.JP_h3d0_roll_short[[4]], file = "gbm.JP_h3d0_roll_short.csv")




gbm.JP_h3d3_roll_short = gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0_short, run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
gbm.JP_h6d3_roll_short = gbm.roc_roll(forecast = 6, lags = 3, zoo.JP_lag0_short, run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
gbm.JP_h12d4_roll_short = gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0_short, run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")



SHORT = c('JAPRECD','IPIRFG','IPIRFGMM','NEWJOB','NEWORD','NEWHOJPE','CONCONF','NIKKEICOM','INTSPREAD','GB10','IBORATE','STOCKPRIC','INVESTCLIM','OPTA','JPNTK0959')
gbm.JP_h12d0_roll_better = gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0_big[,c('JAPRECD','OPTA','CONCONF','JPNTK0959')], run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
gbm.JP_h12d0_roll_better = gbm.roc_roll(forecast = 12, lags = 0, zoo.JP_lag0_big[,SHORT], run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")
gbm.JP_h12d3_roll_better = gbm.roc_roll(forecast = 12, lags = 3, zoo.JP_lag0_big[,SHORT], run.full = TRUE, country = "JP", max_m = 400, input_end = "1995-08-01")





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
plot(gbm.JP_h3d0_roll_big[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("JP: Number of Positive Variables in Forecast 3 Months"), axes = TRUE)
plot(gbm.JP_h3d0_roll_big[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("JP: Number of Positive Variables in Forecast 6 Months"), axes = TRUE)
plot(gbm.JP_h3d0_roll_big[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("JP: Number of Positive Variables in Forecast 12 Months"), axes = TRUE)

plot_zoo_REC(gbm.JP_h3d0_roll_big[[5]], "positive variables", country = "JP", TITLE="Japan: Positive Variables Selected by Boosting in Large Dataset for Horizon = 3 months")
plot_zoo_REC(gbm.JP_h6d0_roll_big[[5]], "positive variables", country = "JP", TITLE="Japan: Positive Variables Selected by Boosting in Large Dataset for Horizon = 6 months")
plot_zoo_REC(gbm.JP_h12d0_roll_big[[5]], "positive variables", country = "JP", TITLE="")


##T-test for AUC##

#In-Sample
roc.test(glm.in_JP_h3, gbm.JP_h3d0_big[[3]], alternative = "less")
roc.test(glm.in_JP_h6, gbm.JP_h6d0_big[[3]], alternative = "less")
roc.test(glm.in_JP_h12, gbm.JP_h12d0_big[[3]], alternative = "less")

roc.test(glm.in_JP_h3, gbm.JP_h3d0_short[[3]], alternative = "less")
roc.test(glm.in_JP_h6, gbm.JP_h6d0_short[[3]], alternative = "less")
roc.test(glm.in_JP_h12, gbm.JP_h12d0_short[[3]], alternative = "less")

roc.test(gbm.JP_h3d0_big[[3]], gbm.JP_h3d0_short[[3]], alternative = "greater")
roc.test(gbm.JP_h6d0_big[[3]], gbm.JP_h6d0_short[[3]], alternative = "greater")
roc.test(gbm.JP_h12d0_big[[3]], gbm.JP_h12d0_short[[3]], alternative = "greater")

#Out-Of-Sample
roc.test(glm.JP_h3_roll_best[[1]], gbm.JP_h3d0_roll_short[[3]], alternative = "greater")
roc.test(glm.JP_h6_roll_best[[1]], gbm.JP_h6d0_roll_short[[3]], alternative = "greater")
roc.test(glm.JP_h12_roll_best[[1]], gbm.JP_h12d0_roll_short[[3]], alternative = "less")

roc.test(glm.JP_h3_roll_best[[1]], gbm.JP_h3d0_roll_big[[3]], alternative = "greater")
roc.test(glm.JP_h6_roll_best[[1]], gbm.JP_h6d0_roll_big[[3]], alternative = "greater")
roc.test(glm.JP_h12_roll_best[[1]], roc_JP_h12_big, alternative = "greater")

roc.test(gbm.JP_h3d0_roll_short[[3]], gbm.JP_h3d0_roll_big[[3]], alternative = "less")
roc.test(gbm.JP_h6d0_roll_short[[3]], gbm.JP_h6d0_roll_big[[3]], alternative = "greater")
roc.test(gbm.JP_h12d0_roll_short[[3]], roc_JP_h12_big, alternative = "greater")
