
#####---------- United States -----------------#######

#Transform and Season#
zoo.US_lag0 = transform_season_US(df.US)

############## In-Sample #####################
#Logit
roc.glm.US_h0d0 = glm.predict_roc(zoo.US_lag0, forecast = 0, country = "US")
roc.glm.US_h3d3 = glm.predict_roc(zoo.US_lag0, forecast = 3, country = "US")
roc.glm.US_h6d3 = glm.predict_roc(zoo.US_lag0, forecast = 6, country = "US")
roc.glm.US_h12d4 = glm.predict_roc(zoo.US_lag0, forecast = 12, country = "US")

#Boost
gbm.US_h0d3 = gbm.forecast_lag(0,3,zoo.US_lag0, "United States", train = 1.0)
gbm.US_h3d3 = gbm.forecast_lag(3,3,zoo.US_lag0, "United States", train = 1.0)
gbm.US_h6d3 = gbm.forecast_lag(6,3,zoo.US_lag0, "United States", train = 1.0)
gbm.US_h12d4= gbm.forecast_lag(forecast = 12,lags = 4,zoo.US_lag0, "United States", train = 1.0)

#Boost w/ no lags
gbm.US_h3d0 = gbm.forecast_lag(3,0,zoo.US_lag0, "United States", train = 1.0)

################## Out-Of-Sample ###################
#Logit Roll
glm.roll.US_h3_PMNO
glm.roll.US_h3 = glm.roc_roll(zoo.US_lag0, forecast = 3, varname = "PMNO", country = "US")
glm.roll.US_h6 = glm.roc_roll(zoo.US_lag0, forecast = 6, varname = "SFYGT5", country = "US")
glm.roll.US_h12 = glm.roc_roll(zoo.US_lag0, forecast = 12, country = "US", varname = "SFYGT10")

#Logit Out-Of-Sample
glm.roll.termspread3 = glm.roc_roll(zoo.US_lag0, forecast = 3, country = "US", varname = "TERMSPREAD")
glm.out.termspread3 = glm.out(zoo.US_lag0, forecast = 3, country = "US", varname = "TERMSPREAD")

glm.roll.termspread17 = glm.roc_roll(zoo.US_lag0, forecast = 17, country = "US", varname = "TERMSPREAD")
glm.out.termspread17 = glm.out(zoo.US_lag0, forecast = 17, country = "US", varname = "TERMSPREAD", logit = "TRUE")

glm.roll.pmp3 = glm.roc_roll(zoo.US_lag0, forecast = 3, country = "US", varname = "PMP")
glm.out.pmp3 = glm.out(zoo.US_lag0, forecast = 3, country = "US", varname = "PMP", end_train = "1985-08-01")

#Logit ALL Out-of_sample
glm.out_all_US_h0 = glm.out_all(zoo.US_lag0, h = 0, c = "US", end = "1985-08-01", graph_param = FALSE, all_col = TRUE)
glm.out_all_US_h3 = glm.out_all(zoo.US_lag0, h = 3, c = "US", end = "1985-08-01", graph_param = FALSE, all_col = TRUE)
glm.out_all_US_h6 = glm.out_all(zoo.US_lag0, h = 6, c = "US", end = "1985-08-01", graph_param = FALSE, all_col = TRUE)
glm.out_all_US_h12 = glm.out_all(zoo.US_lag0, h = 12, c = "US", end = "1985-08-01", graph_param = FALSE, all_col = TRUE)

#Logit ALL Roll
glm.out_all_roll_USh3 = glm.out_all(zoo.US_lag0, h =3, c = "US", OUT = FALSE, all_col = TRUE)
glm.out_all_roll_USh6 = glm.out_all(zoo.US_lag0, h =6, c = "US", OUT = FALSE, all_col = TRUE)
glm.out_all_roll_USh12 = glm.out_all(zoo.US_lag0, h =12, c = "US", OUT = FALSE, all_col = TRUE)


#Boost Mini
gbm.US_h3d0_roll_mini = gbm.roc_roll(forecast = 3, lags = 0, zoo.US_lag0, run.full = FALSE, country = "US", runs = 10)

#Boost Full
gbm.US_h3d3_roll_full = gbm.roc_roll(forecast = 3, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h6d3_roll_full = gbm.roc_roll(forecast = 6, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h12d4_roll_full = gbm.roc_roll(forecast = 12, lags = 4, zoo.US_lag0, run.full = TRUE, country = "US")

save(gbm.US_h3d3_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d3_roll_full_472015.RData")
save(gbm.US_h6d3_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d3_roll_full_472015.RData")
save(gbm.US_h12d4_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d4_roll_full_472015.RData")





#Boost with w/ Lags w/ Fewer Trees
gbm.US_h3d3_roll_full200 = gbm.roc_roll(forecast = 3, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US", m = 200)

#Boost with w/ Lags w/ More Trees

#Boost w/ No Lags
gbm.US_h0d0_roll_full400 = gbm.roc_roll(forecast = 0, lags = 0, zoo.US_lag0, run.full = TRUE, country = "US", m = 400)
gbm.US_h3d0_roll_full400 = gbm.roc_roll(forecast = 3, lags = 0, zoo.US_lag0, run.full = TRUE, country = "US", m = 400)
gbm.US_h6d0_roll_full400 = gbm.roc_roll(forecast = 6, lags = 0, zoo.US_lag0, run.full = TRUE, country = "US", m = 400)
gbm.US_h12d0_roll_full400 = gbm.roc_roll(forecast = 12, lags = 0, zoo.US_lag0, run.full = TRUE, country = "US", m = 400)
gbm.US_h3d0_roll_full200 = gbm.roc_roll(forecast = 3, lags = 0, zoo.US_lag0, run.full = TRUE, country = "US", m = 200)

#Travis Boost
gbm.US_h12d0_roll_travis = gbm.roc_roll(forecast = 12, lags = 0, zoo.US2, run.full = TRUE, country = "US", m = 500, wind = 132)
gbm.US_h12d0_travis = gbm.forecast_lag(forecast = 12, lags = 0, zoo.US2, country = "US", m = 400)

#Boost with 1 Variable
gbm.US_h12d3_roll_SFYGT10 = gbm.roc_roll(forecast = 12, lags = 3,  zoo.US_lag0[,c('USRECD','SFYGT10', 'PMP')], run.full = TRUE, country = "US", m = 400, wind = 145, steps = 0.1)
gbm.US_h12d0_roll_SFYGT10AAA = gbm.roc_roll(forecast = 12, lags = 0,  zoo.US_lag0[,c('USRECD','SFYGT10', 'SFYAAAC')], run.full = TRUE, country = "US", m = 400, wind = 180, steps = 0.1)
gbm.US_h12d0_roll_SFYGT10BAA = gbm.roc_roll(forecast = 12, lags = 0,  zoo.US_lag0[,c('USRECD','SFYGT10', 'SFYBAAC')], run.full = TRUE, country = "US", m = 400, wind = 180, steps = 0.1)
gbm.US_h12d0_roll_SFYGT10PM = gbm.roc_roll(forecast = 12, lags = 0,  zoo.US_lag0[,c('USRECD','SFYGT10', 'PMDEL')], run.full = TRUE, country = "US", m = 400, wind = 145, steps = 0.1)

#Travis MBoost
mboost_model = mboost(NBER ~ slope + level + curve,data = df.US, baselearner = "bols")
mboost_fit(mboost_model, df.US$NBER)

glmb1 = glmboost(NBER ~ . ,
                 data = zoo.US2, 
                 control = boost_control(mstop = 100))

maic <- AIC(glmb1)

glmb1 = glmboost(NBER ~ . ,data = zoo.US2)



##Analysis ####

plot(gbm.US_h3d3_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 3 Months"), axes = TRUE)
plot(gbm.US_h6d3_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 6 Months"), axes = TRUE)
plot(gbm.US_h12d4_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 12 Months"), axes = TRUE)

##Misc

#Recover
load("~/Google Drive/Independent Work/Code/save_pred.RData")
head(pred_final)

load("~/Google Drive/Independent Work/Code/save_pred.RData")



