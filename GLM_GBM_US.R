
#####---------- United States -----------------#######

#Transform and Season#
zoo.US_lag0 = transform_season_US()
zoo.US_lag0_big = zoo.US_lag0
#zoo.US_lag0_short = read_berge_US()


SHORT_US = c("USRECD", "A0M001", "CLAIMUII", "A1M008", "PMNO", "A0M027", "PERMITNSA", "FSPCOM", "CCINRV", "SFYGT10", "HHSNTN")
SHORT_US_GT5 = c("USRECD", "A0M001", "CLAIMUII", "A1M008", "PMNO", "A0M027", "PERMITNSA", "FSPCOM", "CCINRV", "SFYGT5", "HHSNTN")

zoo.US_lag0_CB = zoo.US_lag0[,SHORT_US]
zoo.US_lag0_CB5 = zoo.US_lag0[,SHORT_US_GT5]

############## In-Sample #####################
#####Logit #GRAPHED
glm.in_US_h3 = glm.roc_in(zoo.US_lag0, forecast = 3, country = "US", varname = "SFYGM3")
glm.in_US_h6 = glm.roc_in(zoo.US_lag0, forecast = 6, country = "US", varname = "SFYGT5")
glm.in_US_h12 = glm.roc_in(zoo.US_lag0, forecast = 12, country = "US", varname = "SFYGT10")
#####

glm.roc_in(zoo.JP_lag0_big, forecast = 12, country = "JP", varname = "JPNTK0595")

glm.US_in_all_h3 = glm.out_roll_all(zoo.US_lag0, h = 3, model = 0, c = "US")
glm.US_in_all_h6 = glm.out_roll_all(zoo.US_lag0, h = 6, model = 0, c = "US")
glm.US_in_all_h12 = glm.out_roll_all(zoo.US_lag0, h = 12, model = 0, c = "US")


####Boost GRAPHED
gbm.US_in_h3d3_big = gbm.forecast_lag(3,3,zoo.US_lag0_big, "US", "bernoulli", train = 1.0) 
gbm.US_in_h3d0_short = gbm.forecast_lag(3,0,zoo.US_lag0_CB, "US", "bernoulli", train = 1.0) 
gbm.US_in_h6d3_big = gbm.forecast_lag(6,3,zoo.US_lag0_big, "US", "bernoulli", train = 1.0) 
gbm.US_in_h6d0_short = gbm.forecast_lag(6,0,zoo.US_lag0_CB, "US", "bernoulli", train = 1.0) 
gbm.US_in_h12d3_big = gbm.forecast_lag(12,3,zoo.US_lag0_big, "US", "bernoulli", train = 1.0) 
gbm.US_in_h12d0_short = gbm.forecast_lag(12,0,zoo.US_lag0_CB, "US", "bernoulli", train = 1.0) 
#####

################## Out-Of-Sample ###################
######Logit Roll  GRAPHED
glm.US_h3_roll_best  = glm.roc_roll(zoo.US_lag0_big, forecast = 3, varname = "PMNO", country = "US")
glm.US_h6_roll_best  = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "FCLNBW", country = "US")
glm.US_h12_roll_best = glm.roc_roll(zoo.US_lag0_big, forecast = 12, varname = "SFYGT5", country = "US")
#######
glm.US_h3_roll = glm.roc_roll(zoo.US_lag0_big, forecast = 3, varname = "SFYGM3", country = "US")
glm.US_h6_roll = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "EXRUK", country = "US")
glm.US_h6_roll_CRAP = glm.roc_roll_CRAP(zoo.US_lag0_big, forecast = 6, varname = "EXRUK", country = "US")

###Logit Out 
glm.US_h3_out_best = glm.out(zoo.US_lag0_big, forecast = 3, country = "US", varname = "PMNO")
glm.US_h6_out_best = glm.out(zoo.US_lag0_big, forecast = 6, country = "US", varname = "PMNO")
glm.US_h12_out_best = glm.out(zoo.US_lag0_big, forecast = 12, country = "US", varname = "SFYGT5")




#Logit Roll
glm.US_h12_roll = glm.roc_roll(zoo.US_lag0_big, forecast = 12, varname = "CES002", country = "US")

glm.US_h6_roll_PMNO = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "PMNO", country = "US")
glm.US_h6_roll_PMI = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "PMI", country = "US")
glm.US_h6_roll_PMP = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "PMP", country = "US")
glm.US_h6_roll_SFYGT5 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "SFYGT5", country = "US")

#Logit ALL Out-of_sample 
glm.out_all_US_h3 = glm.out_roll_all(zoo.US_lag0, h = 3, c = "US", graph_param = FALSE, all_col = TRUE)
glm.out_all_US_h6 = glm.out_roll_all(zoo.US_lag0, h = 6, c = "US", graph_param = FALSE, all_col = TRUE)
glm.out_all_US_h12 = glm.out_roll_all(zoo.US_lag0, h = 12, c = "US",  graph_param = FALSE, all_col = TRUE)

#Logit ALL Roll
glm.out_all_roll_USh3 = glm.out_roll_all(zoo.US_lag0, h =3, c = "US", all_col = TRUE, model = 2, graph = FALSE)
glm.out_all_roll_USh6 = glm.out_roll_all(zoo.US_lag0, h =6, c = "US", all_col = TRUE, model = 2, graph =FALSE)
glm.out_all_roll_USh12 = glm.out_roll_all(zoo.US_lag0, h =12, c = "US",all_col = TRUE, model = 2, graph = FALSE)

save(glm.out_all_roll_USh3 , file = "~/Google Drive/Independent Work/Saved RData/glm.out_all_roll_USh3_4132015.RData")
save(glm.out_all_roll_USh6 , file = "~/Google Drive/Independent Work/Saved RData/glm.out_all_roll_USh6_4132015.RData")
save(glm.out_all_roll_USh12 , file = "~/Google Drive/Independent Work/Saved RData/glm.out_all_roll_USh12_4132015.RData")


#Big Boost: 1985-08-01 GRAPHED
gbm.US_h3d3_roll_full = gbm.roc_roll(forecast = 3, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h6d3_roll_full = gbm.roc_roll(forecast = 6, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h12d4_roll_full = gbm.roc_roll(forecast = 12, lags = 4, zoo.US_lag0, run.full = TRUE, country = "US")

save(glm.out_all_roll_USh6, file = "~/Google Drive/Independent Work/Saved RData/glm.out_all_roll_USh6_41215.RData")


#Big Boost: Serena 
zoo.US_lag0_S = transform_season_US()
gbm.US_h3d3_roll_serena = gbm.roc_roll(forecast = 3, lags = 3, zoo.C_lag0 = zoo.US_lag0_S, run.full = TRUE, country = "US", input_end = "1977-02-01", manual_end = TRUE)
gbm.US_h6d3_roll_serena = gbm.roc_roll(forecast = 6, lags = 3, zoo.C_lag0 = zoo.US_lag0_S, run.full = TRUE, country = "US", input_end = "1977-02-01", manual_end = TRUE)
gbm.US_h12d4_roll_serena = gbm.roc_roll(forecast = 12, lags = 4, zoo.C_lag0 = zoo.US_lag0_S, run.full = TRUE, country = "US", input_end = "1977-02-01", manual_end = TRUE)

save(gbm.US_h3d3_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d3_roll_full_4112015.RData")
save(gbm.US_h6d3_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d3_roll_full_4112015.RData")
save(gbm.US_h12d4_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d4_roll_full_4112015.RData")

#Boost w/ No Lags
#gbm.US_h3d0_roll_full400 = gbm.roc_roll(forecast = 3, lags = 0, zoo.US_lag0, run.full = TRUE, country = "US", m = 400)
#gbm.US_h6d0_roll_full400 = gbm.roc_roll(forecast = 6, lags = 0, zoo.US_lag0, run.full = TRUE, country = "US", m = 400)
#gbm.US_h12d0_roll_full400 = gbm.roc_roll(forecast = 12, lags = 0, zoo.US_lag0, run.full = TRUE, country = "US", m = 400)

#Small Boost 
gbm.US_h3d0_roll_travis = gbm.roc_roll(forecast = 3, lags = 0, zoo.US_lag0_short, run.full = TRUE, country = "US", m = 1000)
gbm.US_h6d0_roll_travis = gbm.roc_roll(forecast = 6, lags = 0, zoo.US_lag0_short, run.full = TRUE, country = "US", m = 1000)
gbm.US_h12d0_roll_travis = gbm.roc_roll(forecast = 12, lags = 0, zoo.US_lag0_short, run.full = TRUE, country = "US", m = 1000)

#Small Boost GRAPHED using CB
gbm.US_h3d0_roll_CB = gbm.roc_roll(forecast = 3, lags = 0, zoo.US_lag0_CB, run.full = TRUE, country = "US", max_m = 400)
gbm.US_h6d0_roll_CB = gbm.roc_roll(forecast = 6, lags = 0, zoo.US_lag0_CB5, run.full = TRUE, country = "US", max_m = 400)
gbm.US_h12d0_roll_CB = gbm.roc_roll(forecast = 12, lags = 0, zoo.US_lag0_CB, run.full = TRUE, country = "US", max_m = 400)



gbm.US_h6d3_roll_CB = gbm.roc_roll(forecast = 6, lags = 3, zoo.US_lag0_CB5, run.full = TRUE, country = "US", max_m = 400)
gbm.US_h12d4_roll_CB = gbm.roc_roll(forecast = 12, lags = 4, zoo.US_lag0_CB, run.full = TRUE, country = "US", max_m = 400)



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
load("~/Google Drive/Independent Work/Saved RData/save_pred_recent_gbm_roll.RData")
head(pred_final)

load("~/Google Drive/Independent Work/Code/save_pred.RData")


#Reproduce Liu and Moench Results
glm.out.termspread3 = glm.out(zoo.US_lag0_liu, forecast = 3, country = "US", varname = "TERMSPREAD")
zoo.US_lag0_liu = window(zoo.US_lag0, start = "1959-01-01", end = "2011-12-01")

glm.out(zoo.US_lag0_liu, forecast = 2, country = "US", varname = "TERMSPREAD")
glm.out(zoo.US_lag0_liu, forecast = 5, country = "US", varname = "TERMSPREAD")
glm.out(zoo.US_lag0_liu, forecast = 11, country = "US", varname = "TERMSPREAD")
glm.out(zoo.US_lag0_liu, forecast = 17, country = "US", varname = "TERMSPREAD")
glm.out(zoo.US_lag0_liu, forecast = 23, country = "US", varname = "TERMSPREAD")
