
#####---------- United States -----------------#######

#Transform and Season#
zoo.US_lag0 = transform_season_US()
zoo.US_lag0_big = zoo.US_lag0
zoo.US_lag0_berge = read_berge_US()


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
gbm.US_in_h12d4_big = gbm.forecast_lag(12,4,zoo.US_lag0_big, "US", "bernoulli", train = 1.0) 
gbm.US_in_h12d0_short = gbm.forecast_lag(12,0,zoo.US_lag0_CB, "US", "bernoulli", train = 1.0) 
#####

################## Out-Of-Sample ###################
#Logit Roll  
glm.US_h3_roll_best  = glm.roc_roll(zoo.US_lag0_big, forecast = 3, varname = "PMNO", country = "US")
glm.US_h6_roll_best  = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "PMNO", country = "US")
glm.US_h12_roll_best = glm.roc_roll(zoo.US_lag0_big, forecast = 12, varname = "SFYGT5", country = "US")
#
glm.US_h3_roll = glm.roc_roll(zoo.US_lag0_big, forecast = 3, varname = "SFYGM3", country = "US")
glm.US_h6_roll = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "EXRUK", country = "US")
glm.US_h6_roll_CRAP = glm.roc_roll_CRAP(zoo.US_lag0_big, forecast = 6, varname = "EXRUK", country = "US")

###Logit Out GRAPHED
glm.US_h3_out_best = glm.out(zoo.US_lag0_big, forecast = 3, country = "US", varname = "PMNO")
glm.US_h6_out_best = glm.out(zoo.US_lag0_big, forecast = 6, country = "US", varname = "PMNO")
glm.US_h12_out_best = glm.out(zoo.US_lag0_big, forecast = 12, country = "US", varname = "SFYGT5")


#Logit Roll
glm.US_h6_roll1 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "EXRUK", country = "US")
glm.US_h6_roll2 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "CES278", country = "US")
glm.US_h6_roll3 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "PUS", country = "US")
glm.US_h6_roll4 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "FMRRA", country = "US")
glm.US_h6_roll5 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "PUCD", country = "US")
glm.US_h6_roll6 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "GMDCD", country = "US")
glm.US_h6_roll7 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "LHU680", country = "US")
glm.US_h6_roll8 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "CES275", country = "US")
glm.US_h6_roll9 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "A0M077", country = "US")
glm.US_h6_roll10 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "FCLNBW", country = "US")
glm.US_h6_roll11 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "PMNO", country = "US")
glm.US_h6_roll12 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "IPS306", country = "US")
glm.US_h6_roll13 = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "CCINRV", country = "US")
glm.US_h12_roll = glm.roc_roll(zoo.US_lag0_big, forecast = 6, varname = "PUS", country = "US")

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

#save(glm.out_all_roll_USh3 , file = "~/Google Drive/Independent Work/Saved RData/glm.out_all_roll_USh3_4132015.RData")
#save(glm.out_all_roll_USh6 , file = "~/Google Drive/Independent Work/Saved RData/glm.out_all_roll_USh6_4132015.RData")
#save(glm.out_all_roll_USh12 , file = "~/Google Drive/Independent Work/Saved RData/glm.out_all_roll_USh12_4132015.RData")

glm.out_all_roll_USh3_fix = glm.out_roll_all(zoo.US_lag0, h =3, c = "US", model = 2, graph = TRUE, all_col = FALSE)
glm.out_all_roll_USh6_fix = glm.out_roll_all(zoo.US_lag0, h =6, c = "US", all_col = TRUE, model = 2, graph =FALSE)
glm.out_all_roll_USh12_fix = glm.out_roll_all(zoo.US_lag0, h =12, c = "US",all_col = TRUE, model = 2, graph = FALSE)

#save(glm.out_all_roll_USh6_fix, file = "~/Google Drive/Independent Work/Saved RData/glm.out_all_roll_USh6_fix_4142015.RData")

###Big Boost: 1985-08-01 GRAPHED
gbm.US_h3d3_roll_full = gbm.roc_roll(forecast = 3, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h6d3_roll_full = gbm.roc_roll(forecast = 6, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h12d4_roll_full = gbm.roc_roll(forecast = 12, lags = 4, zoo.US_lag0, run.full = TRUE, country = "US")
###
#save(glm.out_all_roll_USh6, file = "~/Google Drive/Independent Work/Saved RData/glm.out_all_roll_USh6_41215.RData")
#save(gbm.US_h3d3_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d3_roll_full_4112015.RData")
#save(gbm.US_h6d3_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d3_roll_full_4112015.RData")
#save(gbm.US_h12d4_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d4_roll_full_4112015.RData")
setwd("~/Google Drive/Independent Work/Writing/Graphs")

write.csv(gbm.US_h12d4_roll_full[[4]], file = "gbm.US_h2d4_roll_full.csv")

#Travis Boost 
gbm.US_h3d0_roll_travis = gbm.roc_roll(forecast = 3, lags = 0, zoo.US_lag0_berge, run.full = TRUE, country = "US", m = 1000)
gbm.US_h6d0_roll_travis = gbm.roc_roll(forecast = 6, lags = 0, zoo.US_lag0_berge, run.full = TRUE, country = "US", m = 1000)
gbm.US_h12d0_roll_travis = gbm.roc_roll(forecast = 12, lags = 0, zoo.C_lag0 = zoo.US_lag0_berge, run.full = TRUE, country = "US", max_m = 100)

mb.US_h0d0_roll_travis <- mboost.roc_roll(forecast = 0, lags = 0, zoo.C_lag0 = zoo.US_lag0_berge, m = 100, country = "US", manual_end = TRUE, input_end = "1985-05-01", CVM = FALSE)
mb.US_h6d0_roll_travis <- mboost.roc_roll(forecast = 6, lags = 0, zoo.C_lag0 = zoo.US_lag0_berge, m = 100, country = "US", manual_end = TRUE, input_end = "1985-05-01", CVM = FALSE)
mb.US_h12d0_roll_travis_2000 <- mboost.roc_roll(forecast = 12, lags = 0, zoo.C_lag0 = zoo.US_lag0_berge, m = 2000, country = "US", manual_end = TRUE, input_end = "1985-05-01", CVM = FALSE)
mb.US_h12d0_roll_travis_CV_100 <- mboost.roc_roll(forecast = 12, zoo.C_lag0 = zoo.US_lag0_berge, m = 100, country = "US",CVM = TRUE)
mb.US_h12d0_roll_travis_NOCV_100 <- mboost.roc_roll(forecast = 12, zoo.C_lag0 = zoo.US_lag0_berge, m = 100, country = "US",CVM = FALSE)
mb.US_h12d0_roll_travis_CV_200 <- mboost.roc_roll(forecast = 12, zoo.C_lag0 = zoo.US_lag0_berge, m = 2000, country = "US",CVM = TRUE)

#Expanding window seems to not overfit 2010 as much.

View(mb.US_h12d0_roll_travis[[2]])

mb.US_h18d0_roll_travis <- mboost.roc_roll(forecast = 18, lags = 0, zoo.C_lag0 = zoo.US_lag0_berge, m = 100, country = "US", manual_end = TRUE, input_end = "1985-05-01", CVM = FALSE)
mb.US_h24d0_roll_travis <- mboost.roc_roll(forecast = 24, lags = 0, zoo.C_lag0 = zoo.US_lag0_berge, m = 100, country = "US", manual_end = TRUE, input_end = "1985-05-01", CVM = FALSE)
mb.US_h12_in <- mboost_h12_US <- mboost.forecast_lag(forecast = 12, lags = 0, zoo.C_lag0 = zoo.US_lag0_berge, country = "US")

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
glm.out(zoo.US_lag0_liu, forecast = 23, country = "US", varname = "TERMSPREAD")


#Sanity Check: GLMOUT score ~ Liu and GLM roll makes sense!
glm.out_h18_termspread = glm.out(zoo.US_lag0_liu, forecast = 17, country = "US", varname = "TERMSPREAD")
glm.out_h18_termspread_roll = glm.roc_roll(zoo.US_lag0_liu, forecast = 17, country = "US", varname = "TERMSPREAD")

#Sanity check: Our results ~ Serena!
zoo.US_lag0_S = transform_season_US()
gbm.US_h3d3_roll_serena = gbm.roc_roll(forecast = 3, lags = 3, zoo.C_lag0 = zoo.US_lag0_S, run.full = TRUE, country = "US", input_end = "1977-02-01", manual_end = TRUE)
gbm.US_h6d3_roll_serena = gbm.roc_roll(forecast = 6, lags = 3, zoo.C_lag0 = zoo.US_lag0_S, run.full = TRUE, country = "US", input_end = "1977-02-01", manual_end = TRUE)
gbm.US_h12d4_roll_serena = gbm.roc_roll(forecast = 12, lags = 4, zoo.C_lag0 = zoo.US_lag0_S, run.full = TRUE, country = "US", input_end = "1977-02-01", manual_end = TRUE)


#Analysis

plot_zoo_REC(gbm.US_h3d3_roll_full[[5]], "positive variables", country = "US", TITLE="US: Positive Variables Selected by Boosting in Large Dataset for Horizon = 3 months")
plot_zoo_REC(gbm.US_h6d3_roll_full[[5]], "positive variables", country = "US", TITLE="US: Positive Variables Selected by Boosting in Large Dataset for Horizon = 6 months")
plot_zoo_REC(gbm.US_h12d4_roll_full[[5]], "positive variables", country = "US", TITLE="US: Positive Variables Selected by Boosting in Large Dataset for Horizon = 12 months")

##T-test for AUC##

#In-Sample
roc.test(glm.in_US_h3, gbm.US_in_h3d3_big[[3]], alternative = "less")
roc.test(glm.in_US_h6, gbm.US_in_h6d3_big[[3]], alternative = "less")
roc.test(glm.in_US_h12, gbm.US_in_h12d3_big[[3]], alternative = "less")

roc.test(glm.in_US_h3, gbm.US_in_h3d0_short[[3]], alternative = "less")
roc.test(glm.in_US_h6, gbm.US_in_h6d0_short[[3]], alternative = "less")
roc.test(glm.in_US_h12, gbm.US_in_h12d0_short[[3]], alternative = "less")

roc.test(gbm.US_in_h3d3_big[[3]], gbm.US_in_h3d0_short[[3]], alternative = "greater")
roc.test(gbm.US_in_h6d3_big[[3]], gbm.US_in_h6d0_short[[3]], alternative = "greater")
roc.test(gbm.US_in_h12d3_big[[3]], gbm.US_in_h12d0_short[[3]], alternative = "greater")



#Out-Of-Sample
roc.test(glm.US_h3_roll_best[[1]], gbm.US_h3d0_roll_CB[[3]], alternative = "greater")
roc.test(glm.US_h6_roll_best[[1]], gbm.US_h6d0_roll_CB[[3]], alternative = "greater")
roc.test(glm.US_h12_roll_best[[1]], gbm.US_h12d0_roll_CB[[3]], alternative = "greater")

roc.test(glm.US_h3_roll_best[[1]], gbm.US_h3d3_roll_full[[3]], alternative = "greater")
roc.test(glm.US_h6_roll_best[[1]], gbm.US_h6d3_roll_full[[3]], alternative = "greater")
roc.test(glm.US_h12_roll_best[[1]], gbm.US_h12d4_roll_full[[3]], alternative = "greater")

roc.test(gbm.US_h3d3_roll_full[[3]], gbm.US_h3d0_roll_CB[[3]], alternative = "less")
roc.test(gbm.US_h6d3_roll_full[[3]], gbm.US_h6d0_roll_CB[[3]], alternative = "less")
roc.test(gbm.US_h12d4_roll_full[[3]], gbm.US_h12d0_roll_CB[[3]], alternative = "less")





