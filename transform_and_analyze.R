"""
Fix Up Code Priority
-Fix GLM and make sure I'm getting same exact value as literature
-Why is GBM getting a value <0.5? I must be calculating it incorrectly
-Add in to data of US_ALL_TRUNCV which to keep as level, which to keep log, etc. Right now I need to manually change :(
-Moduralize the code for roc and roll...I'm printing out the graphs and calculating ROC curve same each time
-Get statistic for AUROC?
"""

#Set workdirectory
setwd("~/Google Drive/Independent Work")

#Preamble
library(ggplot2)
library(gbm)
library(xts)
library(stats)
library(Amelia)
library(quantmod)
library(pROC)
library(dyn)
library(sandwich) #
library(BMS)
library(mboost)
library(lubridate)
setwd("~/Google Drive/Independent Work/Code")
source("gw.test.R")

### Functions We'll Need ###

date_COUNTRY <- function(df.COUNTRY)
{
  df.COUNTRY$DATE = as.Date(df.COUNTRY$DATE, format="%m/%d/%Y")  
  return(df.COUNTRY)
}

zoo_COUNTRY <- function(df.COUNTRY)
{
  zoo.COUNTRY = read.zoo(df.COUNTRY)
  return(zoo.COUNTRY)
}


SA_COUNTRY <- function(zoo.COUNTRY, NSA_names)
{
  begin_month = as.numeric(format(start(zoo.COUNTRY),"%m"))
  begin_year = as.numeric(format(start(zoo.COUNTRY),"%Y"))
  end_month = as.numeric(format(end(zoo.COUNTRY),"%m"))
  end_year = as.numeric(format(end(zoo.COUNTRY),"%Y"))
  ts.COUNTRY_NSA = ts(zoo.COUNTRY[,NSA_names], frequency = 12, start=c(begin_year,begin_month))
  for(i in 1:length(NSA_names))
  {
    zoo.COUNTRY[,NSA_names[i]]=ts.COUNTRY_NSA[,NSA_names[i]] - decompose(ts.COUNTRY_NSA[,NSA_names[i]])$season
  }
  return(zoo.COUNTRY)
}

TRANSFORM_COUNTRY <- function(zoo.C, same, level_1D, log_1D,log_2D)
{
  #Same
  zoo.C_same = zoo.C[,same]
  
  #Level First Difference
  zoo.C_level_1D = diff(zoo.C[,level_1D], differences = 1)
  
  #Log First Difference
  zoo.C_log_1D = zoo(apply(zoo.C[,log_1D], 2, Delt),time(zoo.C))
  
  #Log Transform Second Difference
  zoo.C_log_2D = diff(log(zoo.C[,log_2D]), differences = 2)
  
  #Merge
  zoo.C_lag0 = merge(zoo.C_same, zoo.C_level_1D, zoo.C_log_1D, zoo.C_log_2D)
  
  #Return All Merged for Lag = 0
  return(zoo.C_lag0)
}

add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]



################### Read in Data and Clean ######################

### Read In Data for US ####


### US Transform Season Function ### 
transform_season_US <- function(df.US, rec = 'D')
{
  ####Convert to Zoo ###
  #strs.US <- readLines("~/Google Drive/Independent Work/Data/US/US_BERGE.csv")
  strs.US <- readLines("~/Google Drive/Independent Work/Data/US/US_ALL_TRUNC.csv")
  df.US <- read.csv(text=strs.US,             # read from an R object rather than a file
                    skip=9,                # skip the first 8
                    stringsAsFactors=FALSE
  )
  
  df.US_header <- read.csv(text=strs.US,             # read from an R object rather than a file
                           nrows=10,                # skip the first line
                           stringsAsFactors=FALSE,
                           row.names = 1
  )
  #Get US Recessionary binary 0 1

  if(rec == "D")
  {
    df.US$USRECE <- NULL
    df.US$USRECG <- NULL
  }
  #Get US Severity with lowest Employment
  else if(rec == "E")
  {
    df.US$USRECD <- NULL
    df.US$USRECG <- NULL
    df.US$USRECD <- df.US$USRECE
    df.USRECE <- NULL
    df.US$USRECD[abs(df.US$USRECD) > 0] <- as.integer(abs(df.US$USRECD[abs(df.US$USRECD) > 0])+1)    
  }
  #Get US Severity with lowest GDP
  else
  {
    #Delete two other columns
    df.US$USRECE <- NULL
    df.US$USRECD <- NULL
    #Set US Severity with lowest GDP to USRECD
    df.US$USRECD <- df.US$USRECG
    #Delete US Severity column
    df.USRECG <- NULL
    #Order Recessions, if gdp less than 0 set as 1
    df.US$USRECD[abs(df.US$USRECD) > 0] <- as.integer(abs(df.US$USRECD[abs(df.US$USRECD) > 0])+1)    
  }
  df.US = date_COUNTRY(df.US)
  zoo.US = zoo_COUNTRY(df.US)
  
  ### Fill in NA ####
  zoo.US = na.approx(zoo.US)
  #Last Observation Carry Forward
  zoo.US = na.locf(na.locf(na.approx(zoo.US)), fromLast = TRUE)
  ### Add Constant to "FMRNBA" so not negative log difference ###
  zoo.US$FMRNBA = zoo.US$FMRNBA  + 4000 
  #View(df.US)
  str(df.US)
  
  ## Seasonally Adjust ### 
  NSA = c("PERMITNSA","HSBNE","HSBMW","HSBSOU","HSBWST", "A0M070", "FYAAAC", "FYBAAC")
  ts.US_NSA = ts(df.US[,NSA], frequency = 12, start=c(1959,2))
  ts.PERMITNSA_SA = ts.US_NSA[,NSA[1]] - decompose(ts.US_NSA[,NSA[1]])$season
  for(i in 1:length(NSA))
  {
    zoo.US[,NSA[i]]=ts.US_NSA[,NSA[i]] - decompose(ts.US_NSA[,NSA[i]])$season
  }
  
  ### Transform ###
  #Keep Already Transformed
  zoo.US_fix = zoo.US[,c("USRECD","YPR")]
  
  #Keep As Level 
  levels = c("PMP","CES151","A0M001","PMEMP","PMI","PMNO","PMDEL","PMNV","FCLBMC","PMCP", "SCP90F",  "SFYGM3",  "SFYGM6",	"SFYGT1",	"SFYGT5",	"SFYGT10",	"SFYAAAC",	"SFYBAAC", "TERMSPREAD")
  zoo.US_levels = zoo.US[,levels]
  
  #Log
  log_stay = c("HSFR","HSNE","HSMW","HSSOU","HSWST","PERMITNSA","HSBNE","HSBMW","HSBSOU","HSBWST")
  zoo.US_log = log(zoo.US[,log_stay])
  
  #Level Transform First Difference
  level_1D = c("UTL11", "LHELX", "LHUR", "LHU680", "CES155","A0M077","HHSNTN", "CCIPY", "FYFF","CP90","FYGM3","FYGM6","FYGT1","FYGT5","FYGT10","FYAAAC","FYBAAC","FSDXP")
  zoo.US_1D = diff(zoo.US[,level_1D])
  
  #Log Transform First Difference
  #We include YPR in the fix
  log_1D = c("IPS10",  "IPS11",	"IPS12",	"IPS13",	"IPS18",	"IPS25",	"IPS32",	"IPS34",	"IPS38",	"IPS43",	"IPS299",	"IPS307",	"IPS306", "LHEM",  "LHNAG",	"LHU5",	"LHU14",	"LHU15",	"LHU26",	"LHU27",	"CLAIMUII",	"CES002",	"CES003",	"CES006",	"CES011",	"CES015",	"CES017", "CES033",  "CES046",	"CES048",	"CES049",	"CES053",	"CES088",	"CES140", "A0M048","A1M008", "A0M007",	"A0M027",	"A1M092","A0M070", "CONS.R",  "MTQ",	"A0M059","FM2.R", "EXRUS",  "EXRSW",	"EXRJAN",	"EXRUK",	"EXRCAN", "FSPCOM",  "FSPIN", "FSPXE")
  zoo.US_log_1D = diff(log(zoo.US[,log_1D]), differences = 1)
  
  #Log Transform Second Difference
  log_2D = c("CES275",  "CES277",	"CES278", "FM1",  "FM2",	"FMSCU",	"FMFBA",	"FMRRA",	"FMRNBA",	"FCLNBW",	"CCINRV", "PWFSA",  "PWFCSA",	"PWIMSA",	"PWCMSA",	"PSCCOM",	"PW102",	"PUNEW",	"PU83",	"PU84",	"PU85",	"PUC",	"PUCD",	"PUS",	"PUXF",	"PUXHS",	"PUXM",	"GMDC",	"GMDCD",	"GMDCN",	"GMDCS")
  #zoo.US[,log_2D]
  zoo.US_log_2D = diff(log(zoo.US[,log_2D]), differences = 2)
  
  #Combine transformed Series
  zoo.US_lag0 = merge(zoo.US_fix, zoo.US_levels, zoo.US_log, zoo.US_1D, zoo.US_log_1D, zoo.US_log_2D)
  zoo.US_all = zoo.US_lag0
  #Remove rows with NA in them
  zoo.US_lag0 = na.omit(zoo.US_lag0)
  
  #Need to find what 2 series are missing
  ncol(zoo.US_lag0) #131 columns, minus USRECD, only 130 monthly series
  nrow(zoo.US_lag0) #666 months
  ncol(df.US) #132 columns including a date colum and recession, so 130 monthly series
  nrow(df.US) #668 rows (we lose 2 months from second difference)
  
  ### Plot ###
  plot(zoo.US_lag0$USRECD, xlab = "Year", ylab = "Recession", col = "Red", main = "US Recessions 1959-2014")
  autoplot.zoo(zoo.US_lag0$USRECD, xlab = "Year", ylab = "Recession", col = "Red", main = "US Recessions 1959-2014")
  
  return(zoo.US_lag0)
}

## Transform Japan Function ##
transform_season_JP <- function()
{
  
  #### Read in Data for Japan ####
  strs.JP <- readLines("~/Google Drive/Independent Work/Data/Japan/JAPAN_ALL_TRUNC.csv")
  df.JP <- read.csv(text=strs.JP,             # read from an R object rather than a file
                    skip=9,                # skip the first line
                    stringsAsFactors=FALSE
  )
    
  df.JP_header <- read.csv(text=strs.JP,             # read from an R object rather than a file
                           nrows=10,                # skip the first line
                           stringsAsFactors=FALSE,
                           row.names = 1
  )

  ###Convert to Zoo###
  df.JP = date_COUNTRY(df.JP)
  zoo.JP = zoo_COUNTRY(df.JP)
  
  zoo.JP = window(zoo.JP, start = "1975-01-01", end = end(zoo.JP))
  #missmap(zoo.JP, main="Japan Data - Missings Map",  col=c("yellow", "black"), legend=TRUE)
  
  zoo.JP = window(zoo.JP, start = "1978-01-01", end = end(zoo.JP))
  #missmap(zoo.JP, main="Japan Data - Missings Map",  col=c("yellow", "black"), legend=TRUE)
  
  #Approximate couple missing values
  zoo.JP = na.approx(zoo.JP)
  #Impute values using the mean
  zoo.JP = na.aggregate(zoo.JP)
  #missmap(zoo.JP, main="Japan Data - Missings Map",  col=c("yellow", "black"), legend=TRUE)
  
  ### Seasonally Adjust ###
  JP_NSA = names(df.JP_header['NSA',df.JP_header['NSA',] == 1])
  zoo.JP = SA_COUNTRY(zoo.JP, JP_NSA)
    
  sort(df.JP_header['DONE_TRANS',])
  
  JP_var_names <- function(name)
  {
    return(names(df.JP_header['NEED_TRANS',df.JP_header['NEED_TRANS',] == name]))
  }
  
  same_JP = JP_var_names(name = 0)
  level_1D_JP = JP_var_names(name = "LEVEL_1D")
  log_0D_JP = c("JPNQH0037")
  log_1D_JP  = JP_var_names(name = "LOG_1D")
  log_2D_JP = JP_var_names(name = "LOG_2D")
  
  log_transform <-function(zoo.C, log_0D)
  {
    return(log(zoo.C[,log_0D]))
  }
  zoo.JP_lag0 = TRANSFORM_COUNTRY(zoo.JP, same_JP, level_1D_JP, log_1D_JP, log_2D_JP)
  zoo.JP_lag0 = na.omit(zoo.JP_lag0)
  
  #missmap(zoo.JP_lag0, main="Japan Data - Missings Map",  col=c("yellow", "black"), legend=TRUE)
  
  
  #Compare Original and Transformed
  ncol(zoo.JP_lag0) #Have duplicates, also check for NaN
  nrow(zoo.JP_lag0) 
  ncol(zoo.JP) 
  nrow(zoo.JP) 
  
  ### Summary of Data ###
  
  #Japan
  autoplot.zoo(zoo.JP_lag0$JAPRECD, xlab = "Year", ylab = "Recession", col = "Red", main = "US Recessions 1978-2014")
  #Look at Interest Rate Spread versus recession
  autoplot.zoo(zoo.JP_lag0$INTSPREAD)
  
  return(zoo.JP_lag0)
}

######## Apply Gradient Boosting ########

#Use GBM to forecast h months with d lags
gbm.forecast_lag <- function(forecast, lags, zoo.C_lag0, country, distr = "bernoulli", train = 0.5, m = 400, iter.cv = TRUE)
{
  h = forecast
  d = lags
  c = country
  if(d == 0)
  {
    horizon = h
  }
  else
  {
    horizon = seq(from =h+1, to = h+d)
  }
  #Lag h+1,h+2,...,h+d  
  zoo.C_lagRESULT = (na.omit(merge(lag(zoo.C_lag0[,2:ncol(zoo.C_lag0)], k = -horizon))))
  #Need to get Recession Information because not included in Lags
  REC_lagRESULT = window(zoo.C_lag0[,1], start = start(zoo.C_lagRESULT), end = end(zoo.C_lagRESULT))

  #Create GBM Model using ALL data with 50% as train
  gbm.C = gbm(REC_lagRESULT ~ . ,
                      data = zoo.C_lagRESULT,
                      distribution = distr,
                      shrinkage = 0.01, 
                      bag.fraction = 0.5, 
                      train.fraction = train, 
                      cv.folds = 10, 
                      n.trees = m)
  
  #best.iter_test = gbm.perf(gbm.C, method="test"))
  if (iter.cv == TRUE)
  {
  best.iter_cv = gbm.perf(gbm.C, method="cv")
  }
  else
  {
    best.iter_cv = m
  }
  
  print(best.iter_cv)
  #summary(gbm.C,n.trees=best.iter_test)
  print(summary(gbm.C,n.trees=best.iter_cv))

  #head(summary(gbm.US_h12d4,n.trees=best.iter_h12d4_cv), n = 15)
  
  #Predict in Sample
  pred = predict(gbm.C,zoo.C_lagRESULT, 
                 n.trees= best.iter_cv, 
                 type="response")
  
  #Plot Prediction Against Actual Recession
  begin_month = as.numeric(format(start(REC_lagRESULT),"%m"))
  begin_year = as.numeric(format(start(REC_lagRESULT),"%Y"))
  end_month = as.numeric(format(end(REC_lagRESULT),"%m"))
  end_year = as.numeric(format(end(REC_lagRESULT),"%Y"))
  ts.REC = ts(REC_lagRESULT, start = c(begin_year, begin_month), end=c(end_year,end_month), frequency = 12)
  ts.pred = ts(pred, start = c(begin_year,begin_month), end=c(end_year,end_month), frequency = 12)
  plot(ts.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
  par(new=TRUE)
  
  #Use GG Plot here and include what is h and d
  plot(ts.pred, col = "red", ylab = "Prob. of Recession", main = paste(c, ": Forecast",h,"Months"), axes = TRUE)
  
  #Calculate ROC score
  auc_gbm = roc(ts.REC,ts.pred)[9]
  #png(filename="~/Google Drive/Independent Work/Writing/Graphs/USH3D3_V2.png")
  #dev.off()
  #return(list(roc(ts.REC,ts.pred)[9], ts.pred, summary(gbm.C)[,1], summary(gbm.C)[,2]))
  return(list(summary(gbm.C),best.iter_cv,auc_gbm,ts.pred))
}

#Rolling Estimate of GBM
"""
1. Get rid of subscript out of bounds error
2. figure out why h3,h6,12 aren't able to get roc score...something is wrong
4. include lags?
5. 
"""
gbm.roc_roll <- function(forecast = 0, lags = 3, zoo.C_lag0, country, distr = "bernoulli", train = 1.0, run.full = TRUE, runs = 0, m = 400, wind = 180, steps = 0.01)
{
  h = forecast
  d = lags
  c = country
  
  if(d == 0)
  {
    horizon = h
  }
  else
  {
    horizon = seq(from =h+1, to = h+d)
  }

  #Lag h+1,h+2,...,h+d  
  zoo.C_lagRESULT = (na.omit(merge(lag(zoo.C_lag0[,2:ncol(zoo.C_lag0)], k = -horizon))))
  #Need to get Recession Information because not included in Lags
  REC_lagRESULT = window(zoo.C_lag0[,1], start = start(zoo.C_lagRESULT), end = end(zoo.C_lagRESULT))
  #Moving Window Size
  window = wind
  
  #Setting Number of run (Not to be confused with M trees)
  if(run.full == TRUE){
      run = (nrow(zoo.C_lagRESULT)-window-h - 1)
    }
  else{
    run = runs
  }
  
  #Create prediction vector
  pred_final = vector("numeric")
  #cv_score = vector("integer")
  
  #Create store for average and frequency count of variables selected
  df.store = data.frame()
  
  #Create positive prediction 
  pos_var = vector("integer")
  pos_uniq = vector("integer")
  
  #Time
  ptm <- proc.time()

  #Big for loop that will iterate about 400 times and predict out of sample and increment by 1
  for(i in 1:run)
  {
    #Get zoo from 1 to 180, then 2 to 182, then 3 to 183 all the way to run + window so like 10 to 190
    shift  <- sum(i,window)
    #I never use forward
    #forward <- sum(-1,-h)
    zoo.C_shift =  zoo.C_lagRESULT[i:shift,]
    REC_shift = REC_lagRESULT[i:shift,]

#     gbm.C = gbm(REC_shift ~ . ,
#                 data = zoo.C_shift,
#                 distribution = distr,
#                 shrinkage = steps, 
#                 bag.fraction = 1,
#                 cv.folds = 5,
#                 train.fraction = 1.0,
#                 n.trees = m)
    
    
    gbm.C = gbm.fit(x = zoo.C_shift,
                y = REC_shift,
                distribution = distr,
                shrinkage = steps, 
                bag.fraction = 1,
                n.trees = m,
                verbose = FALSE)
    
    
    #Get the summary of GBM model
    sum_gbm.C = summary(gbm.C, plotit= FALSE)

    #Print best iteration and store
    #best.iter_cv = gbm.perf(gbm.C, method="cv")
    #cv_score[i] = best.iter_cv
    cv_score = m
    #Forecast using LAST time to forecast NEXT h period
    next_predict = sum(shift,h,1)
    pred_final[i] =  predict(gbm.C,
                            zoo.C_lagRESULT[next_predict,], 
                            n.trees= m, 
                            type="response")  
    #pred_final[i] = 0

    #Store number of positive variables
    pos_var[i] = sum(as.numeric(sum_gbm.C[2] >0))
    
    #Update average and frequency    
    if(length(df.store) == 0)
    {
    df.store = data.frame(NAME = sum_gbm.C[order(sum_gbm.C[[1]]),1], AVG = 0, FREQ = 0)
    rownames(df.store) = df.store$NAME
    }
    if(length(df.store) > 0)
    {
    order_var = order(sum_gbm.C[[1]])
    #Add I_j^2 value
    df.store[,2] <- df.store[,2] + sum_gbm.C[order_var,2]
    #Add to get frequency of each I_j^2
    df.store[,3] <- df.store[,3] + as.numeric(sum_gbm.C[order_var,2]>0)
    }
    
    if(i %% 10 == 0)
    {
      cat(i)
      setwd("~/Google Drive/Independent Work/Code")
      save(pred_final, file = "save_pred.RData")
      #save(pred_final, file = paste("gbm_",c,"_h",h,"d",d,"_pred_",run,"_.RData",sep=""))
    }
  }
  #Print how long it took for ALL the run
  time_spent = proc.time() - ptm
  
  #Take average of I_j^2 value
  df.store[,2] = df.store[,2]/run
  df.store[,3] = df.store[,3]/run

  #Return the average score from highest to lowest
  df.store = df.store[order(df.store[,2], decreasing = TRUE),]
  
  #Plot from first iteration to last iteration
  begin_window = sum(1,window,h,1)
  end_window = sum(begin_window, run, -1) #In case where run = nrow of zoo.C_lagresult, end window should just be nrows
  begin_month = as.numeric(format(time(zoo.C_lagRESULT[begin_window,]),"%m"))
  begin_year = as.numeric(format(time(zoo.C_lagRESULT[begin_window,]),"%Y"))
  end_month = as.numeric(format(time(zoo.C_lagRESULT[end_window,]),"%m"))
  end_year = as.numeric(format(time(zoo.C_lagRESULT[end_window,]),"%Y"))
  ts.REC = ts(REC_lagRESULT[begin_window:end_window], start = c(begin_year, begin_month), end=c(end_year,end_month), frequency = 12)
  ts.pred = ts(pred_final, start = c(begin_year,begin_month), end=c(end_year,end_month), frequency = 12)
  ts.pos = ts(pos_var, start = c(begin_year,begin_month), end=c(end_year,end_month), frequency = 12)

  #Plot Prediction Against ACTUAL Recession
  plot(ts.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
  par(new=TRUE)
  plot(ts.pred, col = "red", ylab = "Prob. of Recession", main = paste(country, ":", m, "Boosting Roll Forecast",forecast,"Months"), axes = TRUE, ylim=c(0,1))
  setwd("~/Google Drive/Independent Work/Writing/Graphs")
  dev.copy(png, paste(c,"_boost_","h",h,"d",d,"_outsample_",run,"_.png", sep = ""))
  dev.off()
  
  #Return Prediction, Final Score, CV,Score and Ideally ROC
  return(list(ts.REC,
              ts.pred,
              roc(ts.REC,ts.pred),
              df.store,
              ts.pos,
              cv_score,
              time_spent))
}



######   Logit Models  #######



glm.predict_roc <- function(zoo.C_lag0, forecast, country)
{
  h = forecast
  if(country == "US")
  {
    glm.C_h = dyn$glm(USRECD ~ lag(JPNVT0060, sum(-1,-h)), data = zoo.C_lag0, family = "binomial")
    pred.glm.C_h = predict(glm.C_h, data.frame = zoo.C_lag0, type = "response")
    RECD = window(zoo.C_lag0$USRECD, start = start(pred.glm.C_h), end = end(pred.glm.C_h))
    return(roc(RECD, pred.glm.C_h))
  }
  else if(country == "JP")
  {
    glm.C_h = dyn$glm(JAPRECD ~ lag(JPNTI0015, sum(-1,-h)), data = zoo.C_lag0, family = "binomial")
    pred.glm.C_h = predict(glm.C_h, data.frame = zoo.C_lag0, type = "response")
    RECD = window(zoo.C_lag0$JAPRECD, start = start(pred.glm.C_h), end = end(pred.glm.C_h))
    return(roc(RECD, pred.glm.C_h))
  }
}


##Rolling Forecast Logit
glm.roc_roll <- function(zoo.C_lag0, varname = "TERMSPREAD", forecast = 0, country, wind = 317)
{
  h = forecast
  c = country
  window = wind #317 will go from 1959-04-01 to 1984-09-01
  #run = (nrow(zoo.C)-window-h)
  run = (nrow(zoo.C_lag0)-window-h - 1)
  pred_final = vector("numeric")
  #zoo.C_lag0 = lag(zoo.C_shift[,"PMP"], forward)
  
  for(i in 1:run)
  {
    #Get zoo from 1 to 180, then 2 to 182
    shift  <- sum(i,window)
    forward <- sum(-1,-h)
    zoo.C_shift = zoo.C_lag0[i:shift,]
    #REC_shift = REC_lagRESULT[i:shift,]
    if(country == "US")
    {
    glm.C = eval(substitute(
              dyn$glm(USRECD ~ lag(variable, forward), 
              data = zoo.C_shift, 
              family = "binomial"),
              list(variable = as.name(varname))))
    }
    else if(country == "JP")
    {
      glm.C = dyn$glm(JAPRECD ~ lag(OPTA, forward), data = zoo.C_shift, family = "binomial")
    }
    else
    {
      print("Uh oh, no country specified or incorrect spelling")
    }
    #Forecast using LAST time to forecast NEXT h period
    pred_final[i] =  predict.glm(glm.C,
                             newdata = zoo.C_lag0[shift,], 
                             type="response")
    
  }
  begin_window = sum(1,window,h,1) #Forecast out of sample h +1 months ahead
  end_window = sum(begin_window, run, -1) 
  begin_month = as.numeric(format(time(zoo.C_lag0[begin_window,]),"%m"))
  begin_year = as.numeric(format(time(zoo.C_lag0[begin_window,]),"%Y"))
  end_month = as.numeric(format(time(zoo.C_lag0[end_window,]),"%m"))
  end_year = as.numeric(format(time(zoo.C_lag0[end_window,]),"%Y"))
  if(country == "US")
  {
    ts.REC = ts(zoo.C_lag0$USRECD[begin_window:end_window], start = c(begin_year, begin_month), end=c(end_year,end_month), frequency = 12)
  }
  else if(country == "JP")
  {ts.REC = ts(zoo.C_lag0$JAPRECD[begin_window:end_window], start = c(begin_year, begin_month), end=c(end_year,end_month), frequency = 12)
  }
  ts.pred = ts(pred_final, start = c(begin_year,begin_month), end=c(end_year,end_month), frequency = 12)  
  
  plot(ts.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
  par(new=TRUE)
  plot(ts.pred, col = "red", ylab = "Prob. of Recession", main = paste(c, ": ", varname," GLM Roll Forecast",h,"Months"), axes = TRUE, ylim = c(0,1))
  
  return(roc(ts.REC, ts.pred))
}


glm.roll.termspread3 = glm.roc_roll(zoo.US_lag0, forecast = 3, country = "US", varname = "TERMSPREAD")
glm.out.termspread3 = glm.out(zoo.US_lag0, forecast = 3, country = "US", varname = "TERMSPREAD")

glm.roll.termspread17 = glm.roc_roll(zoo.US_lag0, forecast = 17, country = "US", varname = "TERMSPREAD")
glm.out.termspread17 = glm.out(zoo.US_lag0, forecast = 17, country = "US", varname = "TERMSPREAD")

glm.roll.pmp3 = glm.roc_roll(zoo.US_lag0, forecast = 3, country = "US", varname = "PMP")
glm.out.pmp3 = glm.out(zoo.US_lag0, forecast = 3, country = "US", varname = "PMP", end_train = "1985-08-01")

glm.out.OPTA0 = glm.out(zoo.JP_lag0, forecast = 0, country = "JP", varname = "OPTA", end_train = "1985-08-01")
glm.out.OPTA3 = glm.out(zoo.JP_lag0, forecast = 3, country = "JP", varname = "OPTA", end_train = "1985-08-01")
glm.out.OPTA6 = glm.out(zoo.JP_lag0, forecast = 6, country = "JP", varname = "OPTA", end_train = "1985-08-01")
glm.out.OPTA12 = glm.out(zoo.JP_lag0, forecast = 12, country = "JP", varname = "OPTA", end_train = "1985-08-01")
glm.out.JPN0015_12 = glm.out(zoo.JP_lag0, forecast = 12, country = "JP", varname = "JPNTI0015", end_train = "1985-08-01")
glm.out.JPNTK0011 = glm.out(zoo.JP_lag0, forecast = 12, country = "JP", varname = "JPNTK0011", end_train = "1985-08-01")

glm.out_all <
glm.out <- function(zoo.C_lag0, forecast = 0, country, varname = "PMP", end_train = "1985-09-01")
{
  forward <- sum(-1,-forecast)
  train_start = start(zoo.C_lag0)
  train_end = end_train
  test_start = as.Date(train_end) + months(1)
  test_end = end(zoo.C_lag0)
  
  zoo.C_train = window(zoo.C_lag0, start = train_start, end=train_end)
  zoo.C_test = window(zoo.C_lag0, start = test_start, end=test_end)  
  
  if(country == "US")
  {
    glm.C_os = eval(substitute(
      dyn$glm(USRECD ~ lag(variable,forward),
          data = zoo.C_train,
          family = binomial(link = "probit")),
      list(variable = as.name(varname)))
    )
    
    pred_os = predict(glm.C_os, 
                      newdata = zoo.C_test,
                      type="response")
    
    zoo.REC = window(zoo.C_lag0$USRECD, 
                     start = start(pred_os),
                     end=end(pred_os),
                     frequency = 12)
    
  }
  else if(country == "JP")
  {
    glm.C_os = eval(substitute(
      dyn$glm(JAPRECD ~ lag(variable,forward),
          data = zoo.C_train,
          family = binomial(link = "probit")),
      list(variable = as.name(varname)))
    )
    pred_os = predict(glm.C_os, 
                      newdata = zoo.C_test,
                      type="response")
    
    zoo.REC = window(zoo.C_lag0$JAPRECD, 
                     start = start(pred_os),
                     end=end(pred_os),
                     frequency = 12)
  }
  else
  {
    print("Uh oh, no country specified or incorrect spelling")
  }

  plot(zoo.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
  par(new=TRUE)
  plot(pred_os, ylab = "Prob. of Recession", main = paste(country, ":", varname,"GLM out Forecast",forecast,"Months"), axes = TRUE, ylim=c(0,1))
  return(roc(zoo.REC,pred_os))
}



#####---------- Japan -----------------#######

#Transform and Season Japan#
zoo.JP_lag0 = transform_season_JP()

##################In-Sample############################

#Logit
glm.JP_h0d0 = glm.predict_roc(zoo.JP_lag0, forecast = 0, country = "JP")
glm.JP_h3d3 = glm.predict_roc(zoo.JP_lag0, forecast = 3, country = "JP")

glm.JP_h6d3 = glm.predict_roc(zoo.JP_lag0, forecast = 6, country = "JP")
glm.JP_h12d4 = glm.predict_roc(zoo.JP_lag0, forecast = 12, country = "JP")

#Boosting
gbm.JP_h0d3 = gbm.forecast_lag(0,3,zoo.JP_lag0, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h3d3 = gbm.forecast_lag(3,3,zoo.JP_lag0, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h6d3 = gbm.forecast_lag(6,3,zoo.JP_lag0, "Japan", "bernoulli", train = 1.0) 
gbm.JP_h12d4 = gbm.forecast_lag(12,4,zoo.JP_lag0, "Japan", "bernoulli", train = 1.0, iter = 27) 

index = 2
print(list(gbm.JP_h0d3[index],gbm.JP_h3d3[index],gbm.JP_h6d3[index],gbm.JP_h12d4[index]))

###################Out-Sample##########################

#Logit
roc.glm.JP_h0_roll = glm.roc_roll(zoo.JP_lag0, forecast = 0, country = "JP")
roc.glm.JP_h3_roll = glm.roc_roll(zoo.JP_lag0, forecast = 3, country = "JP")
roc.glm.JP_h6_roll = glm.roc_roll(zoo.JP_lag0, forecast = 6, country = "JP")
roc.glm.JP_h12_roll = glm.roc_roll(zoo.JP_lag0, forecast = 12, country = "JP")

#Boost Mini
gbm.JP_h0d3_roll = gbm.roc_roll(forecast = 0, lags = 3, zoo.JP_lag0, run.full = FALSE, iter = 10)
gbm.JP_h3d3_roll = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0, run.full = FALSE, iter = 20)
gbm.JP_h6d3_roll = gbm.roc_roll(forecast = 6, lags = 3, zoo.JP_lag0, run.full = FALSE, iter = 80)
gbm.JP_h12d4_roll = gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0, run.full = FALSE, iter = 50)

#Boost Full
gbm.JP_h0d3_roll_full = gbm.roc_roll(forecast = 0, lags = 3, zoo.JP_lag0, run.full = TRUE, country = "JP", m = 400)
gbm.JP_h3d3_roll_full = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0, run.full = TRUE, country = "JP", m = 400)
gbm.JP_h6d3_roll_full = gbm.roc_roll(forecast = 6, lags = 3, zoo.JP_lag0, run.full = TRUE, country = "JP", m = 400)
gbm.JP_h12d4_roll_full = gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0, run.full = TRUE, country = "JP", m = 400)

gbm.JP_h0d3_roll_full2 = gbm.roc_roll(forecast = 0, lags = 3, zoo.JP_lag0, run.full = TRUE)
gbm.JP_h3d3_roll_full2 = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0, run.full = TRUE, m = 400)
gbm.JP_h6d3_roll_full2 = gbm.roc_roll(forecast = 6, lags = 3, zoo.JP_lag0, run.full = TRUE, m = 400)
gbm.JP_h12d4_roll_full2 = gbm.roc_roll(forecast = 12, lags = 4, zoo.JP_lag0, run.full = TRUE, m = 400)

gbm.JP_h3d3_roll_full361 = gbm.roc_roll(forecast = 3, lags = 3, zoo.JP_lag0, run.full = TRUE, m = 361)


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
plot(gbm.US_h3d3_roll_full2[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 3 Months"), axes = TRUE)
plot(gbm.US_h6d3_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 6 Months"), axes = TRUE)
plot(gbm.US_h12d4_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 12 Months"), axes = TRUE)




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
#Logit
roc.glm.US_h0_OS = glm.roc_roll(zoo.US_lag0, forecast = 0, country = "US")
roc.glm.US_h3_OS = glm.roc_roll(zoo.US_lag0, forecast = 3)
roc.glm.US_h6_OS = glm.roc_roll(zoo.US_lag0, forecast = 6)
roc.glm.US_h12_OS = glm.roc_roll(zoo.US_lag0, forecast = 12, country = "US", wind = 312)

#Boost Mini
gbm.US_h0d3_roll = gbm.roc_roll(forecast = 0, lags = 3, zoo.US_lag0, run.full = FALSE, iter = 10)
gbm.US_h3d3_roll = gbm.roc_roll(forecast = 3, lags = 3, zoo.US_lag0, run.full = FALSE, iter = 20)
gbm.US_h6d3_roll = gbm.roc_roll(forecast = 6, lags = 3, zoo.US_lag0, run.full = FALSE, iter = 80)
gbm.US_h12d4_roll = gbm.roc_roll(forecast = 12, lags = 4, zoo.US_lag0, run.full = FALSE, iter = 50)

#Boost Full
gbm.US_h0d3_roll_full = gbm.roc_roll(forecast = 0, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h3d3_roll_full = gbm.roc_roll(forecast = 3, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h6d3_roll_full = gbm.roc_roll(forecast = 6, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h12d4_roll_full = gbm.roc_roll(forecast = 12, lags = 4, zoo.US_lag0, run.full = TRUE, country = "US")
gbm.US_h12d4_roll_full_100 = gbm.roc_roll(forecast = 12, lags = 4, zoo.US_lag0, run.full = TRUE, country = "US", m = 100)
gbm.US_h3d3_roll_full2 = gbm.roc_roll(forecast = 3, lags = 3, zoo.US_lag0, run.full = TRUE, country = "US", m = 400)

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

#plot(gbm.US_h12d4_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 12 Months"), axes = TRUE)
plot(gbm.US_h3d3_roll_full2[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 3 Months"), axes = TRUE)
plot(gbm.US_h6d3_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 6 Months"), axes = TRUE)
plot(gbm.US_h12d4_roll_full[[5]], col = "black", ylab = "Number of Selected Variables", main = paste("US: Number of Positive Variables in Forecast 12 Months"), axes = TRUE)

##Misc

#Recover
load("~/Google Drive/Independent Work/Code/save_pred.RData")
head(pred_final)
