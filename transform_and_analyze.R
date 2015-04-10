"""
Fix Up Code Priority
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
library(pracma)
#library(tis)
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

DEMEAN_COUNTRY <- function(zoo.C, DEMEAN_names)
{
  for(i in 1:(length(DEMEAN_names)))
  {
    raw_pre1992 = window(zoo.C[,DEMEAN_names[i]], start = "1963-01-01", end = "1991-12-01")
    raw_pre1992_demean = raw_pre1992 - mean(raw_pre1992)
    raw_post1992 = window(zoo.C[,DEMEAN_names[i]], start = "1992-01-01")
    raw_post1992_demean = raw_post1992 - mean(raw_post1992)
    zoo.C[,DEMEAN_names[i]] = rbind(raw_pre1992_demean, raw_post1992_demean)
    #plot(rbind(raw_pre1992,raw_post1992), main = paste("RAW Graph of",DEMEAN_names[i]))
    #plot(rbind(raw_pre1992_demean, raw_post1992_demean),main = paste("DEMEAN Graph of",DEMEAN_names[i]))
  }
    return(zoo.C)
}

DETREND_COUNTRY <- function(zoo.C, DETREND_names, break_point = 193)
{
  for(i in 1:(length(DETREND_names)))
  {
    zoo.C[,DETREND_names[i]] = zoo(detrend(as.numeric(zoo.C[,DETREND_names[i]]),bp=break_point))
  }
  return(zoo.C)
}

add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


plot_zoo_REC <- function(zoo.C, varname, country)
{
  if(country == "US")
  { 
    getSymbols("USRECD",src="FRED")
    RECD = USRECD
  }
  else if(country == "JP")
  {
    getSymbols("JPNRECD",src="FRED")  
    RECD = JPNRECD
  }
  else
  {
    stop("No valid country name provided!")
  }
  varname.df <- data.frame(date= index(zoo.C[,varname]), value = as.vector(zoo.C[,varname]))
  start <- index(RECD[which(diff(RECD)==1)])
  end   <- index(RECD[which(diff(RECD)==-1)-1])
  end <- c(end, as.Date("2014-12-01"))
  reccesion.df <- data.frame(start=start, end=end)
  recession.df <- subset(reccesion.df, start >= min(varname.df$date))
  varname.df <- subset(varname.df, date >= start[1])
  g <- ggplot(varname.df)+geom_line(data=varname.df, aes(x=date,y=value)) + theme_bw() + geom_rect(data=recession.df, aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf), fill="red", alpha=0.5)+xlab("Time")+ylab(paste(varname))+ggtitle(paste(country,":", varname, "and recession"))
  return(g)
}


plot_transforms <- function(zoo.C, varname, country)
{
  autoplot(zoo.C[,varname])
  autoplot(DEMEAN_COUNTRY(zoo.C, varname)[,varname])
  autoplot(DETREND_COUNTRY(zoo.C, varname)[,varname])
  autoplot(diff(DETREND_COUNTRY(zoo.C, varname)[,varname]))
  autoplot(diff(DEMEAN_COUNTRY(zoo.C, varname)[,varname]))
  autoplot(Delt(DEMEAN_COUNTRY(zoo.C, varname)[,varname]))
  autoplot(Delt(zoo.C[,varname]))
}



################### Read in Data and Clean ######################

### Read In Data for US ####


### US Transform Season Function ### 
transform_season_US <- function(rec = 'D')
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

#Tankan
approx_tankan_JP <- function(FULL = FALSE)
{
  if(FULL == TRUE)
  {
    strs.tankan <- readLines("~/Google Drive/Independent Work/Data/Japan/TANKAN_FULL.csv")
  }
  else if(FULL == FALSE)
  {
    strs.tankan <- readLines("~/Google Drive/Independent Work/Data/Japan/TANKAN_SHORT.csv")
    
  }
  else
  {
    stop("uhhh didn't specify if you want the full TANKAN")
  }
  df.JP_TN <- read.csv(text=strs.tankan,             # read from an R object rather than a file
                       skip=1,                # skip the first line
                       stringsAsFactors=FALSE
  )
  
  df.JP_TN_header <- read.csv(text=strs.tankan,             # read from an R object rather than a file
                              nrows=2,                # skip the first line
                              stringsAsFactors=FALSE,
                              row.names = 1
  )
  
  zoo.JP_TN = zooreg(df.JP_TN, start = as.yearqtr("1974-2"), frequency = 4)
  time(zoo.JP_TN) = as.Date(time(zoo.JP_TN), format = "%y/0%q")
  month.JP_TN = seq(as.Date("1974-02-01"), as.Date("2015-01-01"), by = "month")
  zoo.NA_TN = zoo(NA, month.JP_TN)
  zoo.JP_TN_ALL = merge(zoo.JP_TN, zoo.NA_TN)
  time.JP_TN_ALL = time(zoo.JP_TN_ALL)
  zoo.JP_TN_ALL = zoo.JP_TN_ALL[,2:sum(ncol(zoo.JP_TN_ALL),-1)]
  num.JP_TN_ALL = apply(zoo.JP_TN_ALL, 2, as.numeric)
  out.JP_TN_DONE = apply(num.JP_TN_ALL, 2, na.approx)
  zoo.JP_TN_DONE = zoo(out.JP_TN_DONE, month.JP_TN)
  
  return(zoo.JP_TN_DONE)
}

## Transform Japan Function ##
transform_season_JP <- function(include_TN = TRUE, TN_SHORT = TRUE)
{
  
  #### Read in Data for Japan ####
  strs.JP <- readLines("~/Google Drive/Independent Work/Data/Japan/JAPAN_ALL_TRUNC.csv")
  df.JP <- read.csv(text=strs.JP,             # read from an R object rather than a file
                    skip=10,                # skip the first line
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
  
  #zoo.JP = window(zoo.JP, start = "1978-01-01", end = end(zoo.JP))
  #missmap(zoo.JP, main="Japan Data - Missings Map",  col=c("yellow", "black"), legend=TRUE)
  
  #Approximate couple missing values
  #Impute values using the mean
  zoo.JP = na.approx(zoo.JP)
  zoo.JP = na.aggregate(zoo.JP)
  
  ##Demean Trend
  JP_DEMEAN = names(df.JP_header['DEMEAN',df.JP_header['DEMEAN',] == 1])
  JP_DETREND = names(df.JP_header['DEMEAN',df.JP_header['DEMEAN',] == 2])
  #zoo.JP_predemean = zoo.JP
  zoo.JP = DEMEAN_COUNTRY(zoo.JP, JP_DEMEAN)
  zoo.JP = DETREND_COUNTRY(zoo.JP, JP_DETREND)
  
  ### Seasonally Adjust ###
  JP_NSA = names(df.JP_header['NSA',df.JP_header['NSA',] == 1])
  zoo.JP = SA_COUNTRY(zoo.JP, JP_NSA)
    
  sort(df.JP_header['DONE_TRANS',])
  
  JP_var_names <- function(name)
  {
    return(names(df.JP_header['NEED_TRANS',df.JP_header['NEED_TRANS',] == name]))
  }
  
  same_JP = JP_var_names(name = "LEVEL")
  level_1D_JP = JP_var_names(name = "LEVEL_1D")
  log_1D_JP  = JP_var_names(name = "LOG_1D")
  log_2D_JP = JP_var_names(name = "LOG_2D")
  
  log_transform <-function(zoo.C, log_0D)
  {
    return(log(zoo.C[,log_0D]))
  }
  zoo.JP_lag0 = TRANSFORM_COUNTRY(zoo.JP, same_JP, level_1D_JP, log_1D_JP, log_2D_JP)
  
  if(ncol(zoo.JP_lag0) == ncol(zoo.JP))
  {
    print("same number of columns before and after transform. Yay!")
  }
  else
  {
    stop("Uh, we don't have same number of columns before and after transform. Double check to make sure non-transformed in dataset has LEVEL and NOT 0")
  }
  
  #Merge Tankan Dataset
  
  if(include_TN == TRUE)
  {
    if(TN_SHORT == TRUE)
      {
      zoo.JP_TAN = approx_tankan_JP(FULL = FALSE)
      }
    else
    {
      zoo.JP_TAN = approx_tankan_JP(FULL = TRUE)
    }
  zoo.JP_TAN = window(zoo.JP_TAN, start = start(zoo.JP_lag0), end = end(zoo.JP_lag0))
  zoo.JP_lag0 = merge(zoo.JP_lag0, zoo.JP_TAN)
  }
  
  #Remove NA from original data. Should only remove row 1 because of log_1D  
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
gbm.roc_roll <- function(forecast = 0, lags = 3, zoo.C_lag0,  country, distr = "bernoulli", train = 1.0, run.full = TRUE, runs = 10,  max_m= 400, wind = 317, steps = 0.01, end_train = "1985-08-01")
{
  h = forecast
  d = lags
  c = country
  
  if(d == 0)
  {
    horizon = h + 1
  }
  else
  {
    horizon = seq(from =h+1, to = h+d)
  }

  #Lag h+1,h+2,...,h+d  
  zoo.C_lagRESULT = (na.omit(merge(lag(zoo.C_lag0[,2:ncol(zoo.C_lag0)], k = -horizon))))
  #Need to get Recession Information because not included in Lags
  REC_lagRESULT = window(zoo.C_lag0[,1], start = start(zoo.C_lagRESULT), end = end(zoo.C_lagRESULT))
  
  train_start = start(zoo.C_lagRESULT)
  #train_end = end_train
  train_end = warning_train_end(country, input_end)
  test_start = as.Date(train_end) + months(h+1) 
  test_end = end(zoo.C_lagRESULT)
  
  #Setting Number of run (Not to be confused with M trees)
  if(run.full == TRUE){
      #run = (nrow(zoo.C_lagRESULT)-window-h - 1)
      run = elapsed_months(test_end, test_start)
    }
  else{
    run = runs
  }
  
  #Create prediction vector
  pred_final = vector("numeric")
  
  #Create store for average and frequency count of variables selected
  df.store = data.frame()
  
  #Create positive prediction 
  pos_var = vector("integer")
  pos_uniq = vector("integer")
  
  #Time
  ptm <- proc.time()

  #Big for loop that will iterate about 400 times and predict out of sample and increment by 1
  for(i in 1:sum(run,1))
  {
    #Get zoo from 1 to 180, then 2 to 182, then 3 to 183 all the way to run + window so like 10 to 190
    start_shift  <- months(i-1) + as.Date(train_start)
    end_shift <- months(i-1) + as.Date(train_end)
    
    zoo.C_shift =  window(zoo.C_lagRESULT, start = start_shift, end = end_shift)
    REC_shift = window(REC_lagRESULT, start = start_shift, end = end_shift)
    zoo.C_predict = window(zoo.C_lagRESULT, start = (as.Date(test_start)+months(i-1)), end = (as.Date(test_start)+months(i-1)))
    
    if(i == 1)
    {
      gbm.C = gbm(REC_shift ~ . ,
                  data =  zoo.C_shift,
                  distribution = distr,
                  shrinkage = steps, 
                  bag.fraction = 1.0, 
                  train.fraction = 1.0, 
                  cv.folds = 5, 
                  n.trees = max_m)
      
      m = gbm.perf(gbm.C, method="cv")
      print(m)
    }
    else
    {  
      gbm.C = gbm.fit(x = zoo.C_shift,
                y = REC_shift,
                distribution = distr,
                shrinkage = steps, 
                bag.fraction = 1,
                n.trees = m,
                verbose = FALSE)
    }
    
      
    #Get the summary of GBM model
    sum_gbm.C = summary(gbm.C, plotit= FALSE)

    #Print best iteration and store
    #best.iter_cv = gbm.perf(gbm.C, method="cv")
    #cv_score[i] = best.iter_cv
    cv_score = m
    #Forecast using LAST time to forecast NEXT h period
    pred_final[i] =  predict(gbm.C,
                            zoo.C_predict, 
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
      save(pred_final, file = "~/Google Drive/Independent Work/Saved RData/save_pred_recent_gbm_roll.RData")
      #save(pred_final, file = paste("gbm_",c,"_h",h,"d",d,"_pred_",run,"_.RData",sep=""))
    }
  }
  #Print how long it took for ALL the run
  time_spent = proc.time() - ptm
  
  #Take average of I_j^2 value
  df.store[,2] = df.store[,2]/(run+1)
  df.store[,3] = df.store[,3]/(run+1)

  #Return the average score from highest to lowest
  df.store = df.store[order(df.store[,2], decreasing = TRUE),]

  #Convert into time series object
  from <- as.Date(test_start)
  to <- as.Date(test_start) + months(run)
  months <- seq.Date(from=from,to=to,by="month")
  zoo.pred = zoo(pred_final, months)
  zoo.pos = zoo(pos_var, months)
  zoo.REC = window(REC_lagRESULT, 
                   start = start(zoo.pred),
                   end=end(zoo.pred),
                   frequency = 12)

  #Plot Prediction Against ACTUAL Recession
  plot(zoo.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
  par(new=TRUE)
  plot(zoo.pred, col = "red", ylab = "Prob. of Recession", 
       main = paste(c, ":", "Boosting Rolling Forecast",h,"Months with", m, "iterations"), 
       axes = TRUE, 
       ylim=c(0,1))
  #setwd("~/Google Drive/Independent Work/Writing/Graphs")
  #dev.copy(png, paste(c,"_boost_","h",h,"d",d,"_outsample_",run,"_.png", sep = ""))
  #dev.off()
  
  #Return Prediction, Final Score, CV,Score and Ideally ROC
  return(list(zoo.REC,
              zoo.pred,
              roc(zoo.REC,zoo.pred),
              df.store,
              zoo.pos,
              cv_score,
              time_spent
              )
         )
}

######   Logit Models  #######

glm.roc_in <- function(zoo.C_lag0, forecast, country, varname = "PMP")
{
  h = forecast
  if(country == "US")
  {
    glm.C_h = eval(substitute(dyn$glm(USRECD ~ lag(variable, sum(-h,-1)), 
              data = zoo.C_lag0, 
              family = "binomial"),list(variable = as.name(varname))))
    pred.glm.C_h = predict(glm.C_h, data.frame = zoo.C_lag0, type = "response")
    RECD = window(zoo.C_lag0$USRECD, start = start(pred.glm.C_h), end = end(pred.glm.C_h))
    return(roc(RECD, pred.glm.C_h))
  }
  else if(country == "JP")
  {
    glm.C_h = eval(substitute(dyn$glm(JAPRECD ~ lag(variable, sum(-h,-1)), 
                              data = zoo.C_lag0, 
                              family = "binomial"),list(variable = as.name(varname))))
    
    pred.glm.C_h = predict(glm.C_h, data.frame = zoo.C_lag0, type = "response")
    RECD = window(zoo.C_lag0$JAPRECD, start = start(pred.glm.C_h), end = end(pred.glm.C_h))
    return(roc(RECD, pred.glm.C_h))
  }
}

warning_train_end <- function(country, input_end)
{
  if(country == "JP")
  {
    if(!input_end == "1995-08-01")
    {
      print("JP train end date not 1995-08-01, automatically correcting")
    }
    train_end = "1995-08-01"
  }
  else if(country == "US")
  {
    if(!input_end == "1985-08-01")
    {
      print("US train end date not 1985-08-01,  automatically correcting")
    }
    train_end = "1985-08-01"
  }
  return(train_end)
}


##GLM Rolling Estimation
glm.roc_roll <- function(zoo.C_lag0, varname = "TERMSPREAD", forecast = 0, country, input_end = "1985-08-01", graph = TRUE)
{
  train_end = warning_train_end(country, input_end)
  h = forecast
  forward = sum(-h,-1)
  c = country
  train_start = start(zoo.C_lag0)
 
  test_start = as.Date(train_end) + months(h+1) 
  test_end = end(zoo.C_lag0)
    
  run = elapsed_months(test_end, test_start)
  pred_final = vector("numeric")
  
  for(i in 1:sum(run,1))
  {
    
    start_shift  <- months(i-1) + as.Date(train_start)
    end_shift <- months(i-1) + as.Date(train_end)
    
    zoo.C_shift =  window(zoo.C_lag0, start = start_shift, end = end_shift)
    zoo.C_predict = window(zoo.C_lag0, start = end_shift, end = end_shift)
    
    if(c == "US")
    {
    glm.C = eval(substitute(
              dyn$glm(USRECD ~ lag(variable, sum(-h,-1)), 
              data = zoo.C_shift, 
              family = "binomial"),
              list(variable = as.name(varname))))
    }
    else if(c == "JP")
    {      
      glm.C = eval(substitute(
        dyn$glm(JAPRECD ~ lag(variable, forward), 
                data = zoo.C_shift, 
                family = "binomial"),
        list(variable = as.name(varname))))
    }
    else
    {
      stop("Uh oh, no country specified or incorrect spelling")
    }
    #Forecast using LAST time to forecast NEXT h period
    pred_final[i] =  predict.glm(glm.C,
                             newdata = zoo.C_predict, 
                             type="response")
    
  }
  from <- as.Date(test_start)
  to <- as.Date(test_end)
  months <- seq.Date(from=from,to=to,by="month")
  zoo.pred = zoo(pred_final, months)
  zoo.REC = window(zoo.C_lag0[,1], 
                   start = start(zoo.pred),
                   end=end(zoo.pred),
                   frequency = 12)
  
  #Plot Prediction Against ACTUAL Recession
  plot(zoo.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
  par(new=TRUE)
  plot(zoo.pred, col = "red", ylab = "Prob. of Recession", 
       main = paste(c, ":", varname, "GLM Roll Forecast",h,"Months"), 
       axes = TRUE, 
       ylim=c(0,1))

  return(roc(zoo.REC, zoo.pred))
}
#GLM ALL Out-Of-Sample or ALL Roll
glm.out_roll_all <-function(zoo.C_lag0, h = 3, c, graph_param = FALSE, all_col = TRUE, model = 1)
{
  name_all = colnames(zoo.C_lag0)[2:ncol(zoo.C_lag0)]
  df.store_all = data.frame(NAME = name_all, ROC_SCORE = 0)
  if(all_col == TRUE)
  {
    total = length(name_all)
  }
  else
  {
    total = 3
  }
  
  ptm <- proc.time()
  
  #In-Sample ALL
  if(model == 0 )
  {
    for(i in 1:total)
    {
      glm.in_model = glm.roc_in(zoo.C_lag0, forecast = h, country = c, varname = name_all[i])
      #df.store_all[,name_all[i]] = 
      df.store_all[df.store_all$NAME == name_all[i],"ROC_SCORE"] = as.numeric(glm.in_model[9]) 
      if(i %% 10 == 0)
      {cat(i)}    
    }
  }
  #Out-Sample Standard
  else if(model == 1)
  {
    for(i in 1:total)
    {
      glm.out_model = glm.out(zoo.C_lag0, forecast = h, country = c, varname = name_all[i], end_train = end, graph = graph_param)
      #df.store_all[,name_all[i]] = 
      df.store_all[df.store_all$NAME == name_all[i],"ROC_SCORE"] = as.numeric(glm.out_model[9]) 
      if(i %% 10 == 0)
      {cat(i)}    
    }
  }
  #Out-Sample Rolling
  else if(model == 2)
  {
    for(i in 1:total)
    {
      glm.out_model = glm.roc_roll(zoo.C_lag0, forecast = h, country = c, varname = name_all[i], graph = graph_param)
      #df.store_all[,name_all[i]] = 
      df.store_all[df.store_all$NAME == name_all[i],"ROC_SCORE"] = as.numeric(glm.out_model[9]) 
      if(i %% 10 == 0)
      {cat(i)}    
    }
  }
  else
  {
    stop("umm model only goes 0,1,2")
  }
  time_spent = proc.time() - ptm
  
  print(time_spent)
  #df.store_all = df.store_all[order(df.store_all[,1], decreasing = TRUE),]
  df.store_all = df.store_all[order(df.store_all[,2]),]
  return(df.store_all)
}
  
#GLM Out-Of-Sample

glm.out <- function(zoo.C_lag0, forecast = 0, country, varname = "PMP", input_end = "1985-08-01", logit = "TRUE", graph = "TRUE")
{
  forward <- sum(-1,-forecast)
  train_start = start(zoo.C_lag0)
  train_end = warning_train_end(country, input_end)
  #train_end = end_train
  test_start = train_end
  test_end = end(zoo.C_lag0)
  
  zoo.C_train = window(zoo.C_lag0, start = train_start, end=train_end)
  zoo.C_test = window(zoo.C_lag0, start = test_start, end=test_end)  
  
  if(logit == "TRUE")
  {
    model = "logit"
  }
  else
  {
    model = "probit"
  }
  
  if(country == "US")
  {
    glm.C_os = eval(substitute(
      dyn$glm(USRECD ~ lag(variable,forward),
          data = zoo.C_train,
          family = binomial(link = model)),
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
    stop("Uh oh, no country specified or incorrect spelling")
  }
  if(graph == TRUE)
  {
    plot(zoo.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
    par(new=TRUE)
    plot(pred_os, ylab = "Prob. of Recession", main = paste(country, ":", varname,"GLM out Forecast",forecast,"Months"), axes = TRUE, ylim=c(0,1))
  }
  return(roc(zoo.REC,pred_os))
}

