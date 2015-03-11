"""
To Do
-toss out USTONCSERJ because its all zero...
-in us include michigan sentiment!
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


################### Read in Data and Clean ######################

### Read In Data for US ####


### US Transform Season Function ### 
transform_season_US <- function(df.US, rec = 'D')
{
  ####Convert to Zoo ###
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
    df.US$USRECD <- as.integer(abs(df.US$USRECD))
  
  }
  #Get US Severity with lowest GDP
  else
  {
    df.US$USRECE <- NULL
    df.US$USRECD <- NULL
    df.US$USRECD <- df.US$USRECG
    df.USRECG <- NULL
    df.US$USRECD <- as.integer(abs(df.US$USRECD))
    
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
  levels = c("PMP","CES151","A0M001","PMEMP","PMI","PMNO","PMDEL","PMNV","FCLBMC","PMCP", "SCP90F",  "SFYGM3",  "SFYGM6",	"SFYGT1",	"SFYGT5",	"SFYGT10",	"SFYAAAC",	"SFYBAAC")
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
transform_season_JP <- function(df.JP)
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
    
  sort(df.JP_header['TRANSFORM NEEDED',])
  
  JP_var_names <- function(name)
  {
    return(names(df.JP_header['TRANSFORM NEEDED',df.JP_header['TRANSFORM NEEDED',] == name]))
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

#Use GBM to forecast 3 months with 3 lags
gbm.forecast_lag <- function(forecast, lags, zoo.C_lag0, country, distr = "bernoulli")
{
  h = forecast
  d = lags
  c = country
  horizon = seq(from =h+1, to = h+d)
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
                      train.fraction = 0.5, 
                      cv.folds = 2, 
                      n.trees = 2000)
  
  #best.iter_test = gbm.perf(gbm.C, method="test"))
  best.iter_cv = gbm.perf(gbm.C, method="cv")

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
  ts.REC = ts(REC_lagRESULT, start = c(begin_year, begin_month), end=c(end_year,begin_month), frequency = 12)
  ts.pred = ts(pred, start = c(begin_year,begin_month), end=c(end_year,end_month), frequency = 12)
  plot(ts.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
  par(new=TRUE)
  
  #Use GG Plot here and include what is h and d
  plot(ts.pred, col = "red", ylab = "Prob. of Recession", main = paste(c, ": Forecast",h,"Months"), axes = TRUE)
  
  #png(filename="~/Google Drive/Independent Work/Writing/Graphs/USH3D3_V2.png")
  #dev.off()

  return(gbm.C)
}
  

### Japan ###
#Transform and Season Japan#
zoo.JP_lag0 = transform_season_JP(df.JP)


#Apply Boosting to Japan
gbm.JP_h0d3 = gbm.forecast_lag(0,3,zoo.JP_lag0, "Japan", "bernoulli")  
gbm.JP_h3d3 = gbm.forecast_lag(3,3,zoo.JP_lag0, "Japan", "bernoulli")  
gbm.JP_h6d3 = gbm.forecast_lag(6,3,zoo.JP_lag0, "Japan")  
gbm.JP_h12d4 = gbm.forecast_lag(12,4,zoo.JP_lag0, "Japan")  

### United States ###
#Transform and Season#
zoo.US_lag0 = transform_season_US(df.US, 'G')


#Apply Boosting to US
gbm.US_h3d3 = gbm.forecast_lag(3,3,zoo.US_lag0, "United States", "poisson")
gbm.US_h3d3 = gbm.forecast_lag(6,3,zoo.US_lag0, "United States", "poisson")

