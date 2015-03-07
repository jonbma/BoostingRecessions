"""
To Do
-modularize h,d function
-Gather Japanese Data Set
-Try on U.S Severity of Recession
"""
#Set workdirectory
setwd("~/Google Drive/Independent Work")

#Preamble
library(ggplot2)
library(gbm)
library(xts)
library(stats)
library(Amelia)

################### Read in Data and Clean ######################

#Read In Data as Dataframe for US
df.US <- read.csv("~/Google Drive/Independent Work/Data/US/US_ALL_TRUNC.csv")

#Read in Data as Dataframe for Japan
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

####US ###
df.US = date_COUNTRY(df.US)
zoo.US = zoo_COUNTRY(df.US)
zoo.US = na.approx(zoo.US)
#Last Observation Carry Forward
zoo.US = na.locf(na.locf(na.approx(zoo.US)), fromLast = TRUE)
#Add Constant to "FMRNBA" so not negative log difference
zoo.US$FMRNBA = zoo.US$FMRNBA  + 4000 
View(df.US)
str(df.US)

###Japan ###
df.JP = date_COUNTRY(df.JP)
zoo.JP = zoo_COUNTRY(df.JP)

zoo.JP = window(zoo.JP, start = "1975-01-01", end = end(zoo.JP))
missmap(zoo.JP, main="Japan Data - Missings Map",  col=c("yellow", "black"), legend=TRUE)

zoo.JP = window(zoo.JP, start = "1978-01-01", end = end(zoo.JP))
missmap(zoo.JP, main="Japan Data - Missings Map",  col=c("yellow", "black"), legend=TRUE)

#Approximate couple missing values
zoo.JP = na.approx(zoo.JP)
#Impute values using the mean
zoo.JP = na.aggregate(zoo.JP)
missmap(zoo.JP, main="Japan Data - Missings Map",  col=c("yellow", "black"), legend=TRUE)


################### Seasonal Adjust ######################

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

## United States ### 
NSA = c("PERMITNSA","HSBNE","HSBMW","HSBSOU","HSBWST", "A0M070", "FYAAAC", "FYBAAC", "EXRSW", "EXRJAN")
ts.US_NSA = ts(df.US[,NSA], frequency = 12, start=c(1959,2))
ts.PERMITNSA_SA = ts.US_NSA[,NSA[1]] - decompose(ts.US_NSA[,NSA[1]])$season
for(i in 1:length(NSA))
{
  zoo.US[,NSA[i]]=ts.US_NSA[,NSA[i]] - decompose(ts.US_NSA[,NSA[i]])$season
}

### Japan ###
JP_NSA = names(df.JP_header['NSA',df.JP_header['NSA',] == 1])
zoo.JP = SA_COUNTRY(zoo.JP, JP_NSA)

################### Transform ######################

### United States ###
#Keep Already Transformed
zoo.US_fix = zoo.US[,c("USRECD","YPR")]

#Keep As Level 
levels = c("PMP","CES151","A0M001","PMEMP","PMI","PMNO","PMDEL","PMNV","FCLBMC","PMCP", "SCP90F",  "SFYGM3",	"SFYGM6",	"SFYGT1",	"SFYGT5",	"SFYGT10",	"SFYAAAC",	"SFYBAAC")
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


### Japan ###

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

TRANSFORM_COUNTRY <- function(zoo.C, same, level_1D, log_0D, log_1D,log_2D)
{
  #Same
  zoo.C_same = zoo.C[,same]
  
  #Level First Difference
  zoo.C_level_1D = diff(zoo.C[,level_1D], differences = 1)
  
  #Log 0 Difference
  #zoo.C_log_0D = log(zoo.C[,log_0D])
  
  #Log First Difference
  zoo.C_log_1D = zoo(apply(zoo.C[,log_1D], 2, Delt),time(zoo.C))
  
  #Log Transform Second Difference
  zoo.C_log_2D = diff(log(zoo.C[,log_2D]), differences = 2)
  
  #Merge
  zoo.C_lag0 = merge(zoo.C_same, zoo.C_level_1D, log(zoo.JP[,log_0D_JP]), zoo.C_log_1D, zoo.C_log_2D)
  
  return(zoo.C_lag0)
}
  
  
TRANSFORM_COUNTRY <- function(zoo.C, same, level_1D, log_0D, log_1D,log_2D)
zoo.JP_lag0 = TRANSFORM_COUNTRY(zoo.JP, same_JP, level_1D_JP, log_0D_JP, log_1D_JP, log_2D_JP)
zoo.JP_lag0 = na.omit(zoo.JP_lag0)

missmap(zoo.JP_lag0, main="Japan Data - Missings Map",  col=c("yellow", "black"), legend=TRUE)


#Need to find what 2 series are missing
ncol(zoo.JP_lag0) #Have duplicates, also check for NaN
nrow(zoo.JP_lag0) 
ncol(zoo.JP) 
nrow(zoo.JP) 


setdiff(names(zoo.JP),names(zoo.JP_lag0))
setdiff(names(zoo.JP_lag0),names(zoo.JP))
### Summary of Data ###
#Plot Graphs
plot(zoo.US_lag0$USRECD, xlab = "Year", ylab = "Recession", col = "Red", main = "US Recessions 1959-2014")
autoplot.zoo(zoo.US_lag0$USRECD, xlab = "Year", ylab = "Recession", col = "Red", main = "US Recessions 1959-2014")
##### Create Lags #####

##### Apply Gradient Boosting #####

#Use GBM to forecast 3 months with 3 lags
gbm.forecast_lag <- function(h, d, data)
{
  for(i in )
  zoo.US_lag456 = merge(lag(zoo.US_lag0[,2:127],-), zoo.US_lag5, zoo.US_lag6)
  
  return(object)
}

zoo.US_lag4 = lag(zoo.US_lag0[,2:127],-4)
zoo.US_lag5 = lag(zoo.US_lag0[,2:127],-5)
zoo.US_lag6 = lag(zoo.US_lag0[,2:127],-6)
zoo.US_lag456 = merge(zoo.US_lag4, zoo.US_lag5, zoo.US_lag6)
zoo.US_lag456 = na.omit(zoo.US_lag456)
USRECD_lag456 = window(zoo.US_lag0$USRECD, start = start(zoo.US_lag456), end = end(zoo.US_lag456))
gbm.US_lag456 = gbm(USRECD_lag456 ~ . ,
                    data =zoo.US_lag456, 
                    distribution = "bernoulli",
                    shrinkage = 0.01, 
                    bag.fraction = 0.5, 
                    train.fraction = 0.5, 
                    cv.folds = 2, 
                    n.trees = 2000)


#Use GBM to forecast 6 months with 3 lags
zoo.US_lag7 = lag(zoo.US_lag0[,2:127],-7)
zoo.US_lag8 = lag(zoo.US_lag0[,2:127],-8)
zoo.US_lag9 = lag(zoo.US_lag0[,2:127],-9)
zoo.US_lag789 = merge(zoo.US_lag7, zoo.US_lag8, zoo.US_lag9)
zoo.US_lag789 = na.omit(zoo.US_lag789)
USRECD_lag789 = window(zoo.US_lag0$USRECD, start = start(zoo.US_lag789), end = end(zoo.US_lag789))
gbm.US_lag789 = gbm(USRECD_lag789 ~ . ,
                    data =zoo.US_lag789, 
                    distribution = "bernoulli",
                    shrinkage = 0.01, 
                    bag.fraction = 0.5, 
                    train.fraction = 0.5, 
                    cv.folds = 2, 
                    n.trees = 2000)


#Use GBM to forecast 12 months with 4 lags
zoo.US_lag13 = lag(zoo.US_lag0[,2:127],-13)
zoo.US_lag14 = lag(zoo.US_lag0[,2:127],-14)
zoo.US_lag15 = lag(zoo.US_lag0[,2:127],-15)
zoo.US_lag16 = lag(zoo.US_lag0[,2:127],-16)
zoo.US_h12d4 = merge(zoo.US_lag13, zoo.US_lag14, zoo.US_lag15,  zoo.US_lag16)
zoo.US_h12d4 = na.omit(zoo.US_lagh12d4)
USRECD_h12d4 = window(zoo.US_lag0$USRECD, start = start(zoo.US_h12d4, end = end(zoo.US_h12d4)))
gbm.US_h12d4 = gbm(USRECD_h12d4 ~ . ,
                    data =zoo.US_h12d4, 
                    distribution = "bernoulli",
                    shrinkage = 0.01, 
                    bag.fraction = 0.5, 
                    train.fraction = 0.5, 
                    cv.folds = 2, 
                    n.trees = 2000)






#Use GBM to Nowcast with 3 lags and current variable
zoo.US_lag1 = lag(zoo.US_lag0[,2:127],-1)
zoo.US_lag2 = lag(zoo.US_lag0[,2:127],-2)
zoo.US_lag3 = lag(zoo.US_lag0[,2:127],-3)
zoo.US_lag0123 = merge(zoo.US_lag0, zoo.US_lag1, zoo.US_lag2, zoo.US_lag3)
gbm.US_lag0123 = gbm(zoo.US_lag0123$USRECD ~ . ,data =zoo.US_lag0123, distribution = "bernoulli", shrinkage = 0.001)


#Use Out of Bag Estimator
#best.iter_lag0 <- gbm.perf(gbm.US_lag0,method="test")
best.iter_lag0123 <- gbm.perf(gbm.US_lag0123,method="OOB")
#best.iter_lag456 <- gbm.perf(gbm.US_lag456,method="OOB")
best.iter_lag456 <- gbm.perf(gbm.US_lag456,method="test")

best.iter_lag456_test = gbm.perf(gbm.US_lag456,method="test")
best.iter_lag456_cv = gbm.perf(gbm.US_lag456,method="cv")
#print(gbm.perf(gbm.US_lag456,method="OOB"))


best.iter_lag456_test = gbm.perf(gbm.US_lag456,method="test")
best.iter_lag456_cv = gbm.perf(gbm.US_lag456,method="cv")
print(best.iter_lag456_test)
print(best.iter_lag456_cv)

best.iter_lag789_test = gbm.perf(gbm.US_lag789,method="test")
best.iter_lag789_cv = gbm.perf(gbm.US_lag789,method="cv")
print(best.iter_lag789_test)
print(best.iter_lag789_cv)

best.iter_h12d4_test = gbm.perf(gbm.US_h12d4,method="test")
best.iter_h12d4_cv = gbm.perf(gbm.US_h12d4,method="cv")
print(best.iter_h12d4_test)
print(best.iter_h12d4_cv)



#Print the Top Predictors
#summary(gbm.US_lag0,n.trees=best.iter)
#summary(gbm.US_lag0123,n.trees=best.iter)
summary(gbm.US_lag456,n.trees=best.iter_lag456_cv)

summary(gbm.US_lag789,n.trees=best.iter_lag789_cv)

head(summary(gbm.US_h12d4,n.trees=best.iter_h12d4_cv), n = 15)


#Get Full-Insample Estimate 
pred3 = predict(gbm.US_lag456,zoo.US_lag456, n.trees= best.iter_lag456_cv, type="response")
pred789 = predict(gbm.US_lag789,zoo.US_lag789, n.trees= best.iter_lag789_cv, type="response")
predh12d4 = predict(gbm.US_h12d4,zoo.US_h12d4, n.trees= best.iter_h12d4_cv, type="response")



#pred2 = predict(gbm.US_lag0123,zoo.US_lag0123, n.trees= best.iter, type="response")
#pred1 = predict(gbm.US_lag0,zoo.US_lag0, n.trees= best.iter, type="response")


#ts.pred1 = ts(pred1, start = c(1959,4), end=c(2014,9), frequency = 12)

#plot(ts.USRECD, col = "blue", ylab = "", axes = FALSE)
autoplot.zoo(zoo.US_lag0$USRECD, xlab = "Year", ylab = "Recession", col = "Red", main = "US Recessions 1959-2014")

autoplot.zoo(ts.pred3, xlab = "Year", ylab = "Recession", col = "Red", main = "US Recessions 1959-2014")


#H =3, D=3
begin_month = as.numeric(format(start(USRECD_lag456),"%m"))
begin_year = as.numeric(format(start(USRECD_lag456),"%Y"))
end_month = as.numeric(format(end(USRECD_lag456),"%m"))
end_year = as.numeric(format(end(USRECD_lag456),"%Y"))
ts.USRECD3 = ts(USRECD_lag456, start = c(begin_year, begin_month), end=c(end_year,begin_month), frequency = 12)
ts.pred3 = ts(pred3, start = c(begin_year,begin_month), end=c(end_year,end_month), frequency = 12)
plot(ts.USRECD3, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
par(new=TRUE)
plot(ts.pred3, col = "red", ylab = "Prob. of Recession", main = "Forecast 3 Months", axes = TRUE)
png(filename="~/Google Drive/Independent Work/Writing/Graphs/USH3D3_V2.png")
dev.off()

#H=6, D=3
begin_month = as.numeric(format(start(USRECD_lag789),"%m"))
begin_year = as.numeric(format(start(USRECD_lag789),"%Y"))
end_month = as.numeric(format(end(USRECD_lag789),"%m"))
end_year = as.numeric(format(end(USRECD_lag789),"%Y"))
ts.USRECD789 = ts(USRECD_lag789, start = c(begin_year, begin_month), end=c(end_year,begin_month), frequency = 12)
ts.pred789 = ts(pred789, start = c(begin_year,begin_month), end=c(end_year,end_month), frequency = 12)
plot(ts.USRECD789, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
par(new=TRUE)
plot(ts.pred789, col = "red", ylab = "Prob. of Recession", main = "Forecasting 6 Months", axes = TRUE)
png(filename="~/Google Drive/Independent Work/Writing/Graphs/USH6D3_V1.png")
dev.off()


#H=12, D = 4
begin_month = as.numeric(format(start(USRECD_h12d4),"%m"))
begin_year = as.numeric(format(start(USRECD_h12d4),"%Y"))
end_month = as.numeric(format(end(USRECD_h12d4),"%m"))
end_year = as.numeric(format(end(USRECD_h12d4),"%Y"))
ts.USRECDh12d4 = ts(USRECD_h12d4, start = c(begin_year, begin_month), end=c(end_year,begin_month), frequency = 12)
ts.predh12d4 = ts(predh12d4, start = c(begin_year,begin_month), end=c(end_year,end_month), frequency = 12)
plot(ts.USRECDh12d4, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
par(new=TRUE)
plot(ts.predh12d4, col = "red", ylab = "Prob. of Recession", main = "Forecasting 12 Months", axes = TRUE)
png(filename="~/Google Drive/Independent Work/Writing/Graphs/USH12D4_V1.png")
dev.off()




  #Test Predict
confusion <- function(a, b){
  tbl <- table(a, b)
  mis <- 1 - sum(diag(tbl))/sum(tbl)
  list(table = tbl, misclass.prob = mis)
}
confusion(predict(gbm.US1, zoo.US_all), as.factor(zoo.US_all$USRECD))$table
