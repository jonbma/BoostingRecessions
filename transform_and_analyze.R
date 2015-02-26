"""
To Do
-Seasonally adjust NSA series
-figure out how to combine different series and remove rows with NA in them
-figure out how to include lags to forecast
-Run adaboost
-Separate into Train and Test and apply Adaboost and test prediction power
-Gather Japanese Data Set or Try on U.S Severity of Recession
"""

#Preamble
library(ggplot2)
library(gbm)
library(xts)

##### Read in Data and Clean Up #####
#Read In Data as Dataframe
df.US <- read.csv("~/Google Drive/Independent Work/Data/US/US_ALL_TRUNC.csv")
View(df.US)

#Structure of Data
str(df.US)
str(df.US[100:129])

#Convert DATE character into DATE object
df.US$DATE = as.Date(df.US$DATE, format="%m/%d/%Y")
zoo.US = read.zoo(df.US)

#Fill in NA for LHELX#
#na.approx(zoo.US$LHELX, zoo.US$LHELM)
zoo.US = na.aggregate(zoo.US)

##### Seasonal Adjust #####

NSA = c("PERMITNSA","HSBNE","HSBMW","HSBSOU","HSBWST", "A0M070", "FYAAAC", "FYBAAC", "EXRSW", "EXRJAN")
ts.US_NSA = ts(df.US[,NSA], frequency = 12, start=c(1959,2))
ts.US_comp = decompose(ts.US_NSA)

ts.PERMITNSA_SA = ts(df.US[,"PERMITNSA"],frequency = 12, start=c(1959,2)) - (decompose(ts.PERMITNSA)$season)
df.US[,"PERMITNSA"] = as.numeric(ts.PERMITNSA_SA)

##### Transform #####

#Keep Already Transformed
zoo.US_fix = zoo.US[,c("USRECD","YPR")]

#Keep As Level 
levels = c("PMP","CES151","A0M001","PMEMP","PMI","PMNO","PMDEL","PMNV","FCLBMC","PMCP", "SCP90F",  "SFYGM3",	"SFYGM6",	"SFYGT1",	"SFYGT5",	"SFYGT10",	"SFYAAAC",	"SFYBAAC")
zoo.US_levels = zoo.US[,levels]

#Log
log_stay = c("HSFR","HSNE","HSMW","HSSOU","HSWST","PERMITNSA","HSBNE","HSBMW","HSBSOU","HSBWST")
zoo.US_log = log(zoo.US[,log_stay])

#Level Transform First Difference
level_1D = c("UTL11", "LHELX", "LHUR", "LHU680", "CES155","HHSNTN", "CCIPY", "FYFF","CP90","FYGM3","FYGM6","FYGT1","FYGT5","FYGT10","FYAAAC","FYBAAC")
zoo.US_1D = diff(zoo.US[,level_1D])

#Log Transform First Difference
log_1D = c("IPS10",  "IPS11",	"IPS12",	"IPS13",	"IPS18",	"IPS25",	"IPS32",	"IPS34",	"IPS38",	"IPS43",	"IPS299",	"IPS307",	"IPS306", "LHEM",  "LHNAG",	"LHU5",	"LHU14",	"LHU15",	"LHU26",	"LHU27",	"CLAIMUII",	"CES002",	"CES003",	"CES006",	"CES011",	"CES015",	"CES017", "CES033",  "CES046",	"CES048",	"CES049",	"CES053",	"CES088",	"CES140", "A1M008",  "A0M007",	"A0M027",	"A0M070", "CONS.R",  "MTQ",	"A0M059","FM2.R", "EXRUS",  "EXRSW",	"EXRJAN",	"EXRUK",	"EXRCAN", "FSPCOM",  "FSPIN", "FSPXE")
zoo.US_log_1D = diff(log(zoo.US[,log_1D]), lag = 1)

#Log Transform Second Difference
log_2D = c("CES275",  "CES277",	"CES278", "FM1",  "FM2",	"FMSCU",	"FMFBA",	"FMRRA",	"FMRNBA",	"FCLNBW",	"CCINRV", "PWFSA",  "PWFCSA",	"PWIMSA",	"PWCMSA",	"PSCCOM",	"PW102",	"PUNEW",	"PU83",	"PU84",	"PU85",	"PUC",	"PUCD",	"PUS",	"PUXF",	"PUXHS",	"PUXM",	"GMDC",	"GMDCD",	"GMDCN",	"GMDCS")
zoo.US[,log_2D]
zoo.US_log_2D = diff(log(zoo.US[,log_2D]), lag = 2)

#Need to find what 2 series are missing

ncol(zoo.US_all) #127 columns
ncol(df.US) #131 columns including a date colum, so missing 3 series..

#Combine transformed Series
zoo.US_all = merge(zoo.US_fix, zoo.US_levels, zoo.US_log, zoo.US_1D, zoo.US_log_1D, zoo.US_log_2D)
#Remove rows with NA in them
zoo.US_all = (na.omit(zoo.US_all))

### Summary of Data ###
#Plot Graphs
#plot(df.US$DATE,, type="l", col = "red", lwd=2, xlab="Date", ylab="Recession", main = "US Recessions 1959-2014")


##### Create Lags #####

##### Apply Gradient Boosting #####
gbm.US1 = gbm(zoo.US_all$USRECD ~ . ,data =zoo.US_all[,2:127], distribution = "bernoulli", shrinkage = 0.001, train.fraction = 0.5, bag.fraction =0.5)
gbm.US2 = gbm(zoo.US_all$USRECD ~ . ,data =zoo.US_all[,2:127], distribution = "bernoulli", shrinkage = 0.01, train.fraction = 0.5, bag.fraction =0.5)


#Use Out of Bag Estimator
best.iter <- gbm.perf(gbm.US1,method="OOB")

best.iter <- gbm.perf(gbm.US1,method="test")

#Print the Top Predictors
summary(gbm.US1,n.trees=best.iter)
