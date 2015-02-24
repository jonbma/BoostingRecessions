"""
Goal:
2. Take transformations
3. Plot relevant metrics for intuition
4. Include lags into dataset
5. Apply Adaboost
6. Separate into Train and Test and apply Adaboost and test prediction power
7. Gather Japanese Data Set or Try on U.S Severity of Recession

To Do
-Seasonally adjust NSA series
-Turn dataframe into a time series or at least make sure to be taking differences correctly...
-Start taking log difference, log second difference, keep as level
-Run adaboost
"""

#Preamble
library(ggplot2)
library(gbm)
library(xts)

##### Read in Data and Clean Up #####
#Read In Data as Dataframe
df.US <- read.csv("~/Google Drive/Independent Work/Data/US/US_ALL_TRUNC.csv")
View(df.US)

#Find Missing Data
require(Amelia)
missmap(df.US[0:130], main="US Recession Forecast Data", col=c("yellow", "black"), legend=FALSE)

#Structure of Data
str(df.US)
str(df.US[100:129])

#Convert DATE character into DATE object
df.US$DATE = as.Date(df.US$DATE, format="%m/%d/%Y")

zoo.US = read.zoo(df.US)
##### Seasonal Adjust #####

NSA = c("PERMITNSA","HSBNE","HSBMW","HSBSOU","HSBWST", "A0M070", "FYAAAC", "FYBAAC", "EXRSW", "EXRJAN")
ts.US_NSA = ts(df.US[,NSA], frequency = 12, start=c(1959,2))
ts.US_comp = decompose(ts.US_NSA)

ts.PERMITNSA_SA = ts(df.US[,"PERMITNSA"],frequency = 12, start=c(1959,2)) - (decompose(ts.PERMITNSA)$season)
df.US[,"PERMITNSA"] = as.numeric(ts.PERMITNSA_SA)

##### Transform #####
df.US_TRANS = df.US
#Keep As Level
df.US$AOM011
levels = c("PMP", "CES151", "AOM011", "FCLBMC", "SCP90F","SFYGM3","SFYGM6","SFYGT1","SFYGT5","SFYGT10","SFYAAAC","SFYBAAC", )
  
df[,c("IPS10","IPS11")]

#Log Transform First Difference
#Log Transform Second Difference
#Group 1
for (x in 4:16)
{
df.US_TRANS[,x] = c(diff(log(df.US[,x]), lag = 1),0)
}

df.US_TRANS[,18] = c(df.diff((US[,18]), lag = 1),0)


#Keep as Level

#Second Difference Log Transform

### Summary of Data ###
#Plot Graphs
plot(df.US$DATE, REC, type="l", col = "red", lwd=2, xlab="Date", ylab="Recession", main = "US Recessions 1959-2014")


##### Create Lags #####

##### Apply Gradient Boosting #####
gbm(USRECD ~ YPR, data = df.US)
