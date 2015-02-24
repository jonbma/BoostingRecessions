library(zoo)

US_SPOT_WEEKLY <- read.csv("~/Google Drive/Independent Work/Data/US/US_SPOT_WEEKLY.csv")
View(US_SPOT_WEEKLY)

tt <- US_SPOT_WEEKLY$Date
tt <- as.Date(tt)


month <- function(x) {
  format(x, "%Y-%m")
}

z <- zoo(US_SPOT_WEEKLY$Spot, tt)

aggregate(z, by = month, mean)

zooA = as.zoo(dfA, order.by = row.names(dfA))
write.csv(zooA, file = 'delete.txt', row.names = TRUE)