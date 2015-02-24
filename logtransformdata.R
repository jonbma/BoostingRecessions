US <- read.csv("~/Google Drive/Independent Work/Data/US/US_ALL_TRUNC.csv")
US_TRANS = US

#First Difference Log Transform

#Group 1
for (x in 4:16)
{
US_TRANS[,x] = c(diff(log(US[,x]), lag = 1),0)
}

US_TRANS[,x] = c(diff(log(US[,x]), lag = 1),0)



#Keep as Level

#Second Difference Log Transform