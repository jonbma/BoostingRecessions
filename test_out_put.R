



#Train Ends: 1985-08-01
#Test Starts: 1985-09-01
glm.roll.US_h3 #PMNO: Starts 1985-12-01 (= 3+1 + 1985-08-01)
glm.roll.US_h3 = glm.roc_roll(zoo.US_lag0, forecast = 3, varname = "PMNO", country = "US", )
glm.out.PMNO3 #PMNO: Starts 1985-12-01 (= 3+1 + 1985-08-01)
gbm.US_h3d3_roll_full #Starts 1986-07-01..should start 1985-12-01. I'm losing 7 months!