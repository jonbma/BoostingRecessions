#US
varname = "FSPCOM"
autoplot(zoo.US[,varname])
autoplot(diff(zoo.US)[,varname])
autoplot(Delt(zoo.US[,varname]))
autoplot(diff(log(zoo.US[,varname]), differences = 2))
autoplot(log(zoo.US[,varname]))

#JP
varname = "JPNTK0595"
zoo.JP = zoo.JP_lag0_big
zoo.varname = zoo.JP[,varname]
autoplot(zoo.varname)
autoplot(DEMEAN_COUNTRY(zoo.JP, varname)[,varname])
autoplot(DETREND_COUNTRY(zoo.JP, varname)[,varname])
autoplot(diff(DETREND_COUNTRY(zoo.JP, varname)[,varname]))
autoplot(Delt(DETREND_COUNTRY(zoo.JP, varname)[,varname]))
autoplot(diff(DEMEAN_COUNTRY(zoo.JP, varname)[,varname]))
autoplot(Delt(DEMEAN_COUNTRY(zoo.JP, varname)[,varname]))
autoplot(Delt(zoo.JP[,varname]))
