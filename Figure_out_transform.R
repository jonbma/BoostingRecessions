#US 
autoplot(zoo.US$IPS10)
IPS10_log_1D = autoplot(Delt(zoo.US$IPS10))
IPS10_detrend = zoo(detrend(as.numeric(zoo.US$IPS10)),time(zoo.US))
autoplot(IPS10_detrend)
autoplot(diff(IPS10_detrend,1))
autoplot(Delt(zoo.US$IPS10))

varname = "A1M008"
autoplot(zoo.US[,varname])
autoplot(diff(zoo.US)[,varname])
autoplot(Delt(zoo.US[,varname]))
autoplot(diff(log(zoo.US[,varname]), differences = 2))
autoplot(log(zoo.US[,varname]))



#JP

#OK So I remove the slope and merge intercepts of the break, now what?
#Do I take log differences or do I take level differences?

autoplot(zoo(detrend(as.numeric(zoo.JP$JPNTI0003),bp=194),time(zoo.JP)))

autoplot(zoo.JP$JQI.J)
JPQIJ_detrend = zoo(detrend(as.numeric(zoo.JP$JPNTI0003),bp=194),time(zoo.JP))

autoplot(Delt(zoo.JP$JQI.J))
autoplot(diff(JPQIJ_detrend,1))

#Should I log_1D or diff NEWJOB
varname = "JPNTI0003"
zoo.varname = zoo.JP[,varname]
getSymbols("JPNRECD",src="FRED")
varname.df <- data.frame(date= index(zoo.varname), value = as.vector(zoo.varname))

start <- index(JPNRECD[which(diff(JPNRECD$JPNRECD)==1)])
end   <- index(JPNRECD[which(diff(JPNRECD$JPNRECD)==-1)-1])
end <- c(end, as.Date("2014-12-01"))
reccesion.df <- data.frame(start=start, end=end)
recession.df <- subset(reccesion.df, start >= min(varname.df$date))
varname.df <- subset(varname.df, date >= start[1])


ggplot()+
  geom_line(data=varname.df, aes(x=date,y=value)) +
  theme_bw() +
  geom_rect(data=recession.df,
            aes(xmin=start,xmax=end, ymin=min(varname.df$value),ymax=max(varname.df$value)), 
            fill="red", alpha=0.5) 



varname = "JQIQTOTAL"
zoo.varname = zoo.JP[,varname]
autoplot(zoo.varname)
autoplot(DEMEAN_COUNTRY(zoo.JP, varname)[,varname])
autoplot(DETREND_COUNTRY(zoo.JP, varname)[,varname])
autoplot(diff(DETREND_COUNTRY(zoo.JP, varname)[,varname]))
autoplot(diff(DEMEAN_COUNTRY(zoo.JP, varname)[,varname]))
autoplot(Delt(DEMEAN_COUNTRY(zoo.JP, varname)[,varname]))
autoplot(Delt(zoo.JP[,varname]))





CPALTT01JPM659N = getSymbols('CPALTT01JPM659N',src='FRED', auto.assign=F) 
CPALTT01JPM659N.df = data.frame(date=time(CPALTT01JPM659N), coredata(CPALTT01JPM659N))
recessions.df = read.table(textConnection(
  "Peak, Trough
1857-06-01, 1858-12-01
  1860-10-01, 1861-06-01
  1865-04-01, 1867-12-01
  1869-06-01, 1870-12-01
  1873-10-01, 1879-03-01
  1882-03-01, 1885-05-01
  1887-03-01, 1888-04-01
  1890-07-01, 1891-05-01
  1893-01-01, 1894-06-01
  1895-12-01, 1897-06-01
  1899-06-01, 1900-12-01
  1902-09-01, 1904-08-01
  1907-05-01, 1908-06-01
  1910-01-01, 1912-01-01
  1913-01-01, 1914-12-01
  1918-08-01, 1919-03-01
  1920-01-01, 1921-07-01
  1923-05-01, 1924-07-01
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01"), sep=',',
colClasses=c('Date', 'Date'), header=TRUE)
recessions.trim = subset(recessions.df, Peak >= min(CPALTT01JPM659N.df$date) )

df_varname <- data.frame(value = as.vector(zoo.varname), time = time(zoo.varname))
df_REC <- data.frame(value = as.vector(zoo.REC), time = time(zoo.REC))

g = ggplot(df_varname) + geom_line(aes(x= time, y= value)) + theme_bw()
g = g + geom_rect(data=df_REC, aes(xmin=, xmax=end(zoo.REC), ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2)


library(quantmod)

getSymbols("JPNRECP",src="FRED")
#getSymbols("LRUN64TTJPM156N", src="FRED")

varname.df <- data.frame(date= index(varname), value = varname)

start <- index(JPNRECP[which(diff(JPNRECP$JPNRECP)==1)])
end   <- index(JPNRECP[which(diff(JPNRECP$JPNRECP)==-1)-1])

reccesion.df <- data.frame(start=start, end=end[length(end)])
recession.df <- subset(reccesion.df, start >= min(varname[,1]))

ggplot()+
  geom_line(data=varname.df, aes(x=date,y=value)) +
  geom_rect(data=recession.df,
            aes(xmin=start,xmax=end, ymin=0,ymax=max(varname[,2])), 
            fill="red", alpha=0.2)

