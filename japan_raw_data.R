
### Look at Japan Raw ###

plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$NEWJOB, col = "blue", axes = TRUE)


plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$CONCONF, col = "blue", axes = TRUE)


plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$INTSPREAD, col = "blue", axes = TRUE, xlab = "Year", ylab = "Interest Rate Spread", main = "Japan Raw Data 1978-2014" )

plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$IBORATE, col = "blue", axes = TRUE, xlab = "Year", ylab = "Interbank Lending Rate (3 months)", main = "Japan Raw Data 1978-2014" )

plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$STOCKPRIC, col = "blue", axes = TRUE)

plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$JQI.J, col = "blue", axes = TRUE)

plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$BCSTNDNSJ, col = "blue", axes = TRUE)

plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$M1J, col = "blue", axes = TRUE)


plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$IPIRFG, col = "blue", axes = TRUE)



plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$CONCONF, col = "blue", axes = TRUE)

plot(zoo.JP$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$JPNVT0060,col = "blue", axes = TRUE, xlab = "Year", ylab = "Import Raw Material in Yen", main = "Japan Raw Data 1978-2014: Raw Material and Recession" )


plot(zoo.JP_lag0$JPNVT0060, col = "blue", axes = TRUE)

raw_pre1992 = window(zoo.JP$JPNVT0060, start = "1963-01-01", end = "1991-12-01")
raw_pre1992_demean = raw_pre1992 - mean(raw_pre1992)
raw_post1992 = window(zoo.JP$JPNVT0060, start = "1992-1-01")
raw_post1992_demean = raw_post1992 - mean(raw_post1992)
plot(raw_pre1992_demean)

plot(rbind(raw_pre1992_demean, raw_post1992_demean))