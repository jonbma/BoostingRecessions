
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

plot(zoo.JP_lag0$JAPRECD, col = "red", axes = FALSE, ann = FALSE)
par(new=TRUE)
plot(zoo.JP$JPNVT0060, col = "blue", axes = TRUE)
plot(zoo.JP_lag0$JPNVT0060, col = "blue", axes = TRUE)

###

