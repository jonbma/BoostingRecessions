
#GW Test
USRECD3 = window(zoo.US_lag0$USRECD, start = start(pred.glm.US_h0d0), end = end(pred.glm.US_h0d0))

starth0d3 = start(gbm.US_h0d3)
endh0d3 = end(gbm.US_h0d3)

begin_month = as.numeric(format(start(gbm.US_h0d3)[2],"%m"))
begin_year = as.numeric(format(start(REC_lagRESULT),"%Y"))
end_month = as.numeric(format(end(REC_lagRESULT),"%m"))
end_year = as.numeric(format(end(REC_lagRESULT),"%Y"))
ts.gbm.US_h0d3 = ts(gbm.US_h0d3, start = c(start(gbm.US_h0d3)[1], start(gbm.US_h0d3)[2]), end=c(end(gbm.US_h0d3)[1], end((gbm.US_h0d3)[2]), frequency = 12)
                    ts.pred = ts(pred, start = c(begin_year,begin_month), end=c(end_year,end_month), frequency = 12)
                    
                    
                    gw.test(
                      pred.glm.US_h0d0[3:length(pred.glm.US_h0d0)],
                      gbm.US_h0d3, 
                      zoo.US_lag0$USRECD[4:length(zoo.US_lag0$USRECD)],
                      T = length(gbm.US_h0d3),
                      tau = 10, 
                      method = c("HAC", "NeweyWest"),
                      alternative = "two.sided")