
dat = read.csv("~/Google Drive/Independent Work/Data/US/BergeData/EM_data.csv", stringsAsFactors=FALSE)

h = 12
rawT = dim(dat)[1]
l = (h+1)+1
u = rawT-(h+1)
Y = factor(dat$NBER[l:rawT])
X = data.frame(dat[1:u, 5:23])

# Locals
T = length(Y)
R = 145-h # 1985m5 is first OOS
P = T - R

yhat = rep(NA,P) #Empty for yhat
ooswgt = c(rep(1,R), rep(0,P)) #Weights for OOS prediction
ctrl = boost_control(mstop=100)

for (i in c(1:P)) {
  cat("Iteration", i, "\n")
  glb = glmboost(Y~., data=X, family=Binomial(), weights=ooswgt, control = ctrl)
  temp = predict(glb, type="response")
  #  aic = AIC(glb, k=log(R), method="classical")
  #  temp = predict(glb[mstop(aic)], type="response")
  yhat[i] = temp[i + R]
  
  ooswgt[i] = 0
  ooswgt[i+R] = 1
}


from <- as.Date("1985-06-01")
to <- as.Date("2013-12-01")
months <- seq.Date(from=from,to=to,by="month")
zoo.pred = zoo(yhat, months)
zoo.REC = window(zoo.US_lag0_berge$USRECD, start = start(zoo.pred), end = end(zoo.pred))
roc(zoo.REC, zoo.pred)

plot_zoo_REC(zoo.pred, country = "US")
mean(sample(yhat[dat$NBER[(rawT-P+1):rawT]==1], 1000, replace=T) > 
       sample(yhat[dat$NBER[(rawT-P+1):rawT]==0], 1000, replace=T))
