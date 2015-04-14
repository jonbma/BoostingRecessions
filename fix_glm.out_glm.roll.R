glm.out_new <- function(zoo.C_lag0, forecast = 0, country, varname = "PMP", input_end = "1985-08-01", logit = "TRUE", graph = "TRUE")
{
  STANDARD = TRUE
  forward <- sum(-1,-forecast)
  Y = zoo.C_lag0[,1]
  X = zoo.C_lag0[,varname]
  X_lag = lag(X, forward)
    
  zoo.final <- na.omit(merge(Y,X_lag))
    
  train_start = start(zoo.final)
  train_end = "1985-08-01"
  test_start = as.Date(train_end) + months(1)
  test_end = end(zoo.final)
  
  zoo.train = window(zoo.final, start = train_start, end=train_end)
  zoo.test = window(zoo.final, start = test_start, end=test_end)  
  
  model = "logit"
  
  if(STANDARD == TRUE)
  {
  #Train Model
  glm.C_os = glm(as.numeric(zoo.train[,1]) ~ as.numeric(zoo.train[,2]), family = binomial(link = model))
  
  pred_os = predict(glm.C_os, 
                    newdata = zoo.test,
                    type="response")
  
  from <- as.Date(test_start)
  to <- as.Date(test_end)
  months <- seq.Date(from=from,to=to,by="month")
  zoo.pred = zoo(pred_os, months)
  
  zoo.REC = window(zoo.final[,1], 
                   start = start(pred_os),
                   end=end(pred_os),
                   frequency = 12)
  }
  #Rolling
  else
  {
    run = elapsed_months(test_end, test_start)
  
    yhat = matrix(NA, length(test_start:test_end-1), 1)
    test_start
    j = 1
    for(i in c(test_start, test_end))
    {
      x = X[j:i,]
      y = Y[j:i,]
      glmfit = glm(y~x$varname, family = "binomial")
      temp = predict(glmfit, data = X, family = "binomial")
      yhat[j] = temp[i+1]
      j = j+1
    }
    
  }

  if(graph == TRUE)
  {
    plot(zoo.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
    par(new=TRUE)
    plot(pred_os, ylab = "Prob. of Recession", main = paste(country, ":", varname,"NEW GLM out Forecast",forecast,"Months"), axes = TRUE)
  }
  roc_score_logit_os = roc(zoo.REC,pred_os)
  return(roc_score_logit_os)
}



#Test cases

#US
glm.US_h6_out = glm.out_new(zoo.US_lag0_big, forecast = 6, country = "US", varname = "CES015")


glm.US_h6_out = glm.out(zoo.US_lag0_big, forecast = 6, country = "US", varname = "CES015")
glm.JP_h12_out_best = glm.out(zoo.JP_lag0_big, forecast = 12, country = "JP", varname = "JPNTK0952")
