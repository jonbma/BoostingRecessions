glm.roc_roll_old <- function(zoo.C_lag0, varname = "TERMSPREAD", forecast = 0, country, input_end = "1985-08-01", graph = TRUE)
{
  train_end = warning_train_end(country, input_end)
  h = forecast
  forward = sum(-h,-1)
  c = country
  train_start = start(zoo.C_lag0)
  
  test_start = as.Date(train_end) + months(h+1) 
  test_end = end(zoo.C_lag0)
  
  run = elapsed_months(test_end, test_start)
  pred_final = vector("numeric")
  
  iters = sum(run,1)
  
  for(i in 1:iters)
  {
    start_shift  <- months(i-1) + as.Date(train_start)
    end_shift <- months(i-1) + as.Date(train_end)
    
    zoo.C_shift =  window(zoo.C_lag0, start = start_shift, end = end_shift)
    zoo.C_predict = window(zoo.C_lag0, start = end_shift, end = end_shift)
    
    if(c == "US")
    {
      glm.C = eval(substitute( dyn$glm(USRECD ~ lag(variable, forward), data = zoo.C_shift, family = "binomial"),list(variable = as.name(varname))))
    }
    else if(c == "JP")
    {      
      glm.C = eval(substitute(
        dyn$glm(JAPRECD ~ lag(variable, forward), 
                data = zoo.C_shift, 
                family = "binomial"),
        list(variable = as.name(varname))))
    }
    else
    {
      stop("Uh oh, no country specified or incorrect spelling")
    }
    #Forecast using LAST time to forecast NEXT h period
    pred_final[i] =  predict.glm(glm.C,
                                 newdata = zoo.C_predict, 
                                 type="response")
    
  }
  
  from <- as.Date(test_start)
  to <- as.Date(test_end)
  months <- seq.Date(from=from,to=to,by="month")
  zoo.pred = zoo(pred_final, months)
  zoo.REC = window(zoo.C_lag0[,1], 
                   start = start(zoo.pred),
                   end=end(zoo.pred),
                   frequency = 12)
  
  #Plot Prediction Against ACTUAL Recession
  if(graph == TRUE)
  {
    plot(zoo.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
    par(new=TRUE)
    plot(zoo.pred, col = "red", ylab = "Prob. of Recession", 
         main = paste(c, ":", varname, "CRAP OLD GLM Roll Forecast",h,"Months"), 
         axes = TRUE, 
         ylim=c(0,1))
  }
  return(roc(zoo.REC, zoo.pred))
}