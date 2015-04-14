
glm.roc_roll_CRAP <- function(zoo.C_lag0, varname = "PMNO", forecast = 0, country, input_end = "1985-08-01", graph = TRUE, manual_end = FALSE)
{
  h = forecast
  c = country
  
  horizon = h + 1
  
  #Lag h+1,h+2,...,h+d  
  zoo.C_lagRESULT = (na.omit(merge(lag(zoo.C_lag0[,2:ncol(zoo.C_lag0)], k = -horizon))))
  #Need to get Recession Information because not included in Lags
  REC_lagRESULT = window(zoo.C_lag0[,1], start = start(zoo.C_lagRESULT), end = end(zoo.C_lagRESULT))
  
  train_start = start(zoo.C_lagRESULT)
  
  if(manual_end == FALSE)
  {
    train_end = warning_train_end(c, input_end)
  }
  else
  {
    train_end = input_end
  }
  test_start = as.Date(train_end) + months(h+1) 
  test_end = end(zoo.C_lagRESULT)
  
  run = elapsed_months(test_end, test_start)
  
  #Create prediction vector
  pred_final = vector("numeric")
  
  #Time
  ptm <- proc.time()
  iters <- sum(run,1)
  
  for(i in 1:iters)
  {
    #Get zoo from 1 to 180, then 2 to 182, then 3 to 183 all the way to run + window so like 10 to 190
    start_shift  <- months(i-1) + as.Date(train_start)
    end_shift <- months(i-1) + as.Date(train_end)
    
    zoo.C_shift =  window(zoo.C_lagRESULT, start = start_shift, end = end_shift)
    REC_shift = window(REC_lagRESULT, start = start_shift, end = end_shift)
    zoo.C_predict = window(zoo.C_lagRESULT, start = (as.Date(test_start)+months(i-1)), end = (as.Date(test_start)+months(i-1)))
    
    glm.C = eval(substitute(glm(REC_shift ~ zoo.C_shift[,varname], family = "binomial"), list(variable = as.name(varname))))
    
    #Forecast starting at test_start
    pred_final[i] =  predict.glm(glm.C,
                                 zoo.C_predict, 
                                 type="response")
    
    #if(i %% 10 == 0)
    #{
    #  cat(i)
    #}
  }
  #Print how long it took for ALL the run
  time_spent = proc.time() - ptm
  
  #Convert into time series object
  from <- as.Date(test_start)
  to <- as.Date(test_start) + months(run)
  months <- seq.Date(from=from,to=to,by="month")
  zoo.pred = zoo(pred_final, months)
  #   zoo.pos = zoo(pos_var, months)
  zoo.REC = window(REC_lagRESULT, 
                   start = start(zoo.pred),
                   end=end(zoo.pred),
                   frequency = 12)
  if(graph == TRUE)
  {
    #Plot Prediction Against ACTUAL Recession
    plot(zoo.REC, col = "blue", ylab = "Prob. of Recession", axes = FALSE)
    par(new=TRUE)
    plot(zoo.pred, col = "red", ylab = "Prob. of Recession", 
         main = paste(c, ":", varname, "GLM Rolling Forecast",h,"Months"), 
         axes = TRUE, 
         ylim=c(0,1))
  }
  
  #Return Prediction, Final Score, CV,Score and Ideally ROC
  return(roc(zoo.REC,zoo.pred))
}