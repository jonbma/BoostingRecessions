
#Put in recession bars
#Add in annotation for ROC score

US_data = merge(US_logit, US_boost_large)


geom_rect(data=recession.df, 
          aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf), 
          fill="red", alpha=0.3)


library("reshape2")
library("ggplot2")

test_data <- data.frame(
  logit = as.numeric(US_logit),
  boost_large = as.numeric(US_boost_large),
  boost_small = as.numeric(US_boost_small),
  date = time(US_logit)
  )

test_data_long <- melt(test_data, id="date")  # convert to long format


RECD = window(zoo.US_lag0$USRECD, start = start(US_logit))
start <- index(RECD[which(diff(RECD)==1)])
end   <- index(RECD[which(diff(RECD)==-1)-1])
recession.df <- data.frame(start=start, end=end)



gg_out <- 
      autoplot(US_data, facet = NULL)+
      geom_rect(data=recession.df, 
            aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf), 
            fill="red", alpha=0.15)+ 
      ggtitle("US 1986-2014 Recession Probability Forecasts")+
      ylim(0, 1)+
      theme_bw()+
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())



#       scale_shape_discrete(name="Model",
#                       breaks=c("logit", "boost_large"),
#                       labels=c("Best Logit", "Boosting Large"))
#   
#   
  
  
  
  




#US H12 Logit
US_logit = glm.US_h12_roll_best[[1]][[11]]

#US H12 Boost Large
US_boost_large = gbm.US_h12d4_roll_full[[2]]
US_out_boost_big_h12 <- plot_zoo_obj(gbm.US_h12d4_roll_full[[2]], 
                                     roc = gbm.US_h12d4_roll_full[[3]][9],
                                     horizon = 12, 
                                     IN = FALSE, 
                                     LARGE = TRUE,
                                     JP = FALSE)


#US H12 Boost Small
US_boost_small = gbm.US_h12d0_roll_CB5[[2]]
US_out_boost_small_h12 <- plot_zoo_obj(gbm.US_h12d0_roll_CB5[[2]],
                                       roc = gbm.US_h12d0_roll_CB5[[3]][9],
                                       horizon = 12, 
                                       IN = FALSE, 
                                       LARGE = FALSE,
                                       JP = FALSE)