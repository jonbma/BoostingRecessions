
#Graph Output for H =3 ,6 12 for Japan and US (6 pages)
"""
Include H, D and AUC score
"""

#Japan
plot_zoo_obj <- function(model_output, roc, varname = "PMP", IN = TRUE, BOOST = TRUE, LARGE = TRUE, JP = TRUE, horizon, country)
{
  roc = round(as.numeric(roc),3)
  if(IN == TRUE)
  {
    sample_type = "In-sample"
  }
  else
  {
    sample_type = "Out-sample"
  }
  if(BOOST == TRUE)
  {
    if(LARGE == TRUE)
      {
      method = "Boosting Large Dataset"
    }
    else
    {
      method = "Boosting Small Dataset"
    }
  }
  else
  {
    method = paste("Best Logit Model using", varname)
  }
  if(JP == TRUE)
  {
    country = "Japan"
    RECD = zoo.JP_lag0$JAPRECD
    start <- index(RECD[which(diff(RECD)==1)])
    end   <- index(RECD[which(diff(RECD)==-1)-1])
    end <- c(end, as.Date("2014-12-01"))
  }
  else
  {
    country = "US"
    RECD = zoo.US_lag0$USRECD
    start <- index(RECD[which(diff(RECD)==1)])
    end   <- index(RECD[which(diff(RECD)==-1)-1])
  }

  recession.df <- data.frame(start=start, end=end)
  gg_object <- 
    autoplot(model_output) + 
    xlab("Time") +
    ylab("Probability") + 
    ylim(0, 1) +
    theme_bw() + 
    geom_rect(data=recession.df, 
              aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf), 
              fill="red", alpha=0.3) +
    ggtitle(paste(country,":",method,",",horizon, "Months",",", sample_type))+
    annotate("text", x = as.Date("1985-12-01"), y = 0.90, label = paste("AUROC:",roc))
  
  return(gg_object)
}




#In-Sample: Logit
in_logit_h3 <- plot_zoo_obj(glm.in_JP_h3[[11]], 
                    roc = glm.in_JP_h3[[9]],
                    varname = "JPNTK0118", 
                    horizon = 3, 
                    IN = TRUE, 
                    LARGE = TRUE, JP = TRUE, BOOST = FALSE)

in_logit_h6 <- plot_zoo_obj(glm.in_JP_h6[[11]], 
                            roc = glm.in_JP_h6[[9]],
                            varname = "JPNTK0199", 
                            horizon = 6, 
                            IN = TRUE, 
                            LARGE = TRUE, JP = TRUE, BOOST = FALSE)

in_logit_h12 <- plot_zoo_obj(glm.in_JP_h12[[11]], 
                            roc = glm.in_JP_h12[[9]],
                            varname = "JPNTK1954", 
                            horizon = 12, 
                            IN = TRUE, 
                            LARGE = TRUE, JP = TRUE, BOOST = FALSE)


#In-Sample: Boost Large Data
in_boost_large_h3 <- plot_zoo_obj(gbm.JP_h3d0_big[[4]], 
                                  roc = gbm.JP_h3d0_big[[3]],
                                  horizon = 3, 
                                  IN = TRUE, 
                                  LARGE = TRUE)

in_boost_large_h6 <- plot_zoo_obj(gbm.JP_h6d0_big[[4]], 
                                  roc = gbm.JP_h6d0_big[[3]],
                                  horizon = 6, 
                                  IN = TRUE, 
                                  LARGE = TRUE)

in_boost_large_h12 <- plot_zoo_obj(gbm.JP_h12d0_big[[4]], 
                                  roc = gbm.JP_h12d0_big[[3]],
                                  horizon = 12, 
                                  IN = TRUE, 
                                  LARGE = TRUE)
#In-Sample: Boost Small Data
in_boost_small_h3 <- plot_zoo_obj(gbm.JP_h3d0_short[[4]],
                                  gbm.JP_h3d0_short[[3]],
                                  horizon = 3, 
                                  IN = TRUE, 
                                  LARGE = FALSE)

in_boost_small_h6 <- plot_zoo_obj(gbm.JP_h6d0_short[[4]],
                                  gbm.JP_h6d0_short[[3]],
                                  horizon = 6, 
                                  IN = TRUE, 
                                  LARGE = FALSE)


in_boost_small_h12 <- plot_zoo_obj(gbm.JP_h12d0_short[[4]],
                                  gbm.JP_h12d0_short[[3]],
                                  horizon = 12, 
                                  IN = TRUE, 
                                  LARGE = FALSE)





#Out-Sample: Logit Roll
out_logit_h3 <- plot_zoo_obj(glm.JP_h3_roll_best[[11]],
                            roc = glm.JP_h3_roll_best[9],
                            varname = "JPNTK0096", 
                            horizon = 3, 
                            IN = FALSE, 
                            LARGE = TRUE, 
                            JP = TRUE, 
                            BOOST = FALSE)

#Out-Sample: Boost Large Data Roll
out_boost_big_h3 <- plot_zoo_obj(gbm.JP_h3d0_roll_big[[2]], 
                                 roc = gbm.JP_h3d0_roll_big[[3]][9],
                                   horizon = 3, 
                                   IN = FALSE, 
                                   LARGE = TRUE)

#Out-Sample: Boost Small Data Roll
out_boost_small_h3 <- plot_zoo_obj(gbm.JP_h3d0_roll_short[[2]],
                                   roc = gbm.JP_h3d0_roll_short[[3]][9],
                                   horizon = 3, 
                                   IN = FALSE, 
                                   LARGE = FALSE)


multiplot(in_logit_h3, in_boost_large_h3, in_boost_small_h3, out_logit_h3, out_boost_big_h3, out_boost_small_h3, cols = 2)
multiplot(in_logit_h6, in_boost_large_h6, in_boost_small_h6, out_logit_h6, 1) out_boost_big_h6, out_boost_small_h6, cols = 2)
multiplot(in_logit_h12, in_boost_large_h12, in_boost_small_h12, out_logit_h12, out_boost_big_h12, out_boost_small_h12, cols = 2)



