
#Graph Output for H =3 ,6 12 for Japan and US (6 pages)
"""
Include H, D and AUC score
"""

#Japan and US plotting function with recession
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
JP_in_logit_h3 <- plot_zoo_obj(glm.in_JP_h3[[11]], 
                    roc = glm.in_JP_h3[[9]],
                    varname = "JPNTK0118", 
                    horizon = 3, 
                    IN = TRUE, 
                    LARGE = TRUE, JP = TRUE, BOOST = FALSE)

JP_in_logit_h6 <- plot_zoo_obj(glm.in_JP_h6[[11]], 
                            roc = glm.in_JP_h6[[9]],
                            varname = "JPNTK0199", 
                            horizon = 6, 
                            IN = TRUE, 
                            LARGE = TRUE, JP = TRUE, BOOST = FALSE)

JP_in_logit_h12 <- plot_zoo_obj(glm.in_JP_h12[[11]], 
                            roc = glm.in_JP_h12[[9]],
                            varname = "JPNTK1954", 
                            horizon = 12, 
                            IN = TRUE, 
                            LARGE = TRUE, JP = TRUE, BOOST = FALSE)


US_in_logit_h3 <- plot_zoo_obj(glm.in_US_h3[[11]], 
                               roc = glm.in_US_h3[[9]],
                               varname = "PMNO", 
                               horizon = 3, 
                               IN = TRUE, 
                               LARGE = TRUE, JP = FALSE, BOOST = FALSE)

US_in_logit_h6 <- plot_zoo_obj(glm.in_US_h6[[11]], 
                               roc = glm.in_US_h6[[9]],
                               varname = "JPNTK0199", 
                               horizon = 6, 
                               IN = TRUE, 
                               LARGE = TRUE, JP = FALSE, BOOST = FALSE)

US_in_logit_h12 <- plot_zoo_obj(glm.in_US_h12[[11]], 
                                roc = glm.in_US_h12[[9]],
                                varname = "JPNTK1954", 
                                horizon = 12, 
                                IN = TRUE, 
                                LARGE = TRUE, JP = FALSE, BOOST = FALSE)




#In-Sample: Boost Large Data
JP_in_boost_large_h3 <- plot_zoo_obj(gbm.JP_h3d0_big[[4]], 
                                  roc = gbm.JP_h3d0_big[[3]][9],
                                  horizon = 3, 
                                  IN = TRUE, 
                                  LARGE = TRUE)

JP_in_boost_large_h6 <- plot_zoo_obj(gbm.JP_h6d0_big[[4]], 
                                  roc = gbm.JP_h6d0_big[[3]][9],
                                  horizon = 6, 
                                  IN = TRUE, 
                                  LARGE = TRUE)

JP_in_boost_large_h12 <- plot_zoo_obj(gbm.JP_h12d0_big[[4]], 
                                  roc = gbm.JP_h12d0_big[[3]][9],
                                  horizon = 12, 
                                  IN = TRUE, 
                                  LARGE = TRUE)


US_in_boost_large_h3 <- plot_zoo_obj(gbm.US_in_h3d0_big[[4]], 
                                     roc = gbm.US_in_h3d0_big[[3]][9],
                                     horizon = 3, 
                                     IN = TRUE, 
                                     LARGE = TRUE,
                                     JP = FALSE)

US_in_boost_large_h6 <- plot_zoo_obj(gbm.US_in_h6d0_big[[4]], 
                                     roc = gbm.US_in_h6d0_big[[3]][9],
                                     horizon = 6, 
                                     IN = TRUE, 
                                     LARGE = TRUE,
                                     JP = FALSE)

US_in_boost_large_h12 <- plot_zoo_obj(gbm.US_in_h12d0_big[[4]], 
                                      roc = gbm.US_in_h12d0_big[[3]][9],
                                      horizon = 12, 
                                      IN = TRUE, 
                                      LARGE = TRUE,
                                      JP = FALSE)


#In-Sample: Boost Small Data
JP_in_boost_small_h3 <- plot_zoo_obj(gbm.JP_h3d0_short[[4]],
                                  gbm.JP_h3d0_short[[3]][9],
                                  horizon = 3, 
                                  IN = TRUE, 
                                  LARGE = FALSE)

JP_in_boost_small_h6 <- plot_zoo_obj(gbm.JP_h6d0_short[[4]],
                                  gbm.JP_h6d0_short[[3]][9],
                                  horizon = 6, 
                                  IN = TRUE, 
                                  LARGE = FALSE)


JP_in_boost_small_h12 <- plot_zoo_obj(gbm.JP_h12d0_short[[4]],
                                  gbm.JP_h12d0_short[[3]][9],
                                  horizon = 12, 
                                  IN = TRUE, 
                                  LARGE = FALSE)

US_in_boost_small_h3 <- plot_zoo_obj(gbm.US_in_h3d0_short[[4]], 
                                     roc = gbm.US_in_h3d0_short[[3]][9],
                                     horizon = 3, 
                                     IN = TRUE, 
                                     LARGE = FALSE,
                                     JP = FALSE)

US_in_boost_small_h6 <- plot_zoo_obj(gbm.US_in_h6d0_short[[4]], 
                                     roc = gbm.US_in_h6d0_short[[3]][9],
                                     horizon = 6, 
                                     IN = TRUE, 
                                     LARGE = FALSE,
                                     JP = FALSE)

US_in_boost_small_h12 <- plot_zoo_obj(gbm.US_in_h12d0_short[[4]], 
                                      roc = gbm.US_in_h12d0_short[[3]][9],
                                      horizon = 12, 
                                      IN = TRUE, 
                                      LARGE = FALSE,
                                      JP = FALSE)



#Out-Sample: Logit Roll
JP_out_logit_h3 <- plot_zoo_obj(glm.JP_h3_roll_best[[11]],
                            roc = glm.JP_h3_roll_best[9],
                            varname = "JPNTK0096", 
                            horizon = 3, 
                            IN = FALSE, 
                            LARGE = TRUE, 
                            JP = TRUE, 
                            BOOST = FALSE)

JP_out_logit_h6 <- plot_zoo_obj(glm.JP_h6_roll_best[[11]],
                             roc = glm.JP_h6_roll_best[9],
                             varname = "JPNTK0199", 
                             horizon = 6, 
                             IN = FALSE, 
                             LARGE = TRUE, 
                             JP = TRUE, 
                             BOOST = FALSE)

#I used the best logit standard and calculate the roll
JP_out_logit_h12 <- plot_zoo_obj(glm.JP_h12_roll_JPNTK0959[[11]],
                             roc = glm.JP_h12_roll_JPNTK0959[9],
                             varname = "JPNTK0959", 
                             horizon = 12, 
                             IN = FALSE, 
                             LARGE = TRUE, 
                             JP = TRUE, 
                             BOOST = FALSE)


US_out_logit_h3 <- plot_zoo_obj(glm.US_h3_roll_best[[11]],
                                roc = glm.US_h3_roll_best[9],
                                varname = "PMNO", 
                                horizon = 3, 
                                IN = FALSE, 
                                LARGE = TRUE, 
                                JP = FALSE, 
                                BOOST = FALSE)

US_out_logit_h6 <- plot_zoo_obj(glm.US_h6_roll_best[[11]],
                                roc = glm.US_h6_roll_best[9],
                                varname = "EXRUK", 
                                horizon = 6, 
                                IN = FALSE, 
                                LARGE = TRUE, 
                                JP = FALSE, 
                                BOOST = FALSE)

US_out_logit_h12 <- plot_zoo_obj(glm.US_h12_roll_best[[11]],
                                 roc = glm.US_h12_roll_best[9],
                                 varname = "SFYGT5", 
                                 horizon = 12, 
                                 IN = FALSE, 
                                 LARGE = TRUE, 
                                 JP = FALSE, 
                                 BOOST = FALSE)

#Out-Sample: Boost Large Data Roll
JP_out_boost_big_h3 <- plot_zoo_obj(gbm.JP_h3d0_roll_big[[2]], 
                                 roc = gbm.JP_h3d0_roll_big[[3]][9],
                                   horizon = 3, 
                                   IN = FALSE, 
                                   LARGE = TRUE)

JP_out_boost_big_h6 <- plot_zoo_obj(gbm.JP_h6d0_roll_big[[2]], 
                                 roc = gbm.JP_h6d0_roll_big[[3]][9],
                                 horizon = 6, 
                                 IN = FALSE, 
                                 LARGE = TRUE)


JP_out_boost_big_h12 <- plot_zoo_obj(gbm.JP_h12d0_roll_big[[2]], 
                                 roc = gbm.JP_h12d0_roll_big[[3]][9],
                                 horizon = 12, 
                                 IN = FALSE, 
                                 LARGE = TRUE)


US_out_boost_big_h3 <- plot_zoo_obj(gbm.US_h3d3_roll_full[[2]], 
                                    roc = gbm.US_h3d3_roll_full[[3]][9],
                                    horizon = 3, 
                                    IN = FALSE, 
                                    LARGE = TRUE,
                                    JP = FALSE)

US_out_boost_big_h6 <- plot_zoo_obj(gbm.US_h6d3_roll_full[[2]], 
                                    roc = gbm.US_h6d3_roll_full[[3]][9],
                                    horizon = 6, 
                                    IN = FALSE, 
                                    LARGE = TRUE,
                                    JP = FALSE)


US_out_boost_big_h12 <- plot_zoo_obj(gbm.US_h12d4_roll_full[[2]], 
                                     roc = gbm.US_h12d4_roll_full[[3]][9],
                                     horizon = 12, 
                                     IN = FALSE, 
                                     LARGE = TRUE,
                                     JP = FALSE)

#Out-Sample: Boost Small Data Roll
JP_out_boost_small_h3 <- plot_zoo_obj(gbm.JP_h3d0_roll_short[[2]],
                                   roc = gbm.JP_h3d0_roll_short[[3]][9],
                                   horizon = 3, 
                                   IN = FALSE, 
                                   LARGE = FALSE)
JP_out_boost_small_h6 <- plot_zoo_obj(gbm.JP_h6d0_roll_short[[2]],
                                   roc = gbm.JP_h6d0_roll_short[[3]][9],
                                   horizon = 6, 
                                   IN = FALSE, 
                                   LARGE = FALSE)

JP_out_boost_small_h12 <- plot_zoo_obj(gbm.JP_h12d0_roll_short[[2]],
                                   roc = gbm.JP_h12d0_roll_short[[3]][9],
                                   horizon = 12, 
                                   IN = FALSE, 
                                   LARGE = FALSE)


US_out_boost_small_h3 <- plot_zoo_obj(gbm.US_h3d0_roll_short[[2]],
                                      roc = gbm.US_h3d0_roll_short[[3]][9],
                                      horizon = 3, 
                                      IN = FALSE, 
                                      LARGE = FALSE,
                                      JP = FALSE)
US_out_boost_small_h6 <- plot_zoo_obj(gbm.US_h6d0_roll_short[[2]],
                                      roc = gbm.US_h6d0_roll_short[[3]][9],
                                      horizon = 6, 
                                      IN = FALSE, 
                                      LARGE = FALSE,
                                      JP = FALSE)

US_out_boost_small_h12 <- plot_zoo_obj(gbm.US_h12d0_roll_short[[2]],
                                       roc = gbm.US_h12d0_roll_short[[3]][9],
                                       horizon = 12, 
                                       IN = FALSE, 
                                       LARGE = FALSE,
                                       JP = FALSE)


#Japan
multiplot(JP_in_logit_h3, JP_in_boost_large_h3, JP_in_boost_small_h3, JP_out_logit_h3, JP_out_boost_big_h3, JP_out_boost_small_h3, cols = 2)
multiplot(JP_in_logit_h6, JP_in_boost_large_h6, JP_in_boost_small_h6, JP_out_logit_h6, JP_out_boost_big_h6, JP_out_boost_small_h6, cols = 2)
multiplot(JP_in_logit_h12, JP_in_boost_large_h12, JP_in_boost_small_h12, JP_out_logit_h12, JP_out_boost_big_h12, JP_out_boost_small_h12, cols = 2)

#US
multiplot(US_in_logit_h3, US_in_boost_large_h3, US_in_boost_small_h3, US_out_logit_h3, US_out_boost_big_h3, US_out_boost_small_h3, cols = 2)
multiplot(US_in_logit_h6, US_in_boost_large_h6, US_in_boost_small_h6, US_out_logit_h6, US_out_boost_big_h6, US_out_boost_small_h6, cols = 2)
multiplot(US_in_logit_h12, US_in_boost_large_h12, US_in_boost_small_h12, US_out_logit_h12, US_out_boost_big_h12, US_out_boost_small_h12, cols = 2)



### Save and Load Zone ###

#Save Japan values In-Sample
#setwd("~/Google Drive/Independent Work/Saved RData")
#load("glm.in_JP_h3_4112015.RData")
#load("glm.in_JP_h6_4112015.RData")
#load("gbm.JP_h6d0_roll_big_4112015.RData")

# save(glm.in_JP_h3, file = "~/Google Drive/Independent Work/Saved RData/glm.in_JP_h3_4112015.RData")
# save(glm.in_JP_h6, file = "~/Google Drive/Independent Work/Saved RData/glm.in_JP_h6_4112015.RData")
# save(glm.in_JP_h12, file = "~/Google Drive/Independent Work/Saved RData/glm.in_JP_h12_4112015.RData")
# 
# save(gbm.JP_h3d0_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h3d0_big_4112015.RData")
# save(gbm.JP_h6d0_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h6d0_big_4112015.RData")
# save(gbm.JP_h12d0_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h12d0_big_4112015.RData")
# 
# save(gbm.JP_h3d0_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h3d0_short_4112015.RData")
# save(gbm.JP_h6d0_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h6d0_short_4112015.RData")
# save(gbm.US_h12d0_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h12d0_short_4112015.RData")
# 
# save(glm.JP_h3_roll_best, file = "~/Google Drive/Independent Work/Saved RData/glm.JP_h3_roll_best_4112015.RData")
# save(glm.JP_h3_roll_best, file = "~/Google Drive/Independent Work/Saved RData/glm.JP_h3_roll_best_4112015.RData")
# save(glm.JP_h3_roll_best, file = "~/Google Drive/Independent Work/Saved RData/glm.JP_h3_roll_best_4112015.RData")
# save(glm.JP_h12_roll_JPNTK0959, file = "~/Google Drive/Independent Work/Saved RData/glm.JP_h12_roll_JPNTK0959_4112015.RData")
# 
# save(glm.JP_h3_out_best, file = "~/Google Drive/Independent Work/Saved RData/glm.JP_h3_roll_best_4112015.RData")
# save(glm.JP_h3_out_best, file = "~/Google Drive/Independent Work/Saved RData/glm.JP_h3_roll_best_4112015.RData")
# save(glm.JP_h3_out_best, file = "~/Google Drive/Independent Work/Saved RData/glm.JP_h3_roll_best_4112015.RData")
# 
# save(gbm.JP_h3d0_roll_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h3d0_roll_big_4112015.RData")
# save(gbm.JP_h6d0_roll_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h6d0_roll_big_4112015.RData")
# save(gbm.JP_h12d0_roll_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h12d0_roll_big_4112015.RData")
# 
# save(gbm.JP_h3d0_roll_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h3d0_roll_short_4112015.RData")
# save(gbm.JP_h6d0_roll_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h6d0_roll_short_4112015.RData")
# save(gbm.JP_h12d0_roll_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.JP_h12d0_roll_short_4112015.RData")

save(glm.in_US_h3, file = "~/Google Drive/Independent Work/Saved RData/glm.in_US_h3_4112015.RData")
save(glm.in_US_h6, file = "~/Google Drive/Independent Work/Saved RData/glm.in_US_h6_4112015.RData")
save(glm.in_US_h12, file = "~/Google Drive/Independent Work/Saved RData/glm.in_US_h12_4112015.RData")

save(gbm.US_h3d0_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d0_big_4112015.RData")
save(gbm.US_h6d0_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d0_big_4112015.RData")
save(gbm.US_h12d0_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d0_big_4112015.RData")

save(gbm.US_h3d0_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d0_short_4112015.RData")
save(gbm.US_h6d0_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d0_short_4112015.RData")
save(gbm.US_h12d0_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d0_short_4112015.RData")

save(glm.US_h3_roll_best, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h3_roll_best_4112015.RData")
save(glm.US_h3_roll_best, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h3_roll_best_4112015.RData")
save(glm.US_h3_roll_best, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h3_roll_best_4112015.RData")
#save(glm.US_h12_roll_USNTK0959, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h12_roll_USNTK0959_4112015.RData")

save(glm.US_h3_out_best, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h3_roll_best_4112015.RData")
save(glm.US_h3_out_best, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h3_roll_best_4112015.RData")
save(glm.US_h3_out_best, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h3_roll_best_4112015.RData")

save(gbm.US_h3d0_roll_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d0_roll_big_4112015.RData")
save(gbm.US_h6d0_roll_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d0_roll_big_4112015.RData")
save(gbm.US_h12d0_roll_big, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d0_roll_big_4112015.RData")

save(gbm.US_h3d0_roll_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d0_roll_short_4112015.RData")
save(gbm.US_h6d0_roll_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d0_roll_short_4112015.RData")
save(gbm.US_h12d0_roll_short, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d0_roll_short_4112015.RData")