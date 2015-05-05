
#Graph Output for H =3 ,6 12 for Japan and US (6 pages)
"""
Include H, D and AUC score
"""

#Japan and US plotting function with recession
plot_zoo_obj <- function(model_output, roc, varname = "PMP", IN = TRUE, BOOST = TRUE, LARGE = TRUE, JP = TRUE, horizon, country)
{
  roc = round(as.numeric(roc),3)

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
    method = paste("Logit Model", varname)
  }
  if(JP == TRUE)
  {
    country = "Japan"
    RECD = zoo.JP_lag0$JAPRECD
    start <- index(RECD[which(diff(RECD)==1)])
    end   <- index(RECD[which(diff(RECD)==-1)-1])
    end <- c(end, as.Date("2014-12-01"))
    text_start = "1985-12-01"
  }
  else
  {
    country = "US"
    RECD = window(zoo.US_lag0$USRECD, start = start(model_output))
    start <- index(RECD[which(diff(RECD)==1)])
    end   <- index(RECD[which(diff(RECD)==-1)-1])
    text_start = "1965-12-01"    
  }
  
  if(IN == TRUE)
  {
    sample_type = "In-Sample"
  }
  else
  {
    sample_type = "Out-Sample"    
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
    ggtitle(paste(country,":",method,",",sample_type,",H=",horizon, sep=""))+
    annotate("text", x = as.Date(text_start), y = 0.90, label = paste("AUC=",roc, sep=""))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  return(gg_object)
}


#In-Sample: Logit
JP_in_logit_h3 <- plot_zoo_obj(glm.in_JP_h3[[11]], 
                    roc = glm.in_JP_h3[[9]],
                    varname = "(Business L Electric Machinery)", 
                    horizon = 3, 
                    IN = TRUE, 
                    LARGE = TRUE, JP = TRUE, BOOST = FALSE)

JP_in_logit_h6 <- plot_zoo_obj(glm.in_JP_h6[[11]], 
                            roc = glm.in_JP_h6[[9]],
                            varname = "(Business M Electric Machinery)", 
                            horizon = 6, 
                            IN = TRUE, 
                            LARGE = TRUE, JP = TRUE, BOOST = FALSE)

JP_in_logit_h12 <- plot_zoo_obj(glm.in_JP_h12[[11]], 
                            roc = glm.in_JP_h12[[9]],
                            varname = "(Lending S Manuf,Petroleum,Coal)", 
                            horizon = 12, 
                            IN = TRUE, 
                            LARGE = TRUE, JP = TRUE, BOOST = FALSE)


US_in_logit_h3 <- plot_zoo_obj(glm.in_US_h3[[11]], 
                               roc = glm.in_US_h3[[9]],
                               varname = "(3 month - FF spread)", 
                               horizon = 3, 
                               IN = TRUE, 
                               LARGE = TRUE, JP = FALSE, BOOST = FALSE)

US_in_logit_h6 <- plot_zoo_obj(glm.in_US_h6[[11]], 
                               roc = glm.in_US_h6[[9]],
                               varname = "(5 year - FF spread)", 
                               horizon = 6, 
                               IN = TRUE, 
                               LARGE = TRUE, JP = FALSE, BOOST = FALSE)

US_in_logit_h12 <- plot_zoo_obj(glm.in_US_h12[[11]], 
                                roc = glm.in_US_h12[[9]],
                                varname = "(10 year - FF spread)", 
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


US_in_boost_large_h3 <- plot_zoo_obj(gbm.US_in_h3d3_big[[4]], 
                                     roc = gbm.US_in_h3d3_big[[3]][9],
                                     horizon = 3, 
                                     IN = TRUE, 
                                     LARGE = TRUE,
                                     JP = FALSE)

US_in_boost_large_h6 <- plot_zoo_obj(gbm.US_in_h6d3_big[[4]], 
                                     roc = gbm.US_in_h6d3_big[[3]][9],
                                     horizon = 6, 
                                     IN = TRUE, 
                                     LARGE = TRUE,
                                     JP = FALSE)

US_in_boost_large_h12 <- plot_zoo_obj(gbm.US_in_h12d4_big[[4]], 
                                      roc = gbm.US_in_h12d4_big[[3]][9],
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

JP_out_logit_h3 <- plot_zoo_obj(glm.JP_h3_roll_best[[1]][[11]],
                            roc = glm.JP_h3_roll_best[[1]][9],
                            varname = "(Business L Manuf. Chemicals)", 
                            horizon = 3, 
                            IN = FALSE, 
                            LARGE = TRUE, 
                            JP = TRUE, 
                            BOOST = FALSE)

JP_out_logit_h6 <- plot_zoo_obj(glm.JP_h6_roll_best[[1]][[11]],
                             roc = glm.JP_h6_roll_best[[1]][9],
                             varname = "(Business M Electric Machinery)", 
                             horizon = 6, 
                             IN = FALSE, 
                             LARGE = TRUE, 
                             JP = TRUE, 
                             BOOST = FALSE)

JP_out_logit_h12 <- plot_zoo_obj(glm.JP_h12_roll_best[[1]][[11]],
                             roc = glm.JP_h12_roll_best[[1]][9],
                             horizon = 12, 
                             IN = FALSE, 
                             LARGE = TRUE, 
                             JP = TRUE, 
                             BOOST = FALSE)


US_out_logit_h3 <- plot_zoo_obj(glm.US_h3_roll_best[[1]][[11]],
                                roc = glm.US_h3_roll_best[[1]][9],
                                varname = "(NAPM new orders)", 
                                horizon = 3, 
                                IN = FALSE, 
                                LARGE = TRUE, 
                                JP = FALSE, 
                                BOOST = FALSE)

US_out_logit_h6 <- plot_zoo_obj(glm.US_h6_roll_best[[1]][[11]],
                                roc = glm.US_h6_roll_best[[1]][9],
                                varname = "(NAPM new orders)", 
                                horizon = 6, 
                                IN = FALSE, 
                                LARGE = TRUE, 
                                JP = FALSE, 
                                BOOST = FALSE)

US_out_logit_h12 <- plot_zoo_obj(glm.US_h12_roll_best[[1]][[11]],
                                 roc = glm.US_h12_roll_best[[1]][9],
                                 varname = "5 year - FF spread", 
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


roc_JP_h12_big = roc(gbm.JP_h12d0_roll_big[[1]],gbm.JP_h12d0_roll_big[[2]], direction="<")

JP_out_boost_big_h12 <- plot_zoo_obj(gbm.JP_h12d0_roll_big[[2]], 
                                 roc = roc_JP_h12_big[[9]],
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


US_out_boost_small_h3 <- plot_zoo_obj(gbm.US_h3d0_roll_CB5[[2]],
                                      roc = gbm.US_h3d0_roll_CB5[[3]][9],
                                      horizon = 3, 
                                      IN = FALSE, 
                                      LARGE = FALSE,
                                      JP = FALSE)

US_out_boost_small_h6 <- plot_zoo_obj(gbm.US_h6d0_roll_CB5[[2]],
                                      roc = gbm.US_h6d0_roll_CB5[[3]][9],
                                      horizon = 6, 
                                      IN = FALSE, 
                                      LARGE = FALSE,
                                      JP = FALSE)

US_out_boost_small_h12 <- plot_zoo_obj(gbm.US_h12d0_roll_CB5[[2]],
                                       roc = gbm.US_h12d0_roll_CB5[[3]][9],
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

#JP Load
load("~/Google Drive/Independent Work/Saved RData/gbm.JP_h3d0_roll_big_4112015.RData")
load("~/Google Drive/Independent Work/Saved RData/gbm.JP_h6d0_roll_big_4112015.RData")
load("~/Google Drive/Independent Work/Saved RData/gbm.JP_h12d0_roll_big_4112015.RData")

load("~/Google Drive/Independent Work/Saved RData/gbm.JP_h3d0_roll_short_4112015.RData")
load("~/Google Drive/Independent Work/Saved RData/gbm.JP_h6d0_roll_short_4112015.RData")
load("~/Google Drive/Independent Work/Saved RData/gbm.JP_h12d0_roll_short_4112015.RData")

#save(glm.US_h3_roll_best, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h3_roll_best_4152015.RData")
#save(glm.US_h6_roll_best, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h6_roll_best_4152015.RData")
#save(glm.US_h12_roll_best, file = "~/Google Drive/Independent Work/Saved RData/glm.US_h12_roll_best_4152015.RData")
# 
# save(gbm.US_h3d3_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d3_roll_full_4142015.RData")
# save(gbm.US_h6d3_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d3_roll_full_4142015.RData")
# save(gbm.US_h12d4_roll_full, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d4_roll_full_4142015.RData")

# save(gbm.US_h3d3_roll_serena, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d3_roll_serena_4122015.RData")
# save(gbm.US_h6d3_roll_serena, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d3_roll_serena_4122015.RData")
#save(gbm.US_h12d4_roll_serena, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d4_roll_serena_4132015.RData")
# 
#save(gbm.US_h3d0_roll_CB5, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h3d0_roll_CB_4152015.RData")
#save(gbm.US_h6d0_roll_CB5, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h6d0_roll_CB_4152015.RData")
#save(gbm.US_h12d0_roll_CB5, file = "~/Google Drive/Independent Work/Saved RData/gbm.US_h12d0_roll_CB_4152015.RData")

#US Load
load( "~/Google Drive/Independent Work/Saved RData/glm.US_h3_roll_best_4112015.RData")
load( "~/Google Drive/Independent Work/Saved RData/glm.US_h6_roll_best_4112015.RData")
load( "~/Google Drive/Independent Work/Saved RData/glm.US_h12_roll_best_4112015.RData")

load("~/Google Drive/Independent Work/Saved RData/gbm.US_h3d3_roll_full_4142015.RData")
load("~/Google Drive/Independent Work/Saved RData/gbm.US_h6d3_roll_full_4142015.RData")
load("~/Google Drive/Independent Work/Saved RData/gbm.US_h12d4_roll_full_4142015.RData")

load("~/Google Drive/Independent Work/Saved RData/gbm.US_h3d0_roll_CB5_4152015.RData")
load("~/Google Drive/Independent Work/Saved RData/gbm.US_h6d0_roll_CB5_4152015.RData")
load("~/Google Drive/Independent Work/Saved RData/gbm.US_h12d0_roll_CB5_4152015.RData")
