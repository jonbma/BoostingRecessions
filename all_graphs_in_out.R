
#Graph Output for H =3 ,6 12 for Japan and US (6 pages)
"""
Include H, D and AUC score
"""

#In-Sample: Logit

#In-Sample: Boost Large Data

#In-Sample: Boost Small Data

#Out-Of-Sample: Rolling Logit 

#Out-Of-Sample: Rolling Boost Large

#Out-Of-Sample: Rolling Boost Small



# Fourth plot
p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
  geom_histogram(colour="black", binwidth=50) +
  facet_grid(Diet ~ .) +
  ggtitle("Final weight, by diet") +
  theme(legend.position="none")        # No legend (redundant in this graph)    

multiplot(p1, p2, p3, p4, cols=2)