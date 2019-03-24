rm(list=ls())
library(latex2exp)
library(ggplot2)
d10 <- read.csv("1-NLStoSLStime.csv")
d20 <- read.csv("2-NLStoSLStime.csv")
d30 <- read.csv("3-NLStoSLStime.csv")
d40 <- read.csv("4-NLStoSLStime.csv")
d1=d10[1:49,]
d2=d20[1:49,]
d3=d30[1:49,]
d4=d40[1:49,]

dAll<-data.frame(NLS1=d1$NLStime,SLS1=d1$SLStime,
                 NLS2=d2$NLStime,SLS2=d2$SLStime,
                 NLS3=d3$NLStime,SLS3=d3$SLStime,
                 NLS4=d4$NLStime,SLS4=d4$SLStime)

alllabel=c(rep("NLS1",length(dAll$NLS1)),rep("SLS1",length(dAll$SLS1)),
           rep("NLS2",length(dAll$NLS2)),rep("SLS2",length(dAll$SLS2)),
           rep("NLS3",length(dAll$NLS3)),rep("SLS3",length(dAll$SLS3)),
           rep("NLS4",length(dAll$NLS4)),rep("SLS4",length(dAll$SLS4)))

Method=c(rep("NLS",length(dAll$NLS1)),rep("SLS",length(dAll$SLS1)),
            rep("NLS",length(dAll$NLS2)),rep("SLS",length(dAll$SLS2)),
            rep("NLS",length(dAll$NLS3)),rep("SLS",length(dAll$SLS3)),
            rep("NLS",length(dAll$NLS4)),rep("SLS",length(dAll$SLS4)))

LS=c(dAll$NLS1,dAll$SLS1,
     dAll$NLS2,dAll$SLS2,
     dAll$NLS3,dAll$SLS3,
     dAll$NLS4,dAll$SLS4)
     
Allbox=data.frame(alllabel,Method,LS)


fill <- "#4271AE"
line <- "#1F3552"
ggplot(Allbox, aes(x = alllabel, y = LS ,color = Method)) +
  geom_boxplot() +
  scale_y_continuous(name = "Estimation time (sec)",
                     breaks = seq(0, 1.2, 0.2),
                     limits=c(0, 1.2)) +
  scale_x_discrete(name = "Quality of prior information",
        labels=c("NLS1"="High","SLS1"="High",
                 "NLS2"="","SLS2"="",
                 "NLS3"="","SLS3"="",
                 "NLS4"="Low","SLS4"="Low"))+ggtitle("NLS vs SLS")+
  theme(
    plot.title = element_text(hjust = 0.5,color="Blue", size=12, face="bold"),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=10, face="bold"))
