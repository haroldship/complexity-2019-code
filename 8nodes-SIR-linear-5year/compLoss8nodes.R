rm(list=ls())
library(latex2exp)
library(ggplot2)
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")
d4 <- read.csv("4-NLStoSLSloss.csv")
#d1=d10[1:49,]
#d2=d20[1:49,]

dAll<-data.frame(NLS1=d1$NLSmc,SLS1=d1$SLSmc,
                 NLS2=d2$NLSmc,SLS2=d2$SLSmc,
                 NLS3=d3$NLSmc,SLS3=d3$SLSmc,
                 NLS4=d4$NLSmc,SLS4=d4$SLSmc)

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
  scale_y_continuous(name = "Integral matching loss function",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "Quality of prior information",
        labels=c("NLS1"="High","SLS1"="High",
                 "NLS2"="","SLS2"="",
                 "NLS3"="","SLS3"="",
                 "NLS4"="Low","SLS4"="Low"))+ggtitle("NLS vs SLS")+
  theme(
    plot.title = element_text(hjust = 0.5,color="Blue", size=12, face="bold"),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=10, face="bold"))

