rm(list=ls())
library(latex2exp)
library(ggplot2)
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")
d4 <- read.csv("4-NLStoSLSloss.csv")
#d1=d10[1:49,]
#d2=d20[1:49,]

dAll<-data.frame(NLS1=d1$NLSsse,SLS1=d1$SLSsse,
                 NLS2=d2$NLSsse,SLS2=d2$SLSsse,
                 NLS3=d3$NLSsse,SLS3=d3$SLSsse,
                 NLS4=d4$NLSsse,SLS4=d4$SLSsse)
NLS_Lin_var <- c(var(d1$NLSest_a)+var(d1$NLSest_b),
                 var(d2$NLSest_a)+var(d2$NLSest_b),
                 var(d3$NLSest_a)+var(d3$NLSest_b),
                 var(d4$NLSest_a)+var(d4$NLSest_b))
SLS_Lin_var <- c(var(d1$SLSest_a)+var(d1$SLSest_b),
                 var(d2$SLSest_a)+var(d2$SLSest_b),
                 var(d3$SLSest_a)+var(d3$SLSest_b),
                 var(d4$SLSest_a)+var(d4$SLSest_b))
NLS_Nlin_var <- c(var(d1$NLSest_c),var(d2$NLSest_c),var(d3$NLSest_c),var(d4$NLSest_c))
SLS_Nlin_var <- c(var(d1$SLSest_c),var(d2$SLSest_c),var(d3$SLSest_c),var(d4$SLSest_c))

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

# DVar is a dataframe for showing the variance of the parameter estimates
LVar <- c(NLS_Lin_var, SLS_Lin_var)
NVar <- c(NLS_Nlin_var, SLS_Nlin_var)
PriorInf <- rep(c("1","2","3","4"), 2)
Method <- c(rep("NLS", 4),rep("SLS",4))
DVar <- data.frame(Method, PriorInf, LVar, NVar)

VarRatio <- c(SLS_Lin_var / NLS_Lin_var, SLS_Nlin_var / NLS_Nlin_var)
Linearity <- factor(c(rep("Linear", 4), rep("Non-linear", 4)))
DFVar <- data.frame(PriorInf, Linearity, VarRatio)

fill <- "#4271AE"
line <- "#1F3552"
ggplot(Allbox, aes(x = alllabel, y = LS ,color = Method)) +
  geom_boxplot() +
  scale_y_continuous(name = "Integral matching loss function",
                     breaks = seq(0, 250, 50),
                     limits=c(0, 250)) +
  scale_x_discrete(name = "Quality of prior information",
        labels=c("NLS1"="High","SLS1"="High",
                 "NLS2"="","SLS2"="",
                 "NLS3"="","SLS3"="",
                 "NLS4"="Low","SLS4"="Low"))+ggtitle("NLS vs SLS")+
  theme(
    plot.title = element_text(hjust = 0.5,color="Blue", size=12, face="bold"),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=10, face="bold"))


ggplot(DVar, aes(x=PriorInf, y=LVar)) +
  geom_col(aes(fill=Method), position=position_dodge()) +
  scale_x_discrete(name="Quality of prior information", labels=c("1"="Low", "2"="", "3"="", "4"="High")) +
  scale_y_continuous(name="Variance") +
  ggtitle("Variance of Linear Parameters")

ggplot(DVar, aes(x=PriorInf, y=NVar)) +
  geom_col(aes(fill=Method), position=position_dodge()) +
  scale_x_discrete(name="Quality of prior information", labels=c("1"="Low", "2"="", "3"="", "4"="High")) +
  scale_y_continuous(name="Variance") +
  ggtitle("Variance of Non-linear Parameters")

ggplot(DFVar, aes(x=PriorInf)) +
  geom_point(aes(y=VarRatio, colour=Linearity, shape=Linearity), size=4) +
  scale_colour_discrete(name="Parameter set") +
  scale_shape_discrete(name="Parameter set") +
  scale_x_discrete(name="Quality of prior information", labels=c("1"="Low", "2"="", "3"="", "4"="High")) +
  scale_y_continuous(name=expression(Variance~Ratio~SLS/NLS), limits=c(0,NA)) +
  labs(title="Ratio of variance of parameter estimates for FitzHugh-Nagumo model",
       subtitle=expression(V==-1~";"~R==0.5)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggsave("../out/variance_ratio_fitzhugh-nagumo_V-1_R0.5.pdf", device="pdf")
