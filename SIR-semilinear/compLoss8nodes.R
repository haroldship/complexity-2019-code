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
                     breaks = seq(0, 16, 2),
                     limits=c(0, 16)) +
  scale_x_discrete(name = "Quality of prior information",
        labels=c("NLS1"="High","SLS1"="High",
                 "NLS2"="","SLS2"="",
                 "NLS3"="","SLS3"="",
                 "NLS4"="Low","SLS4"="Low")) +
  ggtitle(expression(NLS~vs~SLS~of~"semi-linear SIR"~Model~with~epsilon==0.001)) +
  theme(
    plot.title = element_text(hjust = 0.5,color="Blue", size=12, face="bold"),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=10, face="bold"))
ggsave("nls_vs_sls_semi-linear-sir_epsilon-0.001.pdf", device="pdf")


# variances of parameters estimates
NLS_Lin_var <- c(var(d1$NLSest_beta1_1)+var(d1$NLSest_beta1_2)+var(d1$NLSest_beta2_1)+var(d1$NLSest_beta2_2),
                 var(d2$NLSest_beta1_1)+var(d2$NLSest_beta1_2)+var(d2$NLSest_beta2_1)+var(d2$NLSest_beta2_2),
                 var(d3$NLSest_beta1_1)+var(d3$NLSest_beta1_2)+var(d3$NLSest_beta2_1)+var(d2$NLSest_beta2_2),
                 var(d4$NLSest_beta1_1)+var(d4$NLSest_beta1_2)+var(d4$NLSest_beta2_1)+var(d4$NLSest_beta2_2))
SLS_Lin_var <- c(var(d1$SLSest_beta1_1)+var(d1$SLSest_beta1_2)+var(d1$SLSest_beta2_1)+var(d1$SLSest_beta2_2),
                 var(d2$SLSest_beta1_1)+var(d2$SLSest_beta1_2)+var(d2$SLSest_beta2_1)+var(d2$SLSest_beta2_2),
                 var(d3$SLSest_beta1_1)+var(d3$SLSest_beta1_2)+var(d3$SLSest_beta2_1)+var(d2$SLSest_beta2_2),
                 var(d4$SLSest_beta1_1)+var(d4$SLSest_beta1_2)+var(d4$SLSest_beta2_1)+var(d4$SLSest_beta2_2))
NLS_Nlin_var <- c(var(d1$NLSest_S1_1)+var(d1$NLSest_S2_1)+var(d1$NLSest_S1_2)+var(d1$NLSest_S2_2)+var(d1$NLSest_S1_3)+var(d1$NLSest_S2_3)+var(d1$NLSest_S1_4)+var(d1$NLSest_S2_4)+var(d1$NLSest_S1_5)+var(d1$NLSest_S2_5),
                  var(d2$NLSest_S1_1)+var(d2$NLSest_S2_1)+var(d2$NLSest_S1_2)+var(d2$NLSest_S2_2)+var(d2$NLSest_S1_3)+var(d2$NLSest_S2_3)+var(d2$NLSest_S1_4)+var(d2$NLSest_S2_4)+var(d2$NLSest_S1_5)+var(d2$NLSest_S2_5),
                  var(d3$NLSest_S1_1)+var(d3$NLSest_S2_1)+var(d3$NLSest_S1_2)+var(d3$NLSest_S2_2)+var(d3$NLSest_S1_3)+var(d3$NLSest_S2_3)+var(d3$NLSest_S1_4)+var(d3$NLSest_S2_4)+var(d3$NLSest_S1_5)+var(d3$NLSest_S2_5),
                  var(d4$NLSest_S1_1)+var(d4$NLSest_S2_1)+var(d4$NLSest_S1_2)+var(d4$NLSest_S2_2)+var(d4$NLSest_S1_3)+var(d4$NLSest_S2_3)+var(d4$NLSest_S1_4)+var(d4$NLSest_S2_4)+var(d4$NLSest_S1_5)+var(d4$NLSest_S2_5))
SLS_Nlin_var <- c(var(d1$SLSest_S1_1)+var(d1$SLSest_S2_1)+var(d1$SLSest_S1_2)+var(d1$SLSest_S2_2)+var(d1$SLSest_S1_3)+var(d1$SLSest_S2_3)+var(d1$SLSest_S1_4)+var(d1$SLSest_S2_4)+var(d1$SLSest_S1_5)+var(d1$SLSest_S2_5),
                  var(d2$SLSest_S1_1)+var(d2$SLSest_S2_1)+var(d2$SLSest_S1_2)+var(d2$SLSest_S2_2)+var(d2$SLSest_S1_3)+var(d2$SLSest_S2_3)+var(d2$SLSest_S1_4)+var(d2$SLSest_S2_4)+var(d2$SLSest_S1_5)+var(d2$SLSest_S2_5),
                  var(d3$SLSest_S1_1)+var(d3$SLSest_S2_1)+var(d3$SLSest_S1_2)+var(d3$SLSest_S2_2)+var(d3$SLSest_S1_3)+var(d3$SLSest_S2_3)+var(d3$SLSest_S1_4)+var(d3$SLSest_S2_4)+var(d3$SLSest_S1_5)+var(d3$SLSest_S2_5),
                  var(d4$SLSest_S1_1)+var(d4$SLSest_S2_1)+var(d4$SLSest_S1_2)+var(d4$SLSest_S2_2)+var(d4$SLSest_S1_3)+var(d4$SLSest_S2_3)+var(d4$SLSest_S1_4)+var(d4$SLSest_S2_4)+var(d4$SLSest_S1_5)+var(d4$SLSest_S2_5))

# DFVar is a dataframe for showing the variance ratio of the parameter estimates
PriorInf <- rep(c("1","2","3","4"), 2)
VarRatio <- c(SLS_Lin_var / NLS_Lin_var, SLS_Nlin_var / NLS_Nlin_var)
Linearity <- factor(c(rep("Linear", 4), rep("Non-linear", 4)))
DFVar <- data.frame(PriorInf, Linearity, VarRatio)

ggplot(DFVar, aes(x=PriorInf)) +
  geom_point(aes(y=VarRatio, colour=Linearity, shape=Linearity), size=4) +
  scale_colour_discrete(name="Parameter set") +
  scale_shape_discrete(name="Parameter set") +
  scale_x_discrete(name="Quality of prior information", labels=c("1"="Low", "2"="", "3"="", "4"="High")) +
  scale_y_continuous(name=expression(Variance~Ratio~SLS/NLS), limits=c(0,1.25)) +
  ggtitle(expression("Ratio of variance of parameter estimates for semi-linear SIR model"~epsilon==0.001)) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("variance_ratio_semi-linear-sir_epsilon-0.001.pdf", device="pdf")
