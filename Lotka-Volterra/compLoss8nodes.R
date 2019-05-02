rm(list=ls())
library(latex2exp)
library(ggplot2)
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")
d4 <- read.csv("4-NLStoSLSloss.csv")

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
                     breaks = seq(0, 14, 5),
                     limits=c(0, 14)) +
  scale_x_discrete(name = "Quality of prior information",
        labels=c("NLS1"="High","SLS1"="High",
                 "NLS2"="","SLS2"="",
                 "NLS3"="","SLS3"="",
                 "NLS4"="Low","SLS4"="Low"))+ggtitle("NLS vs SLS")+
  theme(
    plot.title = element_text(hjust = 0.5,color="Blue", size=12, face="bold"),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=10, face="bold"))

NLS_Lin_var <- c(var(d1$NLSest_alpha)+var(d1$NLSest_beta)+var(d1$NLSest_gamma)+var(d1$NLSest_delta),
                 var(d2$NLSest_alpha)+var(d2$NLSest_beta)+var(d2$NLSest_gamma)+var(d2$NLSest_delta),
                 var(d3$NLSest_alpha)+var(d3$NLSest_beta)+var(d3$NLSest_gamma)+var(d3$NLSest_delta),
                 var(d4$NLSest_alpha)+var(d4$NLSest_beta)+var(d4$NLSest_gamma)+var(d4$NLSest_delta))
SLS_Lin_var <- c(var(d1$SLSest_alpha)+var(d1$SLSest_beta)+var(d1$SLSest_gamma)+var(d1$SLSest_delta),
                 var(d2$SLSest_alpha)+var(d2$SLSest_beta)+var(d2$SLSest_gamma)+var(d2$SLSest_delta),
                 var(d3$SLSest_alpha)+var(d3$SLSest_beta)+var(d3$SLSest_gamma)+var(d3$SLSest_delta),
                 var(d4$SLSest_alpha)+var(d4$SLSest_beta)+var(d4$SLSest_gamma)+var(d4$SLSest_delta))

PriorInf <- rep(c("1","2","3","4"), 2)
VarRatio <- c(SLS_Lin_var / NLS_Lin_var)
DFVar <- data.frame(PriorInf, VarRatio)

ggplot(DFVar, aes(x=PriorInf)) +
  geom_point(aes(y=VarRatio, colour="Linear", shape="Linear"), size=4) +
  scale_x_discrete(name="Quality of prior information", labels=c("1"="High", "2"="", "3"="", "4"="Low")) +
  scale_y_continuous(name=expression(Variance~Ratio~SLS/NLS), limits=c(0,NA)) +
  scale_colour_discrete(name="Parameter set") +
  scale_shape_discrete(name="Parameter set") +
  labs(title="Ratio of variance of parameter estimates for Lotka-Volterra model",
       subtitle=expression(sigma==0.4~";"~alpha==2/3~";"~beta==4/3~";"~gamma==1~";"~delta==1)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggsave("../out/variance_ratio_lotka-volterra_sigma0.4_a0.67_b1.33_g1_d1.pdf", device="pdf")

h_alpha <- ggplot(d4) +
  geom_histogram(aes(x=NLSest_alpha, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_alpha, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=2/3) +
  scale_x_continuous(name=expression(Estimate~alpha)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
h_beta <- ggplot(d4) +
  geom_histogram(aes(x=NLSest_beta, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_beta, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=4/3) +
  scale_x_continuous(name=expression(Estimate~beta)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
h_gamma <- ggplot(d4) +
  geom_histogram(aes(x=NLSest_gamma, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_gamma, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=1) +
  scale_x_continuous(name=expression(Estimate~gamma)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
h_delta <- ggplot(d4) +
  geom_histogram(aes(x=NLSest_delta, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_delta, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=1) +
  scale_x_continuous(name=expression(Estimate~delta)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
ggsave("../out/hist_linear_0.4_lotka-volterra-seasonal.pdf", device="pdf",
       arrangeGrob(h_alpha, h_beta, h_gamma, h_delta, ncol=2, heights=unit(c(3, 3), c("in", "in")),
                   top="Lotka-Volterra linear parameter estimates sigma=0.4"))
