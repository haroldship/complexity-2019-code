rm(list=ls())
library(latex2exp)
library(ggplot2)
library(gridExtra)

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
  scale_y_continuous(name = "Normalized integral matching loss function",
                     limits=c(0, 0.1)) +
  scale_x_discrete(name = "Sample Size",
                   labels=c("NLS1"="100","SLS1"="100",
                            "NLS2"="400","SLS2"="400",
                            "NLS3"="900","SLS3"="900",
                            "NLS4"="1,600","SLS4"="1,600")) + 
  labs(title="Integral matching loss for Lotka-Volterra seasonal model",
       subtitle=expression(sigma==0.1~";"~alpha==2/3~";"~beta==4/3~";"~gamma==1~";"~delta==1~";"~epsilon==0.2~";"~omega==0.5)) +
  theme(
    plot.title = element_text(hjust = 0.5,color="Blue", size=12, face="bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=10, face="bold"))
ggsave("../out/loss_by_sample_lotka-volterra-seasonal_sigma0.1_a0.67_b1.33_g1_d1_e0.2_o0.5.pdf", device="pdf")

NLS_Lin_var <- c(var(d1$NLSest_alpha)+var(d1$NLSest_beta)+var(d1$NLSest_gamma)+var(d1$NLSest_delta),
                 var(d2$NLSest_alpha)+var(d2$NLSest_beta)+var(d2$NLSest_gamma)+var(d2$NLSest_delta),
                 var(d3$NLSest_alpha)+var(d3$NLSest_beta)+var(d3$NLSest_gamma)+var(d3$NLSest_delta),
                 var(d4$NLSest_alpha)+var(d4$NLSest_beta)+var(d4$NLSest_gamma)+var(d4$NLSest_delta))
SLS_Lin_var <- c(var(d1$SLSest_alpha)+var(d1$SLSest_beta)+var(d1$SLSest_gamma)+var(d1$SLSest_delta),
                 var(d2$SLSest_alpha)+var(d2$SLSest_beta)+var(d2$SLSest_gamma)+var(d2$SLSest_delta),
                 var(d3$SLSest_alpha)+var(d3$SLSest_beta)+var(d3$SLSest_gamma)+var(d3$SLSest_delta),
                 var(d4$SLSest_alpha)+var(d4$SLSest_beta)+var(d4$SLSest_gamma)+var(d4$SLSest_delta))
NLS_Nlin_var <- c(var(d1$NLSest_epsilon)+var(d1$NLSest_omega),
                  var(d2$NLSest_epsilon)+var(d2$NLSest_omega),
                  var(d3$NLSest_epsilon)+var(d3$NLSest_omega),
                  var(d4$NLSest_epsilon)+var(d4$NLSest_omega))
SLS_Nlin_var <- c(var(d1$SLSest_epsilon)+var(d1$SLSest_omega),
                  var(d2$SLSest_epsilon)+var(d2$SLSest_omega),
                  var(d3$SLSest_epsilon)+var(d3$SLSest_omega),
                  var(d4$SLSest_epsilon)+var(d4$SLSest_omega))

PriorInf <- rep(c("1","2","3","4"), 2)
Linearity <- factor(c(rep("Linear", 4), rep("Non-linear", 4)))
VarRatio <- c(SLS_Lin_var / NLS_Lin_var, SLS_Nlin_var / NLS_Nlin_var)
DFVar <- data.frame(PriorInf, Linearity, VarRatio)

# DVar is a dataframe for showing the variance of the parameter estimates
LVar <- c(NLS_Lin_var, SLS_Lin_var)
NVar <- c(NLS_Nlin_var, SLS_Nlin_var)
SampleSize <- rep(c(100, 400, 900, 1600), 2)
Method <- c(rep("NLS", 4),rep("SLS",4))
DVar <- data.frame(Method, SampleSize, LVar, NVar)

VarRatio <- c(SLS_Lin_var / NLS_Lin_var, SLS_Nlin_var / NLS_Nlin_var)
Linearity <- factor(c(rep("Linear", 4), rep("Non-linear", 4)))
DFVar <- data.frame(PriorInf, Linearity, VarRatio)

col_lin <- ggplot(DVar, aes(x=SampleSize, y=LVar)) +
  geom_col(aes(fill=Method), position=position_dodge()) +
  scale_x_sqrt(name="Sample Size", breaks=c(100,400,900,1600)) +
  scale_y_continuous(name="Variance") +
  labs(title="Variance of Linear parameter estimates for Lotka-Volterra seasonal",
       subtitle=expression(sigma==0.1~";"~alpha==2/3~";"~beta==4/3~";"~gamma==1~";"~delta==1~";"~epsilon==0.2~";"~omega==0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size=10), plot.subtitle = element_text(hjust = 0.5, size=9))

col_nlin <- ggplot(DVar, aes(x=SampleSize, y=NVar)) +
  geom_col(aes(fill=Method), position=position_dodge()) +
  scale_x_sqrt(name="Sample Size", breaks=c(100,400,900,1600)) +
  scale_y_continuous(name="Variance") +
  labs(title="Variance of Nonlinear parameter estimates for Lotka-Volterra seasonal",
       subtitle=expression(sigma==0.1~";"~alpha==2/3~";"~beta==4/3~";"~gamma==1~";"~delta==1~";"~epsilon==0.2~";"~omega==0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size=10), plot.subtitle = element_text(hjust = 0.5, size=9))
ggsave("../out/variance_by_sample_lotka-volterra-seasonal_sigma0.1_a0.67_b1.33_g1_d1_e0.2_o0.5.pdf", device="pdf",
       arrangeGrob(col_lin, col_nlin, ncol=2))

ggplot(DFVar, aes(x=PriorInf)) +
  geom_point(aes(y=VarRatio, colour=Linearity, shape=Linearity), size=4) +
  scale_colour_discrete(name="Parameter set") +
  scale_shape_discrete(name="Parameter set") +
  scale_x_discrete(name="Sample Size", labels=c("1"="100", "2"="400", "3"="900", "4"="1,600")) +
  scale_y_continuous(name=expression(Variance~Ratio~SLS/NLS), limits=c(0,NA)) +
  labs(title="Ratio of variance of parameter estimates for Lotka-Volterra seasonal model",
       subtitle=expression(sigma==0.1~";"~alpha==2/3~";"~beta==4/3~";"~gamma==1~";"~delta==1~";"~epsilon==0.2~";"~omega==0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size=10), plot.subtitle = element_text(hjust = 0.5))
ggsave("../out/variance_ratio_by_sample_lotka-volterra-seasonal_sigma0.1_a0.67_b1.33_g1_d1_e0.2_o0.5.pdf", device="pdf")

h_alpha <- ggplot(d1) +
  geom_histogram(aes(x=NLSest_alpha, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_alpha, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=2/3) +
  scale_x_continuous(name=expression(Estimate~alpha)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
h_beta <- ggplot(d1) +
  geom_histogram(aes(x=NLSest_beta, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_beta, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=4/3) +
  scale_x_continuous(name=expression(Estimate~beta)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
h_gamma <- ggplot(d1) +
  geom_histogram(aes(x=NLSest_gamma, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_gamma, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=1) +
  scale_x_continuous(name=expression(Estimate~gamma)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
h_delta <- ggplot(d1) +
  geom_histogram(aes(x=NLSest_delta, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_delta, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=1) +
  scale_x_continuous(name=expression(Estimate~delta)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
ggsave("../out/hist_linear_100_lotka-volterra-seasonal.pdf", device="pdf",
       arrangeGrob(h_alpha, h_beta, h_gamma, h_delta, ncol=2, heights=unit(c(3, 3), c("in", "in")),
                   top="Lotka-Volterra season linear parameter estimates, n=100"))

h_epsilon <- ggplot(d1) +
  geom_histogram(aes(x=NLSest_epsilon, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_epsilon, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=0.2) +
  scale_x_continuous(name=expression(Estimate~epsilon)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
h_omega <- ggplot(d1) +
  geom_histogram(aes(x=NLSest_omega, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_omega, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=0.5) +
  scale_x_continuous(name=expression(Estimate~omega)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
ggsave("../out/hist_non-linear_100_lotka-volterra-seasonal.pdf", device="pdf",
       arrangeGrob(h_epsilon, h_omega, ncol=2, heights=unit(c(3, 3), c("in", "in")),
                   top="Lotka-Volterra season non-linear parameter estimates, n=100"))

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
ggsave("../out/hist_linear_1600_lotka-volterra-seasonal.pdf", device="pdf",
       arrangeGrob(h_alpha, h_beta, h_gamma, h_delta, ncol=2, heights=unit(c(3, 3), c("in", "in")),
                   top="Lotka-Volterra season linear parameter estimates, n=1,600"))

h_epsilon <- ggplot(d4) +
  geom_histogram(aes(x=NLSest_epsilon, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_epsilon, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=0.2) +
  scale_x_continuous(name=expression(Estimate~epsilon)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
h_omega <- ggplot(d4) +
  geom_histogram(aes(x=NLSest_omega, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.05) +
  geom_histogram(aes(x=SLSest_omega, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.05) +
  geom_vline(xintercept=0.5) +
  scale_x_continuous(name=expression(Estimate~omega)) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method")
ggsave("../out/hist_non-linear_1600_lotka-volterra-seasonal.pdf", device="pdf",
       arrangeGrob(h_epsilon, h_omega, ncol=2, heights=unit(c(3, 3), c("in", "in")),
                   top="Lotka-Volterra season non-linear parameter estimates, n=1,600"))
