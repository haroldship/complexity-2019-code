rm(list=ls())
library(latex2exp)
library(ggplot2)
library(gridExtra)
library(tidyr)

pars1 <- c('gamma11','f121','f131','gamma12','f112','f122','gamma13','f113','f133')
pars2 <- c('gamma21','f211','f221','gamma22','f222')
pars3 <- c('gamma31','f311','f331','gamma32','f332')
pars <- c(pars1,pars2,pars3)
lin_pars <- c('gamma11','gamma12','gamma13','gamma21','gamma22','gamma31','gamma32')
nlin_pars <- setdiff(pars,lin_pars)

theta1 <- c(0.4,-1,-1,3,0.5,-0.1,2,0.75,-0.2)
theta2 <- c(3,0.5,-0.1,1.5,0.5)
theta3 <- c(2,0.75,-0.2,5,0.5)
theta <- c(theta1,theta2,theta3)
names(theta) <- pars


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
  scale_y_continuous(name = "Integral matching loss function") +
  #coord_cartesian(ylim=c(0,5)) +
  scale_x_discrete(name = "Quality of prior information",
                   labels=c("NLS1"="High","SLS1"="High",
                            "NLS2"="","SLS2"="",
                            "NLS3"="","SLS3"="",
                            "NLS4"="Low","SLS4"="Low")) +
  ggtitle(expression(NLS~vs~SLS~of~GMA~Model~with~SNR==10)) +
  theme(
    plot.title = element_text(hjust = 0.5, color="Blue", size=12, face="bold"),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=10, face="bold"))
ggsave("../out/nls_vs_sls_gma_SNR10.pdf", device="pdf")


# variances of parameters estimates
NLS_Lin_var <- c(var(d1$NLSest_gamma11)+var(d1$NLSest_gamma12)+var(d1$NLSest_gamma13)+var(d1$NLSest_gamma21)+var(d1$NLSest_gamma22)+var(d1$NLSest_gamma31)+var(d1$NLSest_gamma32),
                 var(d2$NLSest_gamma11)+var(d2$NLSest_gamma12)+var(d2$NLSest_gamma13)+var(d2$NLSest_gamma21)+var(d2$NLSest_gamma22)+var(d2$NLSest_gamma31)+var(d2$NLSest_gamma32),
                 var(d3$NLSest_gamma11)+var(d3$NLSest_gamma12)+var(d3$NLSest_gamma13)+var(d3$NLSest_gamma21)+var(d3$NLSest_gamma22)+var(d3$NLSest_gamma31)+var(d3$NLSest_gamma32),
                 var(d4$NLSest_gamma11)+var(d4$NLSest_gamma12)+var(d4$NLSest_gamma13)+var(d4$NLSest_gamma21)+var(d4$NLSest_gamma22)+var(d4$NLSest_gamma31)+var(d4$NLSest_gamma32))
SLS_Lin_var <- c(var(d1$SLSest_gamma11)+var(d1$SLSest_gamma12)+var(d1$SLSest_gamma13)+var(d1$SLSest_gamma21)+var(d1$SLSest_gamma22)+var(d1$SLSest_gamma31)+var(d1$SLSest_gamma32),
                 var(d2$SLSest_gamma11)+var(d2$SLSest_gamma12)+var(d2$SLSest_gamma13)+var(d2$SLSest_gamma21)+var(d2$SLSest_gamma22)+var(d2$SLSest_gamma31)+var(d2$SLSest_gamma32),
                 var(d3$SLSest_gamma11)+var(d3$SLSest_gamma12)+var(d3$SLSest_gamma13)+var(d3$SLSest_gamma21)+var(d3$SLSest_gamma22)+var(d3$SLSest_gamma31)+var(d3$SLSest_gamma32),
                 var(d4$SLSest_gamma11)+var(d4$SLSest_gamma12)+var(d4$SLSest_gamma13)+var(d4$SLSest_gamma21)+var(d4$SLSest_gamma22)+var(d4$SLSest_gamma31)+var(d4$SLSest_gamma32))
NLS_Nlin_var <- c(var(d1$NLSest_f121)+var(d1$NLSest_f131)+var(d1$NLSest_f112)+var(d1$NLSest_f122)+var(d1$NLSest_f113)+var(d1$NLSest_f133)+var(d1$NLSest_f211)+var(d1$NLSest_f221)+var(d1$NLSest_f222)+var(d1$NLSest_f311)+var(d1$NLSest_f331)+var(d1$NLSest_f332),
                  var(d2$NLSest_f121)+var(d2$NLSest_f131)+var(d2$NLSest_f112)+var(d2$NLSest_f122)+var(d2$NLSest_f113)+var(d2$NLSest_f133)+var(d2$NLSest_f211)+var(d2$NLSest_f221)+var(d2$NLSest_f222)+var(d2$NLSest_f311)+var(d2$NLSest_f331)+var(d2$NLSest_f332),
                  var(d3$NLSest_f121)+var(d3$NLSest_f131)+var(d3$NLSest_f112)+var(d3$NLSest_f122)+var(d3$NLSest_f113)+var(d3$NLSest_f133)+var(d3$NLSest_f211)+var(d3$NLSest_f221)+var(d3$NLSest_f222)+var(d3$NLSest_f311)+var(d3$NLSest_f331)+var(d3$NLSest_f332),
                  var(d4$NLSest_f121)+var(d4$NLSest_f131)+var(d4$NLSest_f112)+var(d4$NLSest_f122)+var(d4$NLSest_f113)+var(d4$NLSest_f133)+var(d4$NLSest_f211)+var(d4$NLSest_f221)+var(d4$NLSest_f222)+var(d4$NLSest_f311)+var(d4$NLSest_f331)+var(d4$NLSest_f332))
SLS_Nlin_var <- c(var(d1$SLSest_f121)+var(d1$SLSest_f131)+var(d1$SLSest_f112)+var(d1$SLSest_f122)+var(d1$SLSest_f113)+var(d1$SLSest_f133)+var(d1$SLSest_f211)+var(d1$SLSest_f221)+var(d1$SLSest_f222)+var(d1$SLSest_f311)+var(d1$SLSest_f331)+var(d1$SLSest_f332),
                  var(d2$SLSest_f121)+var(d2$SLSest_f131)+var(d2$SLSest_f112)+var(d2$SLSest_f122)+var(d2$SLSest_f113)+var(d2$SLSest_f133)+var(d2$SLSest_f211)+var(d2$SLSest_f221)+var(d2$SLSest_f222)+var(d2$SLSest_f311)+var(d2$SLSest_f331)+var(d2$SLSest_f332),
                  var(d3$SLSest_f121)+var(d3$SLSest_f131)+var(d3$SLSest_f112)+var(d3$SLSest_f122)+var(d3$SLSest_f113)+var(d3$SLSest_f133)+var(d3$SLSest_f211)+var(d3$SLSest_f221)+var(d3$SLSest_f222)+var(d3$SLSest_f311)+var(d3$SLSest_f331)+var(d3$SLSest_f332),
                  var(d4$SLSest_f121)+var(d4$SLSest_f131)+var(d4$SLSest_f112)+var(d4$SLSest_f122)+var(d4$SLSest_f113)+var(d4$SLSest_f133)+var(d4$SLSest_f211)+var(d4$SLSest_f221)+var(d4$SLSest_f222)+var(d4$SLSest_f311)+var(d4$SLSest_f331)+var(d4$SLSest_f332))

# DFVar is a dataframe for showing the variance ratio of the parameter estimates
PriorInf <- rep(c("1","2","3","4"), 2)
VarRatio <- c(SLS_Lin_var / NLS_Lin_var, SLS_Nlin_var / NLS_Nlin_var)
Linearity <- factor(c(rep("Linear", 4), rep("Non-linear", 4)))
DFVar <- data.frame(PriorInf, Linearity, VarRatio)

# DVar is a dataframe for showing the variance of the parameter estimates
LVar <- c(NLS_Lin_var, SLS_Lin_var)
NVar <- c(NLS_Nlin_var, SLS_Nlin_var)
#SampleSize <- rep(c(100, 400, 900, 1600), 2)
Method <- c(rep("NLS", 4),rep("SLS",4))
DVar <- data.frame(Method, PriorInf, LVar, NVar)

ymax <- max(LVar, NVar)

col_lin <- ggplot(DVar, aes(x=PriorInf, y=LVar)) +
  geom_col(aes(fill=Method), position=position_dodge()) +
  scale_x_discrete(name="Quality of prior information", labels=c("1"="High", "2"="", "3"="", "4"="Low")) +
  scale_y_continuous(name="Variance") +
  coord_cartesian(ylim=c(0, ymax)) +
  labs(title="Variance of Linear parameter estimates",
       subtitle=expression(GMA~model,~SNR==10)) +
  theme(plot.title = element_text(hjust = 0.5, size=10), plot.subtitle = element_text(hjust = 0.5, size=9))

col_nlin <- ggplot(DVar, aes(x=PriorInf, y=NVar)) +
  geom_col(aes(fill=Method), position=position_dodge()) +
  scale_x_discrete(name="Quality of prior information", labels=c("1"="High", "2"="", "3"="", "4"="Low")) +
  scale_y_continuous(name="Variance") +
  coord_cartesian(ylim=c(0, ymax)) +
  labs(title="Variance of Nonlinear parameter estimates",
       subtitle=expression(GMA~model,~SNR==10)) +
  theme(plot.title = element_text(hjust = 0.5, size=10), plot.subtitle = element_text(hjust = 0.5, size=9))
ggsave("../out/variance_gma_SNR10.pdf", device="pdf",
       arrangeGrob(col_lin, col_nlin, ncol=2))


ggplot(DFVar, aes(x=PriorInf)) +
  geom_point(aes(y=VarRatio, colour=Linearity, shape=Linearity), size=4) +
  scale_colour_discrete(name="Parameter set") +
  scale_shape_discrete(name="Parameter set") +
  scale_x_discrete(name="Quality of prior information", labels=c("1"="High", "2"="", "3"="", "4"="Low")) +
  scale_y_continuous(name=expression(Variance~Ratio~SLS/NLS), limits=c(0,NA)) +
  ggtitle(expression("Ratio of variance of parameter estimates for GMA model"~SNR==10)) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("../out/variance_ratio_gma_SNR10.pdf", device="pdf")

ggplot(d1) +
  geom_histogram(aes(x=NLSest_gamma11, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.1) +
  scale_x_continuous(name=expression(Estimate~gamma[11])) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method") +
  geom_histogram(aes(x=SLSest_gamma11, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.1) +
  scale_x_continuous(name=expression(Estimate~gamma[11])) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method") +
  geom_vline(xintercept=theta['gamma11'])
  ggsave("../out/hist_gamma11_gma_SNR10.pdf", device="pdf")

ggplot(d1) +
  geom_histogram(aes(x=NLSest_gamma12, colour="NLS", fill="NLS"), alpha=0.5, binwidth=1) +
  scale_x_continuous(name=expression(Estimate~gamma[12])) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method") +
  geom_histogram(aes(x=SLSest_gamma12, colour="SLS", fill="SLS"), alpha=0.5, binwidth=1) +
  scale_x_continuous(name=expression(Estimate~gamma[12])) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method") +
  geom_vline(xintercept=theta['gamma12'])
ggsave("../out/hist_gamma12_gma_SNR10.pdf", device="pdf")

  
ggplot(d1) +
  geom_histogram(aes(x=NLSest_f112, colour="NLS", fill="NLS"), alpha=0.5, binwidth=0.1) +
  scale_x_continuous(name=expression(Estimate~f[112])) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method") +
  geom_histogram(aes(x=SLSest_f112, colour="SLS", fill="SLS"), alpha=0.5, binwidth=0.1) +
  scale_x_continuous(name=expression(Estimate~f[112])) +
  scale_fill_discrete(name="Method") +
  scale_colour_discrete(name="Method") +
  geom_vline(xintercept=theta[f112])
ggsave("../out/hist_f112_gma_SNR10.pdf", device="pdf")

nls_theta_names <- paste0("NLSest_", pars)
sls_theta_names <- paste0("SLSest_", pars)
theta_names <- c(nls_theta_names, sls_theta_names)

nls_all <- rbind(d1[,nls_theta_names], d2[,nls_theta_names], d3[,nls_theta_names], d4[,nls_theta_names])
sls_all <- rbind(d1[,sls_theta_names], d2[,sls_theta_names], d3[,sls_theta_names], d4[,sls_theta_names])
  
nls_theta_hat <- sapply(nls_all, mean)
names(nls_theta_hat) <- pars
sls_theta_hat <- sapply(sls_all, mean)
names(sls_theta_hat) <- pars
nls_50pct <- apply((abs((nls_all - theta) / theta) < 0.50), 2,  mean)
sls_50pct <- apply((abs((sls_all - theta) / theta) < 0.50), 2,  mean)
nls_20pct <- apply((abs((nls_all - theta) / theta) < 0.20), 2,  mean)
sls_20pct <- apply((abs((sls_all - theta) / theta) < 0.20), 2,  mean)

ParamsDF <- data.frame(Param=pars, Theta=theta, NLS_mean=nls_theta_hat, SLS_mean=sls_theta_hat, NLS_50pct=nls_50pct, SLS_50pct=sls_50pct, NLS_20pct=nls_20pct, SLS_20pct=sls_20pct, row.names=NULL)




