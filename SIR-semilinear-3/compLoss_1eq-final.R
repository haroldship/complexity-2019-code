rm(list=ls())
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")
d4 <- read.csv("4-NLStoSLSloss.csv")

beta <- c(6,2,1,3)
names(beta) <- c('beta1_1','beta1_2','beta2_1','beta2_2')
gamma <- 2.333333
names(gamma) <- "gamma"
kappa <- c(1.182)
names(kappa) <- c("kappa2")
theta <- c(beta, gamma, kappa)
lin_pars <- names(beta)
nlin_pars <- names(kappa)

dAll<-data.frame(NLS1=d1$NLSmc,SLS1=d1$SLSmc,
                 NLS2=d2$NLSmc,SLS2=d2$SLSmc,
                 NLS3=d3$NLSmc,SLS3=d3$SLSmc,
                 NLS4=d4$NLSmc,SLS4=d4$SLSmc)


# MSE of parameters estimates
NLS_Lin_mse <- c(mean((d1$NLSest_beta1_1 - theta['beta1_1'])^2)+mean((d1$NLSest_beta2_1 - theta['beta2_1'])^2)+mean((d1$NLSest_beta1_2 - theta['beta1_2'])^2)+mean((d1$NLSest_beta2_2 - theta['beta2_2'])^2),
                 mean((d2$NLSest_beta1_1 - theta['beta1_1'])^2)+mean((d2$NLSest_beta2_1 - theta['beta2_1'])^2)+mean((d2$NLSest_beta1_2 - theta['beta1_2'])^2)+mean((d2$NLSest_beta2_2 - theta['beta2_2'])^2),
                 mean((d3$NLSest_beta1_1 - theta['beta1_1'])^2)+mean((d3$NLSest_beta2_1 - theta['beta2_1'])^2)+mean((d3$NLSest_beta1_2 - theta['beta1_2'])^2)+mean((d3$NLSest_beta2_2 - theta['beta2_2'])^2),
                 mean((d4$NLSest_beta1_1 - theta['beta1_1'])^2)+mean((d4$NLSest_beta2_1 - theta['beta2_1'])^2)+mean((d4$NLSest_beta1_2 - theta['beta1_2'])^2)+mean((d4$NLSest_beta2_2 - theta['beta2_2'])^2))
SLS_Lin_mse <- c(mean((d1$SLSest_beta1_1 - theta['beta1_1'])^2)+mean((d1$SLSest_beta2_1 - theta['beta2_1'])^2)+mean((d1$SLSest_beta1_2 - theta['beta1_2'])^2)+mean((d1$SLSest_beta2_2 - theta['beta2_2'])^2),
                 mean((d2$SLSest_beta1_1 - theta['beta1_1'])^2)+mean((d2$SLSest_beta2_1 - theta['beta2_1'])^2)+mean((d2$SLSest_beta1_2 - theta['beta1_2'])^2)+mean((d2$SLSest_beta2_2 - theta['beta2_2'])^2),
                 mean((d3$SLSest_beta1_1 - theta['beta1_1'])^2)+mean((d3$SLSest_beta2_1 - theta['beta2_1'])^2)+mean((d3$SLSest_beta1_2 - theta['beta1_2'])^2)+mean((d3$SLSest_beta2_2 - theta['beta2_2'])^2),
                 mean((d4$SLSest_beta1_1 - theta['beta1_1'])^2)+mean((d4$SLSest_beta2_1 - theta['beta2_1'])^2)+mean((d4$SLSest_beta1_2 - theta['beta1_2'])^2)+mean((d4$SLSest_beta2_2 - theta['beta2_2'])^2))
NLS_Nlin_mse <- c(mean((d1$NLSest_kappa2 - theta['kappa2'])^2),
                  mean((d2$NLSest_kappa2 - theta['kappa2'])^2),
                  mean((d3$NLSest_kappa2 - theta['kappa2'])^2),
                  mean((d4$SLSest_kappa2 - theta['kappa2'])^2))
SLS_Nlin_mse <- c(mean((d1$SLSest_kappa2 - theta['kappa2'])^2),
                  mean((d2$SLSest_kappa2 - theta['kappa2'])^2),
                  mean((d3$SLSest_kappa2 - theta['kappa2'])^2),
                  mean((d4$SLSest_kappa2 - theta['kappa2'])^2))

DF <- rbind(signif(NLS_Lin_mse / SLS_Lin_mse, 2), signif(NLS_Nlin_mse / SLS_Nlin_mse, 2))
DF <- cbind(c("Linear", "Nonlinear"), DF)
colnames(DF) <- c("Parameters", "high", "good", "reasonable", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}
