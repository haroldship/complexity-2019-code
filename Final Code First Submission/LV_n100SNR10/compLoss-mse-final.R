rm(list=ls())
#library(latex2exp)
#library(ggplot2)
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")
#d1=d10[1:49,]
#d2=d20[1:49,]


pars <- c('alpha','beta','gamma','delta','epsilon','omega')
lin_pars <- c('alpha','beta','gamma','delta')
nlin_pars <- setdiff(pars,lin_pars)
theta <- c(2/3, 4/3, 1, 1, 0.2, 0.5)
names(theta) <- pars

dAll<-data.frame(NLS1=d1$NLSmc,SLS1=d1$SLSmc,
                 NLS2=d2$NLSmc,SLS2=d2$SLSmc,
                 NLS3=d3$NLSmc,SLS3=d3$SLSmc)

NLS_Lin_mse <- c(mean((d1$NLSest_alpha - theta['alpha'])^2)+mean((d1$NLSest_beta - theta['beta'])^2)+mean((d1$NLSest_gamma - theta['gamma'])^2)+mean((d1$NLSest_delta - theta['delta'])^2),
                 mean((d2$NLSest_alpha - theta['alpha'])^2)+mean((d2$NLSest_beta - theta['beta'])^2)+mean((d2$NLSest_gamma - theta['gamma'])^2)+mean((d2$NLSest_delta - theta['delta'])^2),
                 mean((d3$NLSest_alpha - theta['alpha'])^2)+mean((d3$NLSest_beta - theta['beta'])^2)+mean((d3$NLSest_gamma - theta['gamma'])^2)+mean((d3$NLSest_delta - theta['delta'])^2))

SLS_Lin_mse <- c(mean((d1$SLSest_alpha - theta['alpha'])^2)+mean((d1$SLSest_beta - theta['beta'])^2)+mean((d1$SLSest_gamma - theta['gamma'])^2)+mean((d1$SLSest_delta - theta['delta'])^2),
                 mean((d2$SLSest_alpha - theta['alpha'])^2)+mean((d2$SLSest_beta - theta['beta'])^2)+mean((d2$SLSest_gamma - theta['gamma'])^2)+mean((d2$SLSest_delta - theta['delta'])^2),
                 mean((d3$SLSest_alpha - theta['alpha'])^2)+mean((d3$SLSest_beta - theta['beta'])^2)+mean((d3$SLSest_gamma - theta['gamma'])^2)+mean((d3$SLSest_delta - theta['delta'])^2))

NLS_Nlin_mse <- c(mean((d1$NLSest_epsilon - theta['epsilon'])^2),mean((d2$NLSest_epsilon - theta['epsilon'])^2),mean((d3$NLSest_epsilon - theta['epsilon'])^2))

SLS_Nlin_mse <- c(mean((d1$SLSest_epsilon - theta['epsilon'])^2),mean((d2$SLSest_epsilon - theta['epsilon'])^2),mean((d3$SLSest_epsilon - theta['epsilon'])^2))

DF <- rbind(signif(NLS_Lin_mse / SLS_Lin_mse, 2), signif(NLS_Nlin_mse / SLS_Nlin_mse, 2))

DF <- cbind(c("Linear", "Nonlinear"), DF)

colnames(DF) <- c("Parameters", "high", "medium", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}
