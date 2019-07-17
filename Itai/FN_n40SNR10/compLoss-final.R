rm(list=ls())
#library(latex2exp)
#library(ggplot2)
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")


pars <- c('a','b','c')
theta <- c(0.2, 0.2, 3)
names(theta) <- pars
lin_pars <- c('a', 'b')
nlin_pars <- 'c'


NLS_Lin_mse <- c(mean((d1$NLSest_a - theta['a'])^2)+mean((d1$NLSest_b - theta['b'])^2),
                 mean((d2$NLSest_a - theta['a'])^2)+mean((d2$NLSest_b - theta['b'])^2),
                 mean((d3$NLSest_a - theta['a'])^2)+mean((d3$NLSest_b - theta['b'])^2))

SLS_Lin_mse <- c(mean((d1$SLSest_a - theta['a'])^2)+mean((d1$SLSest_b - theta['b'])^2),
                 mean((d2$SLSest_a - theta['a'])^2)+mean((d2$SLSest_b - theta['b'])^2),
                 mean((d3$SLSest_a - theta['a'])^2)+mean((d3$SLSest_b - theta['b'])^2))

NLS_Nlin_mse <- c(mean((d1$NLSest_c - theta['c'])^2),mean((d2$NLSest_c - theta['c'])^2),mean((d3$NLSest_c - theta['c'])^2))

SLS_Nlin_mse <- c(mean((d1$SLSest_c - theta['c'])^2),mean((d2$SLSest_c - theta['c'])^2),mean((d3$SLSest_c - theta['c'])^2))

DF <- rbind(signif(NLS_Lin_mse / SLS_Lin_mse, 2), signif(NLS_Nlin_mse / SLS_Nlin_mse, 2))

DF <- cbind(c("Linear", "Nonlinear"), DF)
colnames(DF) <- c("Parameters", "high", "medium", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}
