rm(list=ls())
#library(latex2exp)
#library(ggplot2)
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")
#d1=d10[1:49,]
#d2=d20[1:49,]



dAll<-data.frame(NLS1=d1$NLSmc,SLS1=d1$SLSmc,
                 NLS2=d2$NLSmc,SLS2=d2$SLSmc,
                 NLS3=d3$NLSmc,SLS3=d3$SLSmc)

NLS_Lin_var <- c(var(d1$NLSest_alpha)+var(d1$NLSest_beta)+var(d1$NLSest_gamma)+var(d1$NLSest_delta),
                 var(d2$NLSest_alpha)+var(d2$NLSest_beta)+var(d2$NLSest_gamma)+var(d2$NLSest_delta),
                 var(d3$NLSest_alpha)+var(d3$NLSest_beta)+var(d3$NLSest_gamma)+var(d3$NLSest_delta))
SLS_Lin_var <- c(var(d1$SLSest_alpha)+var(d1$SLSest_beta)+var(d1$SLSest_gamma)+var(d1$SLSest_delta),
                 var(d2$SLSest_alpha)+var(d2$SLSest_beta)+var(d2$SLSest_gamma)+var(d2$SLSest_delta),
                 var(d3$SLSest_alpha)+var(d3$SLSest_beta)+var(d3$SLSest_gamma)+var(d3$SLSest_delta))
NLS_Nlin_var <- c(var(d1$NLSest_epsilon)+var(d1$NLSest_omega),
                  var(d2$NLSest_epsilon)+var(d2$NLSest_omega),
                  var(d3$NLSest_epsilon)+var(d3$NLSest_omega))
SLS_Nlin_var <- c(var(d1$SLSest_epsilon)+var(d1$SLSest_omega),
                  var(d2$SLSest_epsilon)+var(d2$SLSest_omega),
                  var(d3$SLSest_epsilon)+var(d3$SLSest_omega))

DF <- rbind(signif(NLS_Lin_var / SLS_Lin_var, 2), signif(NLS_Nlin_var / SLS_Nlin_var, 2))
DF <- cbind(c("Linear", "Nonlinear"), DF)
colnames(DF) <- c("Parameters", "high", "medium", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}
