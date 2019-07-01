rm(list=ls())
#library(latex2exp)
#library(ggplot2)
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")
d4 <- read.csv("4-NLStoSLSloss.csv")

pars <- c('a','b','c')
theta <- c(0.2, 0.2, 3)
names(theta) <- pars
lin_pars <- c('a', 'b')
nlin_pars <- 'c'

dAll<-data.frame(NLS1=d1$NLSmc,SLS1=d1$SLSmc,
                 NLS2=d2$NLSmc,SLS2=d2$SLSmc,
                 NLS3=d3$NLSmc,SLS3=d3$SLSmc,
                 NLS4=d4$NLSmc,SLS4=d4$SLSmc)

# variances of parameters estimates
NLS_Lin_var <- c(var(d1$NLSest_beta1_1),
                 var(d2$NLSest_beta1_1),
                 var(d3$NLSest_beta1_1),
                 var(d4$NLSest_beta1_1))
SLS_Lin_var <- c(var(d1$SLSest_beta1_1),
                 var(d2$SLSest_beta1_1),
                 var(d3$SLSest_beta1_1),
                 var(d4$SLSest_beta1_1))
NLS_Nlin_var <- c(var(d1$NLSest_S1_1),
                  var(d2$NLSest_S1_1),
                  var(d3$NLSest_S1_1),
                  var(d4$NLSest_S1_1))
SLS_Nlin_var <- c(var(d1$SLSest_S1_1),
                  var(d2$SLSest_S1_1),
                  var(d3$SLSest_S1_1),
                  var(d4$SLSest_S1_1))

DF <- rbind(signif(NLS_Lin_var / SLS_Lin_var, 2), signif(NLS_Nlin_var / SLS_Nlin_var, 2))
DF <- cbind(c("Linear", "Nonlinear"), DF)
colnames(DF) <- c("Parameters", "high", "good", "reasonable", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}
