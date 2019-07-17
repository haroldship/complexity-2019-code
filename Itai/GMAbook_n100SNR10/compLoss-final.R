rm(list=ls())
#library(latex2exp)
#library(ggplot2)
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")


dAll<-data.frame(NLS1=d1$NLSmc,SLS1=d1$SLSmc,
                 NLS2=d2$NLSmc,SLS2=d2$SLSmc,
                 NLS3=d3$NLSmc,SLS3=d3$SLSmc)

# variances of parameters estimates
NLS_Lin_var <- c(var(d1$NLSest_gamma11)+var(d1$NLSest_gamma12)+var(d1$NLSest_gamma13)+var(d1$NLSest_gamma22)+var(d1$NLSest_gamma32),
                 var(d2$NLSest_gamma11)+var(d2$NLSest_gamma12)+var(d2$NLSest_gamma13)+var(d2$NLSest_gamma22)+var(d2$NLSest_gamma32),
                 var(d3$NLSest_gamma11)+var(d3$NLSest_gamma12)+var(d3$NLSest_gamma13)+var(d3$NLSest_gamma22)+var(d3$NLSest_gamma32))

SLS_Lin_var <- c(var(d1$SLSest_gamma11)+var(d1$SLSest_gamma12)+var(d1$SLSest_gamma13)+var(d1$SLSest_gamma22)+var(d1$SLSest_gamma32),
                 var(d2$SLSest_gamma11)+var(d2$SLSest_gamma12)+var(d2$SLSest_gamma13)+var(d2$SLSest_gamma22)+var(d2$SLSest_gamma32),
                 var(d3$SLSest_gamma11)+var(d3$SLSest_gamma12)+var(d3$SLSest_gamma13)+var(d3$SLSest_gamma22)+var(d3$SLSest_gamma32))

NLS_Nlin_var <- c(var(d1$NLSest_f121)+var(d1$NLSest_f131)+var(d1$NLSest_f112)+var(d1$NLSest_f122)+var(d1$NLSest_f113)+var(d1$NLSest_f133)+var(d1$NLSest_f222)+var(d1$NLSest_f332),
                  var(d2$NLSest_f121)+var(d2$NLSest_f131)+var(d2$NLSest_f112)+var(d2$NLSest_f122)+var(d2$NLSest_f113)+var(d2$NLSest_f133)+var(d2$NLSest_f222)+var(d2$NLSest_f332),
                  var(d3$NLSest_f121)+var(d3$NLSest_f131)+var(d3$NLSest_f112)+var(d3$NLSest_f122)+var(d3$NLSest_f113)+var(d3$NLSest_f133)+var(d3$NLSest_f222)+var(d3$NLSest_f332))

SLS_Nlin_var <- c(var(d1$SLSest_f121)+var(d1$SLSest_f131)+var(d1$SLSest_f112)+var(d1$SLSest_f122)+var(d1$SLSest_f113)+var(d1$SLSest_f133)+var(d1$SLSest_f222)+var(d1$SLSest_f332),
                  var(d2$SLSest_f121)+var(d2$SLSest_f131)+var(d2$SLSest_f112)+var(d2$SLSest_f122)+var(d2$SLSest_f113)+var(d2$SLSest_f133)+var(d2$SLSest_f222)+var(d2$SLSest_f332),
                  var(d3$SLSest_f121)+var(d3$SLSest_f131)+var(d3$SLSest_f112)+var(d3$SLSest_f122)+var(d3$SLSest_f113)+var(d3$SLSest_f133)+var(d3$SLSest_f222)+var(d3$SLSest_f332))


DF <- rbind(signif(NLS_Lin_var / SLS_Lin_var, 2), signif(NLS_Nlin_var / SLS_Nlin_var, 2))
DF <- cbind(c("Linear", "Nonlinear"), DF)
colnames(DF) <- c("Parameters", "high", "medium", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}
