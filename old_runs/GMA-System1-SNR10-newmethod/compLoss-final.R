rm(list=ls())
#library(latex2exp)
#library(ggplot2)
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")
d4 <- read.csv("4-NLStoSLSloss.csv")
#d1=d10[1:49,]
#d2=d20[1:49,]

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


DF <- rbind(signif(NLS_Lin_var / SLS_Lin_var, 2), signif(NLS_Nlin_var / SLS_Nlin_var, 2))
DF <- cbind(c("Linear", "Nonlinear"), DF)
colnames(DF) <- c("Parameters", "high", "good", "reasonable", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}
