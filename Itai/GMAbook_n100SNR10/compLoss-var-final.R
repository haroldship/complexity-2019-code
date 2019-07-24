rm(list=ls())

d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")

pars1 <- c('gamma11','f121','f131','gamma12','f112','f122','gamma13','f113','f133')
pars2 <- c('gamma22','f222')
pars3 <- c('gamma32','f332')
pars <- c(pars1,pars2,pars3)
lin_pars <- c('gamma11','gamma12','gamma13','gamma22','gamma32')
nlin_pars <- setdiff(pars,lin_pars)

theta1 <- c(0.4,-1,-1,3,0.5,-0.1,2,0.75,-0.2)
theta2 <- c(1.5,0.5)
theta3 <- c(5,0.5)
theta <- c(theta1,theta2,theta3)
names(theta) <- pars





NLS_Lin_mse <- c(var(d1$NLSest_gamma11)+var(d1$NLSest_gamma12)
                +var(d1$NLSest_gamma13)+var(d1$NLSest_gamma22)
                +var(d1$NLSest_gamma32),
                var(d2$NLSest_gamma11)+var(d2$NLSest_gamma12)
                +var(d2$NLSest_gamma13)+var(d2$NLSest_gamma22)
                +var(d2$NLSest_gamma32),
                var(d3$NLSest_gamma11)+var(d3$NLSest_gamma12)
                +var(d3$NLSest_gamma13)+var(d3$NLSest_gamma22)
                +var(d3$NLSest_gamma32))

SLS_Lin_mse <- c(var(d1$SLSest_gamma11)+var(d1$SLSest_gamma12)
                +var(d1$SLSest_gamma13)+var(d1$SLSest_gamma22)
                +var(d1$SLSest_gamma32),
                var(d2$SLSest_gamma11)+var(d2$SLSest_gamma12)
                +var(d2$SLSest_gamma13)+var(d2$SLSest_gamma22)
                +var(d2$SLSest_gamma32),
                var(d3$SLSest_gamma11)+var(d3$SLSest_gamma12)
                +var(d3$SLSest_gamma13)+var(d3$SLSest_gamma22)
                +var(d3$SLSest_gamma32))

NLS_Nlin_mse <- c(mean((d1$NLSest_f121 - theta['f121']))+mean((d1$NLSest_f131 - theta['f131']))
                 +mean((d1$NLSest_f112 - theta['f112']))+mean((d1$NLSest_f122 - theta['f122']))
                 +mean((d1$NLSest_f113 - theta['f113']))+mean((d1$NLSest_f133 - theta['f133']))
                 +mean((d1$NLSest_f222 - theta['f222']))+mean((d1$NLSest_f332 - theta['f332'])),
                  mean((d2$NLSest_f121 - theta['f121']))+mean((d2$NLSest_f131 - theta['f131']))
                 +mean((d2$NLSest_f112 - theta['f112']))+mean((d2$NLSest_f122 - theta['f122']))
                 +mean((d2$NLSest_f113 - theta['f113']))+mean((d2$NLSest_f133 - theta['f133']))
                 +mean((d2$NLSest_f222 - theta['f222']))+mean((d2$NLSest_f332 - theta['f332'])),
                  mean((d3$NLSest_f121 - theta['f121']))+mean((d3$NLSest_f131 - theta['f131']))
                 +mean((d3$NLSest_f112 - theta['f112']))+mean((d3$NLSest_f122 - theta['f122']))
                 +mean((d3$NLSest_f113 - theta['f113']))+mean((d3$NLSest_f133 - theta['f133']))
                 +mean((d3$NLSest_f222 - theta['f222']))+mean((d3$NLSest_f332 - theta['f332'])))

SLS_Nlin_mse <- c(mean((d1$SLSest_f121 - theta['f121'])^2)+mean((d1$SLSest_f131 - theta['f131'])^2)
                 +mean((d1$SLSest_f112 - theta['f112'])^2)+mean((d1$SLSest_f122 - theta['f122'])^2)
                 +mean((d1$SLSest_f113 - theta['f113'])^2)+mean((d1$SLSest_f133 - theta['f133'])^2)
                 +mean((d1$SLSest_f222 - theta['f222'])^2)+mean((d1$SLSest_f332 - theta['f332'])^2),
                  mean((d2$SLSest_f121 - theta['f121'])^2)+mean((d2$SLSest_f131 - theta['f131'])^2)
                 +mean((d2$SLSest_f112 - theta['f112'])^2)+mean((d2$SLSest_f122 - theta['f122'])^2)
                 +mean((d2$SLSest_f113 - theta['f113'])^2)+mean((d2$SLSest_f133 - theta['f133'])^2)
                 +mean((d2$SLSest_f222 - theta['f222'])^2)+mean((d2$SLSest_f332 - theta['f332'])^2),
                  mean((d3$SLSest_f121 - theta['f121'])^2)+mean((d3$SLSest_f131 - theta['f131'])^2)
                 +mean((d3$SLSest_f112 - theta['f112'])^2)+mean((d3$SLSest_f122 - theta['f122'])^2)
                 +mean((d3$SLSest_f113 - theta['f113'])^2)+mean((d3$SLSest_f133 - theta['f133'])^2)
                 +mean((d3$SLSest_f222 - theta['f222'])^2)+mean((d3$SLSest_f332 - theta['f332'])^2))

  
DF <- rbind(signif(NLS_Lin_mse / SLS_Lin_mse, 2), signif(NLS_Nlin_mse / SLS_Nlin_mse, 2))

DF <- cbind(c("Linear", "Nonlinear"), DF)

colnames(DF) <- c("Parameters", "high", "medium", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}
