rm(list=ls())

d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")

pars <- c('alpha1','g13','beta1','h11',
          'alpha2','g21','beta2','h22',
          'alpha3','g32','beta3','h33','h34',
          'alpha4','g41','beta4','h44')

lin_pars <- c('alpha1','beta1','alpha2','beta2',
              'alpha3','beta3','alpha4','beta4')
nlin_pars <- setdiff(pars,lin_pars)

theta <- c(12,-0.8,10,0.5,8,0.5,3,0.75,3,0.75,5,0.5,0.2,2,0.5,6,0.8)
names(theta) <- pars




NLS_Lin_mse <- c(mean((d1$NLSest_alpha1 - theta['alpha1'])^2)+mean((d1$NLSest_beta1 - theta['beta1'])^2)+
                 mean((d1$NLSest_alpha2 - theta['alpha2'])^2)+mean((d1$NLSest_beta2 - theta['beta2'])^2)+
                 mean((d1$NLSest_alpha3 - theta['alpha3'])^2)+mean((d1$NLSest_beta3 - theta['beta3'])^2)+
                 mean((d1$NLSest_alpha4 - theta['alpha4'])^2)+mean((d1$NLSest_beta4 - theta['beta4'])^2),
                 mean((d2$NLSest_alpha1 - theta['alpha1'])^2)+mean((d2$NLSest_beta1 - theta['beta1'])^2)+
                 mean((d2$NLSest_alpha2 - theta['alpha2'])^2)+mean((d2$NLSest_beta2 - theta['beta2'])^2)+
                 mean((d2$NLSest_alpha3 - theta['alpha3'])^2)+mean((d2$NLSest_beta3 - theta['beta3'])^2)+
                 mean((d2$NLSest_alpha4 - theta['alpha4'])^2)+mean((d2$NLSest_beta4 - theta['beta4'])^2),
                 mean((d3$NLSest_alpha1 - theta['alpha1'])^2)+mean((d3$NLSest_beta1 - theta['beta1'])^2)+
                 mean((d3$NLSest_alpha2 - theta['alpha2'])^2)+mean((d3$NLSest_beta2 - theta['beta2'])^2)+
                 mean((d3$NLSest_alpha3 - theta['alpha3'])^2)+mean((d3$NLSest_beta3 - theta['beta3'])^2)+
                 mean((d3$NLSest_alpha4 - theta['alpha4'])^2)+mean((d3$NLSest_beta4 - theta['beta4'])^2))

                 
SLS_Lin_mse <- c(mean((d1$SLSest_alpha1 - theta['alpha1'])^2)+mean((d1$SLSest_beta1 - theta['beta1'])^2)+
                 mean((d1$SLSest_alpha2 - theta['alpha2'])^2)+mean((d1$SLSest_beta2 - theta['beta2'])^2)+
                 mean((d1$SLSest_alpha3 - theta['alpha3'])^2)+mean((d1$SLSest_beta3 - theta['beta3'])^2)+
                 mean((d1$SLSest_alpha4 - theta['alpha4'])^2)+mean((d1$SLSest_beta4 - theta['beta4'])^2),
                 mean((d2$SLSest_alpha1 - theta['alpha1'])^2)+mean((d2$SLSest_beta1 - theta['beta1'])^2)+
                 mean((d2$SLSest_alpha2 - theta['alpha2'])^2)+mean((d2$SLSest_beta2 - theta['beta2'])^2)+
                 mean((d2$SLSest_alpha3 - theta['alpha3'])^2)+mean((d2$SLSest_beta3 - theta['beta3'])^2)+
                 mean((d2$SLSest_alpha4 - theta['alpha4'])^2)+mean((d2$SLSest_beta4 - theta['beta4'])^2),
                 mean((d3$SLSest_alpha1 - theta['alpha1'])^2)+mean((d3$SLSest_beta1 - theta['beta1'])^2)+
                 mean((d3$SLSest_alpha2 - theta['alpha2'])^2)+mean((d3$SLSest_beta2 - theta['beta2'])^2)+
                 mean((d3$SLSest_alpha3 - theta['alpha3'])^2)+mean((d3$SLSest_beta3 - theta['beta3'])^2)+
                 mean((d3$SLSest_alpha4 - theta['alpha4'])^2)+mean((d3$SLSest_beta4 - theta['beta4'])^2))




NLS_Nlin_mse <- c(mean((d1$NLSest_g13 - theta['g13'])^2)+mean((d1$NLSest_h11 - theta['h11'])^2)+
                  mean((d1$NLSest_g21 - theta['g21'])^2)+mean((d1$NLSest_h22 - theta['h22'])^2)+
                  mean((d1$NLSest_g32 - theta['g32'])^2)+mean((d1$NLSest_h33 - theta['h33'])^2)+
                  mean((d1$NLSest_h34 - theta['h34'])^2)+mean((d1$NLSest_g41 - theta['g41'])^2)+
                  mean((d1$NLSest_h44 - theta['h44'])^2),
                  mean((d2$NLSest_g13 - theta['g13'])^2)+mean((d2$NLSest_h11 - theta['h11'])^2)+
                  mean((d2$NLSest_g21 - theta['g21'])^2)+mean((d2$NLSest_h22 - theta['h22'])^2)+
                  mean((d2$NLSest_g32 - theta['g32'])^2)+mean((d2$NLSest_h33 - theta['h33'])^2)+
                  mean((d2$NLSest_h34 - theta['h34'])^2)+mean((d2$NLSest_g41 - theta['g41'])^2)+
                  mean((d2$NLSest_h44 - theta['h44'])^2),
                  mean((d3$NLSest_g13 - theta['g13'])^2)+mean((d3$NLSest_h11 - theta['h11'])^2)+
                  mean((d3$NLSest_g21 - theta['g21'])^2)+mean((d3$NLSest_h22 - theta['h22'])^2)+
                  mean((d3$NLSest_g32 - theta['g32'])^2)+mean((d3$NLSest_h33 - theta['h33'])^2)+
                  mean((d3$NLSest_h34 - theta['h34'])^2)+mean((d3$NLSest_g41 - theta['g41'])^2)+
                  mean((d3$NLSest_h44 - theta['h44'])^2))
                 
                

SLS_Nlin_mse <-  c(mean((d1$SLSest_g13 - theta['g13'])^2)+mean((d1$SLSest_h11 - theta['h11'])^2)+
                   mean((d1$SLSest_g21 - theta['g21'])^2)+mean((d1$SLSest_h22 - theta['h22'])^2)+
                   mean((d1$SLSest_g32 - theta['g32'])^2)+mean((d1$SLSest_h33 - theta['h33'])^2)+
                   mean((d1$SLSest_h34 - theta['h34'])^2)+mean((d1$SLSest_g41 - theta['g41'])^2)+
                   mean((d1$SLSest_h44 - theta['h44'])^2),
                   mean((d2$SLSest_g13 - theta['g13'])^2)+mean((d2$SLSest_h11 - theta['h11'])^2)+
                   mean((d2$SLSest_g21 - theta['g21'])^2)+mean((d2$SLSest_h22 - theta['h22'])^2)+
                   mean((d2$SLSest_g32 - theta['g32'])^2)+mean((d2$SLSest_h33 - theta['h33'])^2)+
                   mean((d2$SLSest_h34 - theta['h34'])^2)+mean((d2$SLSest_g41 - theta['g41'])^2)+
                   mean((d2$SLSest_h44 - theta['h44'])^2),
                   mean((d3$SLSest_g13 - theta['g13'])^2)+mean((d3$SLSest_h11 - theta['h11'])^2)+
                   mean((d3$SLSest_g21 - theta['g21'])^2)+mean((d3$SLSest_h22 - theta['h22'])^2)+
                   mean((d3$SLSest_g32 - theta['g32'])^2)+mean((d3$SLSest_h33 - theta['h33'])^2)+
                   mean((d3$SLSest_h34 - theta['h34'])^2)+mean((d3$SLSest_g41 - theta['g41'])^2)+
                   mean((d3$SLSest_h44 - theta['h44'])^2))
  
  
DF <- rbind(signif(NLS_Lin_mse / SLS_Lin_mse, 2), signif(NLS_Nlin_mse / SLS_Nlin_mse, 2))

DF <- cbind(c("Linear", "Nonlinear"), DF)

colnames(DF) <- c("Parameters", "high", "medium", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}

