rm(list=ls())
d1 <- read.csv("1-NLStoSLSloss.csv")
d2 <- read.csv("2-NLStoSLSloss.csv")
d3 <- read.csv("3-NLStoSLSloss.csv")

n <- 36
SNR <- 5

S1_1 <- "-S1_1*(beta1_1*I1_1)"
I1_1 <- "S1_1*(beta1_1*I1_1)-gamma*I1_1"

S0 <- c(0.56)
Svars <- c("S1_1")
names(S0) <- Svars
I0 <- c(1e-04)
Ivars <- c("I1_1")
names(I0) <- Ivars
equations <- c(S1_1,I1_1)
vars <- c(Svars,Ivars)
names(equations) <- vars

beta <- c(6)
names(beta) <- c('beta1_1')
gamma <- 2.333333
names(gamma) <- 'gamma'

theta <- c(beta, gamma, S0)
names(theta) <- c('beta1_1', "gamma", "S1_1")

time <- 1:n

x0 <- c(S0,I0)
names(x0) <- vars

# variances of parameters estimates
NLS_Lin_mse <- c(mean((d1$NLSest_beta1_1 - theta['beta1_1'])^2),
                 mean((d2$NLSest_beta1_1 - theta['beta1_1'])^2),
                 mean((d3$NLSest_beta1_1 - theta['beta1_1'])^2))
SLS_Lin_mse <- c(mean((d1$SLSest_beta1_1 - theta['beta1_1'])^2),
                 mean((d2$SLSest_beta1_1 - theta['beta1_1'])^2),
                 mean((d3$SLSest_beta1_1 - theta['beta1_1'])^2))
NLS_Nlin_mse <- c(mean((d1$NLSest_S1_1 - theta['S1_1'])^2),
                 mean((d2$NLSest_S1_1 - theta['S1_1'])^2),
                 mean((d3$NLSest_S1_1 - theta['S1_1'])^2))
SLS_Nlin_mse <- c(mean((d1$SLSest_S1_1 - theta['S1_1'])^2),
                 mean((d2$SLSest_S1_1 - theta['S1_1'])^2),
                 mean((d3$SLSest_S1_1 - theta['S1_1'])^2))

DF <- rbind(signif(NLS_Lin_mse / SLS_Lin_mse, 2), signif(NLS_Nlin_mse / SLS_Nlin_mse, 2))
DF <- cbind(c("Linear", "Nonlinear"), DF)
colnames(DF) <- c("Parameters", "high", "medium", "low")
rownames(DF) <- c("","")

for (j in 1:2) {
  cat(paste0(paste(DF[j,], collapse=" & "), " \\\\\n"))
}

