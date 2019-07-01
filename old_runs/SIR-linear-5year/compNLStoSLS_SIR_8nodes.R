rm(list=ls())
library(simode)
set.seed(1000)

S1_1 <- "-S1_1*(beta1_1*I1_1+beta1_2*I2_1)"
S2_1 <- "-S2_1*(beta2_1*I1_1+beta2_2*I2_1)"
S1_2 <- "-S1_2*kappa2*(beta1_1*I1_2+beta1_2*I2_2)"
S2_2 <- "-S2_2*kappa2*(beta2_1*I1_2+beta2_2*I2_2)"
S1_3 <- "-S1_3*kappa3*(beta1_1*I1_3+beta1_2*I2_3)"
S2_3 <- "-S2_3*kappa3*(beta2_1*I1_3+beta2_2*I2_3)"
S1_4 <- "-S1_4*kappa4*(beta1_1*I1_4+beta1_2*I2_4)"
S2_4 <- "-S2_4*kappa4*(beta2_1*I1_4+beta2_2*I2_4)"
S1_5 <- "-S1_5*kappa5*(beta1_1*I1_5+beta1_2*I2_5)"
S2_5 <- "-S2_5*kappa5*(beta2_1*I1_5+beta2_2*I2_5)"
I1_1 <- "S1_1*(beta1_1*I1_1+beta1_2*I2_1)-gamma*I1_1"
I2_1 <- "S2_1*(beta2_1*I1_1+beta2_2*I2_1)-gamma*I2_1"
I1_2 <- "S1_2*kappa2*(beta1_1*I1_2+beta1_2*I2_2)-gamma*I1_2"
I2_2 <- "S2_2*kappa2*(beta2_1*I1_2+beta2_2*I2_2)-gamma*I2_2"
I1_3 <- "S1_3*kappa3*(beta1_1*I1_3+beta1_2*I2_3)-gamma*I1_3"
I2_3 <- "S2_3*kappa3*(beta2_1*I1_3+beta2_2*I2_3)-gamma*I2_3"
I1_4 <- "S1_4*kappa4*(beta1_1*I1_4+beta1_2*I2_4)-gamma*I1_4"
I2_4 <- "S2_4*kappa4*(beta2_1*I1_4+beta2_2*I2_4)-gamma*I2_4"
I1_5 <- "S1_5*kappa5*(beta1_1*I1_5+beta1_2*I2_5)-gamma*I1_5"
I2_5 <- "S2_5*kappa5*(beta2_1*I1_5+beta2_2*I2_5)-gamma*I2_5"

S0 <- c(0.56,0.57,0.49,0.45,0.56,0.32,0.56,0.47,0.47,0.41)
Svars <- c("S1_1","S2_1","S1_2","S2_2","S1_3","S2_3","S1_4","S2_4","S1_5","S2_5")
names(S0) <- Svars
I0 <- c(1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04)
Ivars <- c("I1_1","I2_1","I1_2","I2_2","I1_3","I2_3","I1_4","I2_4","I1_5","I2_5")
names(I0) <- Ivars
equations <- c(S1_1,S2_1,S1_2,S2_2,S1_3,S2_3,S1_4,S2_4,S1_5,S2_5,I1_1,I2_1,I1_2,I2_2,I1_3,I2_3,I1_4,I2_4,I1_5,I2_5)
vars <- c(Svars,Ivars)
names(equations) <- vars

beta <- c(6,2,1,3)
names(beta) <- c('beta1_1','beta1_2','beta2_1','beta2_2')
gamma <- 2.333333
names(gamma) <- "gamma"
kappa <- c(0.988,1.182,1.037,1.052)
names(kappa) <- c("kappa2", "kappa3", "kappa4", "kappa5")
theta <- c(beta, gamma, kappa)
pars <- names(theta)
lin_pars <- c(names(beta), names(gamma))
nlin_pars <- setdiff(pars, lin_pars)

print(lin_pars)
print(nlin_pars)

n <- 18
time <- 1:n

x0 <- c(S0,I0)
names(x0) <- vars
sigma <- c(rep(0.05,length(Svars)), rep(0.001,length(Ivars)))
priorInf=c(0.1,1,3,5)

model_out <- solve_ode(equations,theta,x0,time)
#plot(model_out)
x_det <- model_out[,vars]

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
}
names(obs) <- vars
par(mfrow=c(2,2))
plot(time, unlist(obs[1]), ylab="", main="S1_1")
plot(time, unlist(obs[11]), ylab="", main="I1_1")
plot(time, unlist(obs[5]), ylab="", main="S2_3")
plot(time, unlist(obs[15]), ylab="", main="I2_3")

N <- 50
set.seed(1000)
library(doRNG)
require(doParallel)
registerDoParallel(cores=8)

args <- c('equations', 'pars', 'time', 'x0', 'theta',
          'nlin_pars', 'x_det', 'vars', 'sigma')

for(ip in 1:4){
  
  results <- foreach(j=1:N, .packages='simode', .export=args) %dorng% {
    # for(j in 1:N) {
    obs <- list()
    for(i in 1:length(vars)) {
      obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
    }
    names(obs) <- vars
    
    nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                       + priorInf[ip]*abs(theta[nlin_pars]))
    names(nlin_init) <- nlin_pars
    
    ptimeNLS <- system.time({
      NLSmc <- simode(equations=equations, pars=names(beta), fixed=c(x0,gamma,kappa), time=time, obs=obs,
                      nlin_pars=nlin_pars, start=nlin_init, 
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im"))})
    ptimeSLS <- system.time({
      SLSmc <- simode(equations=equations, pars=names(beta), fixed=c(x0,gamma,kappa), time=time, obs=obs,
                      nlin_pars=nlin_pars, start=nlin_init,
                      simode_ctrl=simode.control(optim_type = "im"))})
    
    list(NLSmc=NLSmc,SLSmc=SLSmc,ptimeNLS=ptimeNLS,ptimeSLS=ptimeSLS)
  }
  
  
  NLSmc_im_loss_vals <- sapply(results,function(x) x$NLSmc$im_loss)
  SLSmc_im_loss_vals <- sapply(results,function(x) x$SLSmc$im_loss)
  NLSmc_time=list()
  SLSmc_time=list()
  for (mc in 1:N){
    NLSmc_time[mc]<-  results[[mc]]$ptimeNLS[3]
    SLSmc_time[mc]<-  results[[mc]]$ptimeSLS[3]
  }
  #mean(unlist(NLSmc_im_loss_vals))
  #mean(unlist(SLSmc_im_loss_vals))
  #mean(unlist(NLSmc_time))
  #mean(unlist(SLSmc_time))
  
  loss_df=data.frame(NLSmc=unlist(NLSmc_im_loss_vals),SLSmc=unlist(SLSmc_im_loss_vals))
  time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
  write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
  write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}
#plot(unlist(NLSmc_im_loss_vals),type='l')
#lines(unlist(SLSmc_im_loss_vals),col="red")

