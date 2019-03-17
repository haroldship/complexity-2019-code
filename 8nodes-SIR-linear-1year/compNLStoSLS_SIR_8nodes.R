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

#equations <- c(S1_1,S2_1,S1_2,S2_2,S1_3,S2_3,S1_4,S2_4,S1_5,S2_5,I1_1,I2_1,I1_2,I2_2,I1_3,I2_3,I1_4,I2_4,I1_5,I2_5)
#vars <- c("S1_1","S2_1","S1_2","S2_2","S1_3","S2_3","S1_4","S2_4","S1_5","S2_5","I1_1","I2_1","I1_2","I2_2","I1_3","I2_3","I1_4","I2_4","I1_5","I2_5")
equations <- c(S1_1, I1_1, S2_1, I2_1)
vars <- c("S1_1", "I1_1", "S2_1", "I2_1")
names(equations) <- vars

pars <- c('beta1_1','beta1_2','beta2_1','beta2_2', 'gamma')

theta <- c(6,2,1,3,2.333333)
names(theta) <- pars
x0 <- c(0.56,1e-4,0.57,1e-4)
names(x0) <- vars
sigma <- c(0.05, 0.0005, 0.05, 0.0005)
priorInf=c(0.1,1,3,5)

n <- 18
time <- 1:n
model_out <- solve_ode(equations,theta,x0,time)
plot(model_out)
x_det <- model_out[,vars]
lin_pars <- c('beta1_1','beta1_2','beta2_1','beta2_2','gamma')
nlin_pars <- setdiff(pars,lin_pars)

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
      NLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                      nlin_pars=nlin_pars, start=nlin_init, 
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im"))})
    ptimeSLS <- system.time({
      SLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
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

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
}
names(obs) <- vars
plot(time, unlist(obs[1]), ylab="", main="S1_1")
plot(time, unlist(obs[2]), ylab="", main="I1_1")
plot(time, unlist(obs[3]), ylab="", main="S2_1")
plot(time, unlist(obs[4]), ylab="", main="I2_1")

