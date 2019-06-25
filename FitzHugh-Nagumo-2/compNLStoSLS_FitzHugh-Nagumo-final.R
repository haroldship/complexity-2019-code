rm(list=ls())
library(simode)
set.seed(1000)

V <- 'c*(V-V^3/3+R)'
R <- '-(V - a + b*R)/c'
vars <- c('V','R')
equations <- c(V,R)
names(equations) <- vars

pars <- c('a','b','c')
theta <- c(0.2, 0.2, 3)
names(theta) <- pars
lin_pars <- c('a', 'b')
nlin_pars <- 'c'

n <- 40
time <- seq(0, 20, length.out=n)

x0 <- c(-1, 1)
names(x0) <- vars

priorInf=c(0.1,1,3,5)
model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]
plot(model_out)

SNR <- 5
sigma_x <- apply(x_det, 2, sd)
sigma <- signif(sigma_x / SNR, digits=2)
print(sigma)

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
}
names(obs) <- vars


nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                   + priorInf[1]*theta[nlin_pars])
names(nlin_init) <- nlin_pars

estNS <- simode(equations=equations, pars=pars, time=time, obs=obs,
              fixed=x0, nlin=nlin_pars,
              start=nlin_init, 
              im_method = "non-separable",
              simode_ctrl=simode.control(optim_type = "im"))
#summary(estNS)
#plot(estNS, type='fit', pars_true=theta, mfrow=c(2,1), legend=TRUE)
estS <- simode(equations=equations, pars=pars, time=time, obs=obs,
                fixed=x0, nlin=nlin_pars,
                start=nlin_init, 
                simode_ctrl=simode.control(optim_type = "im"))
#summary(estS)
#plot(estS, type='fit', pars_true=theta, mfrow=c(2,1), legend=TRUE)

N <- 500
set.seed(1000)
library(doRNG)
require(doParallel)
registerDoParallel(cores=16)

args <- c('equations', 'pars', 'time', 'x0', 'theta', 'obs',
          'vars', 'x_det', 'vars', 'sigma')

for(ip in 1:length(priorInf)){
  
  results <- foreach(j=1:N, .packages='simode', .export=args) %dorng% {
    # for(j in 1:N) {
    obs <- list()
    for(i in 1:length(vars)) {
      obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
    }
    names(obs) <- vars
    
    nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                       + priorInf[ip]*theta[nlin_pars])
    names(nlin_init) <- nlin_pars

    ptimeNLS <- system.time({
      NLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs,
                      fixed=x0, nlin=nlin_pars,
                      start=nlin_init,
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im"))})
    ptimeSLS <- system.time({
      SLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs,
                      fixed=x0, nlin=nlin_pars,
                      start=nlin_init,
                      simode_ctrl=simode.control(optim_type = "im"))})
    
    list(NLSmc=NLSmc,SLSmc=SLSmc,ptimeNLS=ptimeNLS,ptimeSLS=ptimeSLS)
  }
  
  
  NLSmc_im_loss_vals <- sapply(results,function(x) x$NLSmc$im_loss)
  SLSmc_im_loss_vals <- sapply(results,function(x) x$SLSmc$im_loss)
  NLS_im_vars=sapply(results,function(x) x$NLSmc$im_pars_est)
  SLS_im_vars=sapply(results,function(x) x$SLSmc$im_pars_est)
  NLSmc_time=list()
  SLSmc_time=list()
  NLSmc_sse=list()
  SLSmc_sse=list()
  for (mc in 1:N){
    NLSmc_time[mc]<-  results[[mc]]$ptimeNLS[3]
    SLSmc_time[mc]<-  results[[mc]]$ptimeSLS[3]
  }
  #mean(unlist(NLSmc_im_loss_vals))
  #mean(unlist(SLSmc_im_loss_vals))
  #mean(unlist(NLSmc_time))
  #mean(unlist(SLSmc_time))

  loss_df=data.frame(NLSmc=unlist(NLSmc_im_loss_vals),SLSmc=unlist(SLSmc_im_loss_vals),
                     NLSest_a=NLS_im_vars['a',],NLSest_b=NLS_im_vars['b',],NLSest_c=NLS_im_vars['c',],
                     SLSest_a=SLS_im_vars['a',],SLSest_b=SLS_im_vars['b',],SLSest_c=SLS_im_vars['c',])
  time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
  write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
  write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}
#plot(unlist(NLSmc_im_loss_vals),type='l')
#lines(unlist(SLSmc_im_loss_vals),col="red")


