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
#plot(model_out)

x_det <- model_out[,vars]
sigma <- 0.05

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
}
names(obs) <- vars

calc_nll_sig <- function(pars, time, obs, model_out, ...) {
  sigma <- pars['sigma']
  -sum(
    unlist(
      lapply(names(obs), function(var) {
        dnorm(obs[[var]], mean=model_out[,var], sd=sigma, log=TRUE)
      })
  ))
}

names(sigma) <- 'sigma'
lik_pars <- names(sigma)
pars_fn_sigma <- c(pars, lik_pars)

nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                   + priorInf[1]*theta[nlin_pars])
names(nlin_init) <- nlin_pars
nlin_init[names(sigma)] <- 0.3
lower <- NULL
lower[names(sigma)] <- 0

estNS <- simode(equations=equations, pars=pars_fn_sigma, time=time, obs=obs,
              fixed=x0, nlin=nlin_pars, likelihood_pars=lik_pars,
              start=nlin_init, lower=lower,
              calc_nll=calc_nll_sig,
              im_method = "non-separable",
              simode_ctrl=simode.control(optim_type = "im"))
summary(estNS)
#plot(estNS, type='fit', pars_true=theta, mfrow=c(2,1), legend=TRUE)
estS <- simode(equations=equations, pars=pars_fn_sigma, time=time, obs=obs,
              fixed=x0, nlin=nlin_pars, likelihood_pars=lik_pars,
              start=nlin_init, lower=lower,
              calc_nll=calc_nll_sig,
              simode_ctrl=simode.control(optim_type = "im"))
summary(estS)
#plot(estS, type='fit', pars_true=theta, mfrow=c(2,1), legend=TRUE)

N <- 50
set.seed(1000)
library(doRNG)
require(doParallel)
registerDoParallel(cores=8)

args <- c('equations', 'pars', 'time', 'x0', 'theta',
          'Svars', 'x_det', 'vars', 'sigma')

for(ip in 1:4){
  
  results <- foreach(j=1:N, .packages='simode', .export=args) %dorng% {
    # for(j in 1:N) {
    obs <- list()
    for(i in 1:length(vars)) {
      obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
    }
    names(obs) <- vars
    
    nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                       + priorInf[1]*theta[nlin_pars])
    names(nlin_init) <- nlin_pars
    nlin_init[names(sigma)] <- rnorm(1,sigma,
                                     + priorInf[1]*sigma)
    lower <- NULL
    lower[names(sigma)] <- 0
    
    ptimeNLS <- system.time({
      NLSmc <- simode(equations=equations, pars=pars_fn_sigma, time=time, obs=obs,
                      fixed=x0, nlin=nlin_pars, likelihood_pars=lik_pars,
                      start=nlin_init, lower=lower,
                      calc_nll=calc_nll_sig,
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im"))})
    ptimeSLS <- system.time({
      SLSmc <- simode(equations=equations, pars=pars_fn_sigma, time=time, obs=obs,
                      fixed=x0, nlin=nlin_pars, likelihood_pars=lik_pars,
                      start=nlin_init, lower=lower,
                      calc_nll=calc_nll_sig, 
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


