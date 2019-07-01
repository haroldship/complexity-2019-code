rm(list=ls())

pars <- c('alpha','beta','gamma','delta','epsilon','omega')
nlin_pars <- c('epsilon','omega')
vars <- c('X','Y')
eq_X <- 'alpha*X-beta*(1+epsilon*sin(2*pi*(t/50+omega)))*X*Y'
eq_Y <- 'delta*(1+epsilon*sin(2*pi*(t/50+omega)))*X*Y-gamma*Y'
equations <- c(eq_X,eq_Y)
names(equations) <- vars
theta <- c(2/3, 4/3, 1, 1, 0.2, 0.5)
names(theta) <- pars
x0 <- c(0.9, 0.9)
names(x0) <- vars
library("simode")

sigma <- 0.1
priorInf=c(0.1,1,3,5)

n <- 256
time <- seq(0,25,length.out=n)
model_out <- solve_ode(equations,theta,x0,time)
plot(model_out)

x_det <- model_out[,vars]

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
}
names(obs) <- vars

nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                   + priorInf[4]*theta[nlin_pars])
nlin_init <- sapply(nlin_init, max, 0)
nlin_init <- sapply(nlin_init, min, 1)
names(nlin_init) <- nlin_pars

par(mfrow=c(1,2))
plot(time,model_out[,2],'l', ylab="X")
points(time,obs$X)

plot(time,model_out[,3],'l', ylab="Y")
points(time,obs$Y)

lower <- rep(0, length(pars))
names(lower) <- pars
upper <- rep(1, length(pars))
names(upper) <- pars

NLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                nlin_pars=nlin_pars, start=nlin_init, lower=lower, upper=upper,
                im_method = "non-separable",
                simode_ctrl=simode.control(optim_type = "im", im_optim_method = "Nelder-Mead"))
SLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                nlin_pars=nlin_pars, start=nlin_init, lower=lower, upper=upper,
                simode_ctrl=simode.control(optim_type = "im", im_optim_method = "Nelder-Mead"))

plot(NLSmc, type='fit', pars_true=theta, mfrow=c(1,2), legend=T)
plot(SLSmc, type='fit', pars_true=theta, mfrow=c(1,2), legend=T)


N <- 50
set.seed(1000)
library(doRNG)
require(doParallel)
registerDoParallel(cores=8)

args <- c('equations', 'pars', 'time', 'x0', 'theta',
          'x_det', 'vars', 'sigma')

for(ip in 1:4){
  
  results <- foreach(j=1:N, .packages='simode', .export=args) %dorng% {
    obs <- list()
    for(i in 1:length(vars)) {
      obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
    }
    names(obs) <- vars
    
    nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                       + priorInf[ip]*theta[nlin_pars])
    names(nlin_init) <- nlin_pars
    
    ptimeNLS <- system.time({
      NLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                      nlin_pars=nlin_pars, start=nlin_init, lower=lower, upper=upper,
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im", im_optim_method = "Nelder-Mead"))})
    ptimeSLS <- system.time({
      SLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                      nlin_pars=nlin_pars, start=nlin_init, lower=lower, upper=upper,
                      simode_ctrl=simode.control(optim_type = "im", im_optim_method = "Nelder-Mead"))})
    
    list(NLSmc=NLSmc,SLSmc=SLSmc,ptimeNLS=ptimeNLS,ptimeSLS=ptimeSLS)
  }
  
  
  NLSmc_im_loss_vals <- sapply(results,function(x) x$NLSmc$im_loss)
  SLSmc_im_loss_vals <- sapply(results,function(x) x$SLSmc$im_loss)
  NLS_im_vars=sapply(results,function(x) x$NLSmc$im_pars_est)
  SLS_im_vars=sapply(results,function(x) x$SLSmc$im_pars_est)
  NLSmc_time=list()
  SLSmc_time=list()
  for (mc in 1:N){
    NLSmc_time[mc]<-  results[[mc]]$ptimeNLS[3]
    SLSmc_time[mc]<-  results[[mc]]$ptimeSLS[3]
  }
  
  loss_df=data.frame(NLSmc=unlist(NLSmc_im_loss_vals),SLSmc=unlist(SLSmc_im_loss_vals),
                     NLSest_alpha=NLS_im_vars['alpha',],NLSest_beta=NLS_im_vars['beta',],NLSest_gamma=NLS_im_vars['gamma',],NLSest_delta=NLS_im_vars['delta',],
                     NLSest_epsilon=NLS_im_vars['epsilon',],NLSest_omega=NLS_im_vars['omega',],
                     SLSest_alpha=SLS_im_vars['alpha',],SLSest_beta=SLS_im_vars['beta',],SLSest_gamma=SLS_im_vars['gamma',],SLSest_delta=SLS_im_vars['delta',],
                     SLSest_epsilon=SLS_im_vars['epsilon',],SLSest_omega=SLS_im_vars['omega',])
  time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
  write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
  write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}

