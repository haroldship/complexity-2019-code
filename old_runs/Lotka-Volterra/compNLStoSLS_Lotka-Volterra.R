rm(list=ls())

pars <- c('alpha','beta','gamma','delta')
vars <- c('X','Y')
eq_X <- 'alpha*X-beta*X*Y'
eq_Y <- 'delta*X*Y-gamma*Y'
equations <- c(eq_X,eq_Y)
names(equations) <- vars
theta <- c(2/3, 4/3, 1, 1)
names(theta) <- pars
x0 <- c(0.9, 0.9)
names(x0) <- vars
library("simode")

sigma <- 0.4
priorInf=c(0.1,1,3,5)

n <- 100
time <- seq(0,25,length.out=n)
model_out <- solve_ode(equations,theta,x0,time)
plot(model_out)


x_det <- model_out[,vars]

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
    
    ptimeNLS <- system.time({
      NLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im"))})
    ptimeSLS <- system.time({
      SLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                      simode_ctrl=simode.control(optim_type = "im"))})
    
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
                     SLSest_alpha=SLS_im_vars['alpha',],SLSest_beta=SLS_im_vars['beta',],SLSest_gamma=SLS_im_vars['gamma',],SLSest_delta=SLS_im_vars['delta',])
  time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
  write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
  write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}

