rm(list=ls())
library(simode)
set.seed(1000)

n <- 40
SNR <- 5
priorInf=c(0.1,1,2)

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

time <- seq(0, 20, length.out=n)

x0 <- c(-1, 1)
names(x0) <- vars

model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]
# plot(model_out)

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

# estNS <- simode(equations=equations, pars=pars, time=time, obs=obs,
#               fixed=x0, nlin=nlin_pars,
#               start=nlin_init, 
#               im_method = "non-separable",
#               simode_ctrl=simode.control(optim_type = "im"))
# summary(estNS)
# plot(estNS, type='fit', pars_true=theta, mfrow=c(2,1), legend=TRUE)
# estS <- simode(equations=equations, pars=pars, time=time, obs=obs,
#                 fixed=x0, nlin=nlin_pars,
#                 start=nlin_init, 
#                 simode_ctrl=simode.control(optim_type = "im"))
# summary(estS)
# plot(estS, type='fit', pars_true=theta, mfrow=c(2,1), legend=TRUE)

nlin_lower <- rep(0, length(nlin_pars))
names(nlin_lower) <- nlin_pars
lower <- rep(0.0,length(pars))
names(lower) <- pars
nlin_upper <- rep(10, length(nlin_pars))
names(nlin_upper) <- nlin_pars
upper <- c(rep(2, length(lin_pars)), nlin_upper)
names(upper) <- pars

N <- 500
library(doRNG)
require(doParallel)
registerDoParallel(cores=16)

args <- c('equations', 'pars', 'time', 'x0', 'theta', 'obs',
          'vars', 'x_det', 'vars', 'sigma')

set.seed(1000)

sls_results <- foreach(j=1:N, .packages='simode', .export=args) %dorng% {
  # for(j in 1:N) {
  obs <- list()
  for(i in 1:length(vars)) {
    obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
  }
  names(obs) <- vars
  
  nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                     + priorInf[1]*theta[nlin_pars])
  names(nlin_init) <- nlin_pars

  ptimeSLS <- system.time({
    SLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs,
                    fixed=x0, nlin=nlin_pars,
                    lower=nlin_lower, upper=nlin_upper, start=nlin_init,
                    simode_ctrl=simode.control(optim_type = "im"))})
  
  list(SLSmc=SLSmc,ptimeSLS=ptimeSLS)
}

for(ip in 1:length(priorInf)) {
  
  set.seed(1000)
  
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
    lin_init <- rnorm(length(lin_pars),theta[lin_pars],priorInf[ip]*theta[lin_pars])
    names(lin_init) <- lin_pars
    init <- c(lin_init, nlin_init)
    #names(init) <- pars
    
    ptimeNLS <- system.time({
      NLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs,
                      fixed=x0, nlin=nlin_pars,
                      lower=lower, upper=upper, start=init,
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im"))})

    list(NLSmc=NLSmc,ptimeNLS=ptimeNLS)
  }
  
  
  NLSmc_im_loss_vals <- sapply(results,function(x) x$NLSmc$im_loss)
  SLSmc_im_loss_vals <- sapply(sls_results,function(x) x$SLSmc$im_loss)
  NLS_im_vars=sapply(results,function(x) x$NLSmc$im_pars_est)
  SLS_im_vars=sapply(sls_results,function(x) x$SLSmc$im_pars_est)
  NLSmc_time=list()
  SLSmc_time=list()
  NLSmc_sse=list()
  SLSmc_sse=list()
  for (mc in 1:N){
    NLSmc_time[mc]<-  results[[mc]]$ptimeNLS[3]
    SLSmc_time[mc]<-  sls_results[[mc]]$ptimeSLS[3]
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


