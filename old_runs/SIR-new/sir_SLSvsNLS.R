
library(simode)
library(ggplot2)
library(pracma)

rm(list=ls())
S1_1 <- "-S1_1*(beta1_1*I1_1)"
I1_1 <- "S1_1*(beta1_1*I1_1)-gamma*I1_1"
S0 <- 0.56
Svars <- c("S1_1")
names(S0) <- Svars
I0 <- 0.0001
Ivars <- c("I1_1")
names(I0) <- Ivars
equations <- c(S1_1,I1_1)
vars <- c(Svars,Ivars)
names(equations) <- vars

beta <- 6
names(beta) <- c('beta1_1')
gamma <- 2.333333
names(gamma) <- "gamma"
theta <- c(beta, gamma)

n <- 18
time <- 1:n

x0 <- c(S0,I0)
names(x0) <- vars
model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,0.001)
}
names(obs) <- vars

par(mfrow=c(1,1))
plot(time,model_out[,3],'l')
points(time,obs$I1_1,col='red')

gen_obs <- function(equations, pars, x0, time, obs,
                    gamma, S_names, I_names, ...)
{
  S0 <- x0[S_names]
  I0 <- x0[I_names]
  I_obs <- obs
  S_obs <- lapply(1:length(S0),function(i)
    S0[i]+I0[i]-I_obs[[i]]-gamma*pracma::cumtrapz(time,I_obs[[i]]))
  names(S_obs) <- S_names
  obs <- c(S_obs,I_obs)
  return (list(obs=obs, time=time))
}
pars <- c(names(beta),names(S0))
pars_min <- rep(0,length(pars))
names(pars_min) <- pars
pars_max <- rep(1,length(S0))
names(pars_max) <- names(S0)
S0_init <- rep(0.5,length(S0))
names(S0_init) <- names(S0)

priorInf=c(0.1,1,3,5)

N <- 20
set.seed(1000)
library(doRNG)
require(doParallel)
registerDoParallel(cores=8)

args <- c('equations', 'pars', 'time', 'x0', 'theta',
          'Svars', 'x_det', 'vars', 'sigma')

results <- list()

for(ip in 1:4){

  #res <- list()
  res <- foreach(j=1:N, .packages='simode', .export=args) %dorng% {
  #for(j in 1:N) {
    obs <- list()
    for(i in 1:length(vars)) {
      obs[[i]] <- x_det[,i] + rnorm(n,0,0.001)
    }
    names(obs) <- vars

    nlin_init <- rnorm(length(S0),S0,priorInf[ip]*abs(S0))
    names(nlin_init) <- Svars


    NLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs[Ivars],
                    fixed=c(I0,gamma), nlin=names(S0), start=nlin_init,
                    lower=pars_min, upper=pars_max, gen_obs=gen_obs, gamma=gamma,
                    S_names=names(S0), I_names=names(I0),
                    im_method = "non-separable",
                    simode_ctrl=simode.control(optim_type = "im"))

    SLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs[Ivars],
                    fixed=c(I0,gamma), nlin=Svars, start=nlin_init,
                    lower=pars_min, upper=pars_max, gen_obs=gen_obs, gamma=gamma,
                    S_names=Svars, I_names=Ivars,
                    simode_ctrl=simode.control(optim_type = "im"))

    #res[[j]] <- list(NLSmc=NLSmc,SLSmc=SLSmc)
    list(NLSmc=NLSmc,SLSmc=SLSmc)
  }
  results[[ip]] <- res
}
# NLSmc_im_loss_vals <- sapply(results,function(x) x$NLSmc$im_loss)
# SLSmc_im_loss_vals <- sapply(results,function(x) x$SLSmc$im_loss)
# NLS_im_vars=sapply(results,function(x) x$NLSmc$im_pars_est)
# SLS_im_vars=sapply(results,function(x) x$SLSmc$im_pars_est)

beta_sd_sls <- c()
beta_mean_sls <- c()
beta_sd_nls <- c()
beta_mean_nls <- c()
S0_sd_sls <- c()
S0_mean_sls <- c()
S0_sd_nls <- c()
S0_mean_nls <- c()
ip_num <- length(priorInf)
for(ip in 1:ip_num) {
  beta_est_sls <- c()
  S0_est_sls <- c()
  beta_est_nls <- c()
  S0_est_nls <- c()
  for(j in 1:N) {
    fit_sls <- results[[ip]][[j]]$SLSmc
    beta_est_sls[j] <- fit_sls$im_pars_est[names(beta)]
    S0_est_sls[j] <- fit_sls$im_pars_est[Svars]
    fit_nls <- results[[ip]][[j]]$NLSmc
    beta_est_nls[j] <- fit_nls$im_pars_est[names(beta)]
    S0_est_nls[j] <- fit_nls$im_pars_est[Svars]
  }
  beta_sd_sls[ip] <- sd(beta_est_sls)
  beta_mean_sls[ip] <- mean(beta_est_sls)
  beta_sd_nls[ip] <- sd(beta_est_nls)
  beta_mean_nls[ip] <- mean(beta_est_nls)
  S0_sd_sls[ip] <- sd(S0_est_sls)
  S0_mean_sls[ip] <- mean(S0_est_sls)
  S0_sd_nls[ip] <- sd(S0_est_nls)
  S0_mean_nls[ip] <- mean(S0_est_nls)
}

#X11()
plot(beta_mean_sls,col='blue',ylim=c(0,8),
     xlab='priorInf', ylab=expression(beta), xaxt="n")
axis(side=1, at=1:ip_num, labels = pracma::num2str(priorInf,1))
errorbar(x=1:ip_num,y=beta_mean_sls,yerr=beta_sd_sls,bar.col='blue',add=T)
points(beta_mean_nls,col='red')
errorbar(x=1:ip_num,y=beta_mean_nls,yerr=beta_sd_nls,bar.col='red',add=T)
legend(x='topleft',legend=c('SLS','NLS'),fill=c('blue','red'), horiz=T)

#X11()
plot(S0_mean_sls,col='blue',ylim=c(0,1),
     xlab='priorInf', ylab=expression(S0), xaxt="n")
axis(side=1, at=1:ip_num, labels = pracma::num2str(priorInf,1))
errorbar(x=1:ip_num,y=S0_mean_sls,yerr=S0_sd_sls,bar.col='blue',add=T)
points(S0_mean_nls,col='red')
errorbar(x=1:ip_num,y=S0_mean_nls,yerr=S0_sd_nls,bar.col='red',add=T)
legend(x='topleft',legend=c('SLS','NLS'),fill=c('blue','red'), horiz=T)
