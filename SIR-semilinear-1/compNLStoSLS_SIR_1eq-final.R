rm(list=ls())
library(simode)
set.seed(2000)

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

theta <- c(beta, gamma)
names(theta) <- c('beta1_1', "gamma")

n <- 18
time <- 1:n

x0 <- c(S0,I0)
names(x0) <- vars

priorInf=c(0.1,1,3,5)

model_out <- solve_ode(equations,theta,x0,time)
plot(model_out)
x_det <- model_out[,vars]

SNR <- 10
sigma_x <- apply(x_det, 2, sd)
sigma <- signif(sigma_x / SNR, digits=2)
print(sigma)

pars <- c('beta1_1', "S1_1")
lin_pars <- c('beta1_1')
nlin_pars <- setdiff(pars,lin_pars)

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
}
names(obs) <- vars

par(mfrow=c(1,1))
plot(time,model_out[,"I1_1"], 'l', ylab="I1_1")
points(time,obs$I1_1)

gen_obs <- function(equations, pars, x0, time, obs, gamma, S_names, I_names) {
  S0 <- x0[S_names]
  I0 <- x0[I_names]
  I_obs <- obs
  S_obs <- lapply(1:length(S0), function(i) {
    S0[i]+I0[i]-I_obs[[i]]-gamma*pracma::cumtrapz(time,I_obs[[i]])
  })
  names(S_obs) <- S_names
  obs <- c(S_obs,I_obs)
  return (list(obs=obs, time=time))
}

pars_min <- rep(0,length(pars))
names(pars_min) <- pars
pars_max <- rep(1,length(S0))
names(pars_max) <- names(S0)
S0_init <- rep(0.5,length(S0))
names(S0_init) <- names(S0)

est <- simode(equations=equations, pars=pars, time=time, obs=obs[Ivars],
              fixed=c(I0,gamma), nlin=Svars, start=S0_init,
              lower=pars_min, upper=pars_max, gen_obs=gen_obs, gamma=gamma,
              S_names=Svars, I_names=Ivars,
              simode_ctrl=simode.control(optim_type = "im"))
summary(est)

N <- 100
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
      obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
    }
    names(obs) <- vars

    nlin_init <- rnorm(length(S0),S0,priorInf[ip]*abs(S0))
    names(nlin_init) <- Svars
    
    ptimeNLS <- system.time({
      NLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs[Ivars],
                      fixed=c(I0,gamma), nlin=Svars, start=nlin_init,
                      lower=pars_min, upper=pars_max, gen_obs=gen_obs, gamma=gamma,
                      S_names=Svars, I_names=Ivars,
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im"))})
    ptimeSLS <- system.time({
      SLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs[Ivars],
                      fixed=c(I0,gamma), nlin=Svars, start=nlin_init,
                      lower=pars_min, upper=pars_max, gen_obs=gen_obs, gamma=gamma,
                      S_names=Svars, I_names=Ivars,
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
  #mean(unlist(NLSmc_im_loss_vals))
  #mean(unlist(SLSmc_im_loss_vals))
  #mean(unlist(NLSmc_time))
  #mean(unlist(SLSmc_time))
  
  loss_df=data.frame(NLSmc=unlist(NLSmc_im_loss_vals),SLSmc=unlist(SLSmc_im_loss_vals),
                     NLSest_beta1_1=NLS_im_vars['beta1_1',],SLSest_beta1_1=SLS_im_vars['beta1_1',],
                     NLSest_S1_1=NLS_im_vars['S1_1',],SLSest_S1_1=SLS_im_vars['S1_1',]
  )
  time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
  write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
  write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}
#plot(unlist(NLSmc_im_loss_vals),type='l')
#lines(unlist(SLSmc_im_loss_vals),col="red")


