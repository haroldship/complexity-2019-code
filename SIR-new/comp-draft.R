rm(list=ls())
library(simode)
set.seed(1000)

S1_1 <- "-S1_1*(beta1_1*I1_1)"
I1_1 <- "S1_1*(beta1_1*I1_1)-gamma*I1_1"

equations <- c(S1_1, I1_1)
vars <- c("S1_1", "I1_1")
names(equations) <- vars

I0 <- 1e-4
names(I0) <- "I1_1"
S0 <- 0.56
names(S0) <- "S1_1"
S0_min <- 0
S0_max <- 1
S0_init <- 0.5

gamma <- 2.333333
names(gamma) <- 'gamma'

pars <- c('beta1_1', "S1_1")

theta <- c(6, gamma)
names(theta) <- c('beta1_1', "gamma")

x0 <- c(S0, I0)
names(x0) <- vars

sigma <- c(1e-3)
priorInf=c(0.1,1,3,5)

n <- 18
time <- 1:n
model_out <- solve_ode(equations,theta,x0,time)
plot(model_out)

x_det <- model_out[,vars]
lin_pars <- c('beta1_1')
nlin_pars <- setdiff(pars,lin_pars)

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
}
names(obs) <- vars

par(mfrow=c(1,1))
plot(time,model_out[,"I1_1"], 'l', ylab="I1_1")
#par(new=TRUE)
points(time,obs$I1_1)

gen_obs <- function(equations, pars, x0, time, obs,
                    gamma, S_names, I_names, ...) {
    S0 <- x0[S_names]
    I0 <- x0[I_names]
    I_obs <- obs
    S_obs <- lapply(1:length(S0),function(i)
      S0[i]+I0[i]-I_obs[[i]]-gamma*pracma::cumtrapz(time,I_obs[[i]]))
    names(S_obs) <- S_names
    obs <- c(S_obs,I_obs)
    return (list(obs=obs, time=time))
}

I_obs <- list(obs$I1_1)

foo <- simode(
  equations=equations, pars=pars, time=time, obs=I_obs,
  fixed=c(I0,gamma), nlin="S1_1", start=S0_init,
  lower=S0_min, upper=S0_max,  gen_obs=gen_obs,
  gamma=gamma, S_names="S1_1", I_names="I1_1")


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
      obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
    }
    names(obs) <- vars
    
    nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                       + priorInf[ip]*abs(theta[nlin_pars]))
    names(nlin_init) <- nlin_pars
    
    ptimeNLS <- system.time({
      NLSmc <- simode(equations=equations, pars=pars, fixed=c(x0,gamma), time=time, obs=obs,
                      nlin_pars=nlin_pars, start=nlin_init, 
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im"))})
    ptimeSLS <- system.time({
      SLSmc <- simode(equations=equations, pars=pars, fixed=c(x0,gamma), time=time, obs=obs,
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

