rm(list=ls())
library(simode)
set.seed(2000)

S1_1 <- "-S1_1*(beta1_1*I1_1+beta1_2*I2_1)"
S2_1 <- "-S2_1*(beta2_1*I1_1+beta2_2*I2_1)"
S1_2 <- "-S1_2*kappa2*(beta1_1*I1_2+beta1_2*I2_2)"
S2_2 <- "-S2_2*kappa2*(beta2_1*I1_2+beta2_2*I2_2)"

I1_1 <- "S1_1*(beta1_1*I1_1+beta1_2*I2_1)-gamma*I1_1"
I2_1 <- "S2_1*(beta2_1*I1_1+beta2_2*I2_1)-gamma*I2_1"
I1_2 <- "S1_2*kappa2*(beta1_1*I1_2+beta1_2*I2_2)-gamma*I1_2"
I2_2 <- "S2_2*kappa2*(beta2_1*I1_2+beta2_2*I2_2)-gamma*I2_2"


S0 <- c(0.56,0.57,0.49,0.45)
Svars <- c("S1_1","S2_1","S1_2","S2_2")
names(S0) <- Svars
I0 <- rep(1e-04,4)
Ivars <- c("I1_1","I2_1","I1_2","I2_2")
names(I0) <- Ivars
equations <- c(S1_1,S2_1,S1_2,S2_2,I1_1,I2_1,I1_2,I2_2)
vars <- c(Svars,Ivars)
names(equations) <- vars

beta <- c(6,2,1,3)
names(beta) <- c('beta1_1','beta1_2','beta2_1','beta2_2')
gamma <- 2.333333
names(gamma) <- "gamma"
kappa <- c(1.182)
names(kappa) <- c("kappa2")
theta <- c(beta, gamma, kappa)

theta <- c(beta, gamma, kappa)
names(theta) <- c(names(beta), names(gamma), names(kappa))

n <- 18
time <- 1:n

x0 <- c(S0,I0)
names(x0) <- vars

priorInf=c(0.25,0.5,1,2)

model_out <- solve_ode(equations,theta,x0,time)
# plot(model_out)
x_det <- model_out[,vars]

SNR <- 10
sigma_x <- apply(x_det, 2, sd)
sigma <- signif(sigma_x / SNR, digits=2)
print(sigma)

pars <- c(names(beta), names(kappa))
lin_pars <- names(beta)
nlin_pars <- setdiff(pars,lin_pars)

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
}
names(obs) <- vars

# par(mfrow=c(1,1))
# plot(time,model_out[,"I1_1"], 'l', ylab="I1_1")
# points(time,obs$I1_1)

lower <- rep(0.5,length(nlin_pars))
names(lower) <- nlin_pars
upper <- rep(1.5,length(nlin_pars))
names(upper) <- nlin_pars

nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                   + priorInf[1]*theta[nlin_pars])
names(nlin_init) <- nlin_pars

estS <- simode(equations=equations, pars=pars, time=time, obs=obs,
              fixed=c(gamma, x0), nlin=nlin_pars, start=nlin_init,
              lower=lower, upper=upper,
              simode_ctrl=simode.control(optim_type = "im"))
# summary(estS)
# plot(estS, type='fit', pars_true=theta, mfrow=c(2,1), legend=TRUE)
estNS <- simode(equations=equations, pars=pars, time=time, obs=obs,
               fixed=c(gamma, x0), nlin=nlin_pars, start=nlin_init,
               lower=lower, upper=upper,
               im_method = "non-separable",
               simode_ctrl=simode.control(optim_type = "im"))
# summary(estNS)
# plot(estNS, type='fit', pars_true=theta, mfrow=c(2,1), legend=TRUE)


N <- 500
set.seed(1000)
library(doRNG)
require(doParallel)
registerDoParallel(cores=16)

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

    nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                       + priorInf[ip]*theta[nlin_pars])
    nlin_init <- pmax(nlin_init, 0.95*lower)
    nlin_init <- pmin(nlin_init, 0.95*upper)
    names(nlin_init) <- nlin_pars
    
    while (TRUE) {
      ptimeNLS <- system.time({
        NLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs,
                        fixed=c(gamma, x0), nlin=nlin_pars, start=nlin_init,
                        lower=lower, upper=upper,
                        im_method = "non-separable",
                        simode_ctrl=simode.control(optim_type = "im"))})
      if (is.null(NLSmc) || !is.numeric(NLSmc$im_pars_est)) {
        print("should repeat NLS call")
        next
      }
      
      ptimeSLS <- system.time({
        SLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs,
                        fixed=c(gamma, x0), nlin=nlin_pars, start=nlin_init,
                        lower=lower, upper=upper,
                        simode_ctrl=simode.control(optim_type = "im"))})
      if (is.null(SLSmc) || !is.numeric(SLSmc$im_pars_est)) {
        print("should repeat SLS call")
        next
      }
      break
      
    }
    
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
                     NLSest_beta1_1=NLS_im_vars['beta1_1',],NLSest_beta2_1=NLS_im_vars['beta2_1',],NLSest_beta1_2=NLS_im_vars['beta1_2',],NLSest_beta2_2=NLS_im_vars['beta2_2',],
                     SLSest_beta1_1=SLS_im_vars['beta1_1',],SLSest_beta2_1=SLS_im_vars['beta2_1',],SLSest_beta1_2=SLS_im_vars['beta1_2',],SLSest_beta2_2=SLS_im_vars['beta2_2',],
                     NLSest_kappa2=NLS_im_vars['kappa2',],
                     SLSest_kappa2=SLS_im_vars['kappa2',]
  )
  time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
  write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
  write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}
#plot(unlist(NLSmc_im_loss_vals),type='l')
#lines(unlist(SLSmc_im_loss_vals),col="red")


