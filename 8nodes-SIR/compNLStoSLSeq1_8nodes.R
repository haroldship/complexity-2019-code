rm(list=ls())

pars <- c('alpha1','g12','beta1','h11', 'h13',
          'alpha2','g21','g23', 'g24', 'beta2','h22')
vars <- paste0('x', 1:4)
eq1 <- 'alpha1*(x2^g12)-beta1*(x1^h11)*(x3^h13)'
eq2 <- 'alpha2*(x1^g21)*(x3^g23)*(x4^g24)-beta2*(x2^h22)'
eq3 <- '0'
eq4 <- '0'
equations <- c(eq1,eq2,eq3,eq4)
names(equations) <- vars
theta <- c(2,1,1.2,0.5,-1,2,0.1,-1,0.5,-2,1)
names(theta) <- pars
x0 <- c(2,0.1,0.5,1)
names(x0) <- vars
library("simode")
#sigma <- 0.05 compNLStoSLS1
sigma <- 0.05 
priorInf=c(0.1,1,3,5)


n <- 50
time <- seq(0,10,length.out=n)
model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]
lin_pars <- c('alpha1','beta1','alpha2','beta2')
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
      obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
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