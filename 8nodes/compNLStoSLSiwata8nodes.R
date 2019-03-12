rm(list=ls())

pars <- c('alpha1','g13','beta1','h11',
          'alpha2','g21','beta2','h22',
          'alpha3','g32','beta3','h33','h34',
          'alpha4','g41','beta4','h44')
vars <- paste0('x', 1:4)
eq1 <- 'alpha1*(x3^g13)-beta1*(x1^h11)'
eq2 <- 'alpha2*(x1^g21)-beta2*(x2^h22)'
eq3 <- 'alpha3*(x2^g32)-beta3*(x3^h33)*(x4^h34)'
eq4 <- 'alpha4*(x1^g41)-beta4*(x4^h44)'
equations <- c(eq1,eq2,eq3,eq4)
names(equations) <- vars
theta <- c(12,-0.8,10,0.5,8,0.5,3,0.75,3,0.75,5,0.5,0.2,2,0.5,6,0.8)
names(theta) <- pars
x0 <- c(1.4,2.7,1.2,0.4)
names(x0) <- vars
library("simode")
#sigma <- 0.05 compNLStoSLS1
sigma <- 0.05 
priorInf=c(0.1,1,3,5)


n <- 50
time <- seq(0,10,length.out=n)
model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]
lin_pars <- c('alpha1','beta1','alpha2','beta2',
              'alpha3','beta3','alpha4','beta4')
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