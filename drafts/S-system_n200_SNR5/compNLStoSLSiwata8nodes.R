rm(list=ls())
library(simode)
library(doRNG)
require(doParallel)
#set.seed(2000)

SNR <- 5
n <- 200

priorInf=c(0.1,0.2,0.3)

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


time <- seq(0,10,length.out=n)
model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]
lin_pars <- c('alpha1','beta1','alpha2','beta2',
              'alpha3','beta3','alpha4','beta4')
nlin_pars <- setdiff(pars,lin_pars)

sigma_x <- apply(x_det, 2, sd)
sigma <- signif(sigma_x / SNR, digits=2)

print(sigma)

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
}
names(obs) <- vars


 # par(mfrow=c(1,1))
 # plot(time,model_out[,'x1'],'l',ylab="",ylim=c(0,4), main=expression(GMA~System~with~SNR==10))
 # lines(time,model_out[,'x2'])
 # lines(time,model_out[,'x3'])
 # lines(time,model_out[,'x4'])
 # points(time,obs$x1,pch=1)
 # points(time,obs$x2,pch=2)
 # points(time,obs$x3,pch=4)
 # points(time,obs$x4,pch=4)
         
lower <- c(11,-0.9,9, 0.4,7,0.4,2,0.7, 2,0.7 ,4,0.4,0.1,1,0.4,5,0.7)
names(lower) <- pars
nlin_lower <- lower[nlin_pars]
#########c(12,-0.8,10,0.5,8,0.5,3,0.75,3,0.75,5,0.5,0.2,2,0.5,6,0.8)
upper <- c(13,-0.7,11,0.6,9,0.6,4,0.8 ,4,0.8 ,6,0.6,0.3,3,0.6,7,0.9)
names(upper) <- pars
nlin_upper <- upper[nlin_pars]

# 
# obs <- list()
# for(i in 1:length(vars)) {
#   obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
# }
# names(obs) <- vars
# 
# nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
#                    + 0.001*abs(theta[nlin_pars]))
# names(nlin_init) <- nlin_pars
# lin_init <- rnorm(length(theta[lin_pars]),theta[lin_pars],
#                   + priorInf[1]*abs(theta[lin_pars]))
# names(lin_init) <- lin_pars
# init <- c(lin_init, nlin_init)
# 
# ptimeNLS <- system.time({
#   NLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
#                   nlin_pars=nlin_pars,
#                   start=init,lower=lower, upper=upper,
#                   im_method = "non-separable",
#                   simode_ctrl=simode.control(optim_type = "im"))})
# 
# ptimeSLS <- system.time({
#   SLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
#                   nlin_pars=nlin_pars,
#                   start=nlin_init,lower=nlin_lower, upper=nlin_upper,
#                   simode_ctrl=simode.control(optim_type = "im"))})
# 


N <- 500
registerDoParallel(cores=16)


args <- c('equations', 'pars', 'time', 'x0', 'theta',
         'nlin_pars', 'x_det', 'vars', 'sigma')

results <- list()

for(ip in 1:3){
  
  set.seed(123)
  
  results <- foreach(j=1:N, .packages='simode') %dorng% {
    # for(j in 1:N) {
    
    SLSmc <- NULL
    NLSmc <- NULL
    
    while (TRUE) {
      #print("beginloop")
      obs <- list()
      for(i in 1:length(vars)) {
        obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
      }
      names(obs) <- vars
      
      nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                         + 0.001*abs(theta[nlin_pars]))
      names(nlin_init) <- nlin_pars
      lin_init <- rnorm(length(theta[lin_pars]),theta[lin_pars],
                        + priorInf[ip]*abs(theta[lin_pars]))
      names(lin_init) <- lin_pars
      init <- c(lin_init, nlin_init)
      
      ptimeNLS <- system.time({
        NLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                        nlin_pars=nlin_pars,
                        start=init,lower=lower, upper=upper,
                        im_method = "non-separable",
                        simode_ctrl=simode.control(optim_type = "im"))})
      
      if (is.null(NLSmc) || !is.numeric(NLSmc$im_pars_est)) {
        print("should repeat NLS call")
        next
      }
      ptimeSLS <- system.time({
        SLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                        nlin_pars=nlin_pars,
                        start=nlin_init,lower=nlin_lower, upper=nlin_upper,
                        simode_ctrl=simode.control(optim_type = "im"))})
      
      if (is.null(SLSmc) || !is.numeric(SLSmc$im_pars_est)) {
        print("should repeat SLS call")
        next
      }
      break
    }
    
    #print(paste0("NLS num:", is.numeric(NLSmc$im_pars_est), " SLS num:", is.numeric(SLSmc$im_pars_est), " num NLS:", length(NLSmc$im_pars_est), " num SLS:", length(SLSmc$im_pars_est)))
    
    list(NLSmc=NLSmc,SLSmc=SLSmc,ptimeNLS=ptimeNLS,ptimeSLS=ptimeSLS)
    #results[[j]] <- list(NLSmc=NLSmc,SLSmc=SLSmc,ptimeNLS=ptimeNLS,ptimeSLS=ptimeSLS)
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
#loss_df=data.frame(NLSmc=unlist(NLSmc_im_loss_vals),SLSmc=unlist(SLSmc_im_loss_vals))
  loss_df=data.frame(NLSmc=unlist(NLSmc_im_loss_vals),SLSmc=unlist(SLSmc_im_loss_vals),
                     NLSest_alpha1=NLS_im_vars['alpha1',],NLSest_g13=NLS_im_vars['g13',],
                     NLSest_beta1=NLS_im_vars['beta1',],NLSest_h11=NLS_im_vars['h11',],
                     NLSest_alpha2=NLS_im_vars['alpha2',],NLSest_g21=NLS_im_vars['g21',],
                     NLSest_beta2=NLS_im_vars['beta2',],NLSest_h22=NLS_im_vars['h22',],
                     NLSest_alpha3=NLS_im_vars['alpha3',],NLSest_g32=NLS_im_vars['g32',],
                     NLSest_beta3=NLS_im_vars['beta3',],NLSest_h33=NLS_im_vars['h33',],NLSest_h34=NLS_im_vars['h34',],
                     NLSest_alpha4=NLS_im_vars['alpha4',],NLSest_g41=NLS_im_vars['g41',],
                     NLSest_beta4=NLS_im_vars['beta4',],NLSest_h44=NLS_im_vars['h44',],
                     SLSest_alpha1=SLS_im_vars['alpha1',],SLSest_g13=SLS_im_vars['g13',],
                     SLSest_beta1=SLS_im_vars['beta1',],SLSest_h11=SLS_im_vars['h11',],
                     SLSest_alpha2=SLS_im_vars['alpha2',],SLSest_g21=SLS_im_vars['g21',],
                     SLSest_beta2=SLS_im_vars['beta2',],SLSest_h22=SLS_im_vars['h22',],
                     SLSest_alpha3=SLS_im_vars['alpha3',],SLSest_g32=SLS_im_vars['g32',],
                     SLSest_beta3=SLS_im_vars['beta3',],SLSest_h33=SLS_im_vars['h33',],SLSest_h34=SLS_im_vars['h34',],
                     SLSest_alpha4=SLS_im_vars['alpha4',],SLSest_g41=SLS_im_vars['g41',],
                     SLSest_beta4=SLS_im_vars['beta4',],SLSest_h44=SLS_im_vars['h44',])

time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}
#plot(unlist(NLSmc_im_loss_vals),type='l')
#lines(unlist(SLSmc_im_loss_vals),col="red")