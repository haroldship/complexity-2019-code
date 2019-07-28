rm(list=ls())
library(simode)
library(doRNG)
require(doParallel)
#set.seed(2000)

SNR <- 10
n <- 100
priorInf=c(0.1,0.3,0.5)
#priorInf=c(0.1)

vars <- paste0('x', 1:3)

eq1 <- 'gamma11*(x2^f121)*(x3^f131)-gamma12*(x1^f112)*(x2^f122)-gamma13*(x1^f113)*(x3^f133)'
eq2 <- 'gamma12*(x1^f112)*(x2^f122)-gamma22*(x2^f222)'
eq3 <- 'gamma13*(x1^f113)*(x3^f133)-gamma32*(x3^f332)'
equations <- c(eq1,eq2,eq3)
names(equations) <- vars

pars1 <- c('gamma11','f121','f131','gamma12','f112','f122','gamma13','f113','f133')
pars2 <- c('gamma22','f222')
pars3 <- c('gamma32','f332')
pars <- c(pars1,pars2,pars3)
lin_pars <- c('gamma11','gamma12','gamma13','gamma22','gamma32')
nlin_pars <- setdiff(pars,lin_pars)

theta1 <- c(0.4,-1,-1,3,0.5,-0.1,2,0.75,-0.2)
theta2 <- c(1.5,0.5)
theta3 <- c(5,0.5)
theta <- c(theta1,theta2,theta3)
names(theta) <- pars

x0 <- c(0.5,0.5,1)
names(x0) <- vars

time <- seq(0,4,length.out=n)

model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]

sigma_x <- apply(x_det, 2, sd)
sigma <- signif(sigma_x / SNR, digits=2)

print(sigma)

#obs <- list()
#for(i in 1:length(vars)) {
#  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
#}
#names(obs) <- vars

# pdf(file="../out/solution-gma-SNR10.pdf")
# par(mfrow=c(1,1))
# plot(time,model_out[,'x1'],'l',ylab="",ylim=c(0,1.5), main=expression(GMA~System~with~SNR==10))
# lines(time,model_out[,'x2'])
# lines(time,model_out[,'x3'])
# points(time,obs$x1,pch=1)
# points(time,obs$x2,pch=2)
# points(time,obs$x3,pch=4)
# dev.off()


lower <- c(0, -1.1, -1.1, 0, 0.4, -0.2, 0, 0.6, -0.3, 0, 0.4, 0,0.4)
#lower <- 0.9*c(theta[lin_pars],theta[nlin_pars])
names(lower) <- pars
nlin_lower <- lower[nlin_pars]
upper <- c(1, -0.8, -0.8, 5, 0.6, 0, 3,0.9,0,2,0.6,6,0.6)
#upper <- 1.1*c(theta[lin_pars],theta[nlin_pars])
names(upper) <- pars
nlin_upper <- upper[nlin_pars]

#nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
#                   0.1*abs(theta[nlin_pars]))
#names(nlin_init) <- nlin_pars
#lin_init <- rnorm(length(theta[lin_pars]),theta[lin_pars],
#                  0.1*abs(theta[lin_pars]))
#names(lin_init) <- lin_pars
#init <- c(lin_init, nlin_init)

#ptimeNLS <- system.time({
#NLSest <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
#                 nlin_pars=nlin_pars,
#                 lower=lower, upper=upper, start=init,
#                 im_method = "non-separable",
#                 simode_ctrl=simode.control(optim_type = "im"))
#})

#ptimeSLS <- system.time({
#SLSest <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
#                 nlin_pars=nlin_pars,
#                 lower=nlin_lower, upper=nlin_upper, start=nlin_init,
#                 simode_ctrl=simode.control(optim_type = "im"))
#
#})
#ptimeNLS
#ptimeSLS

#par(mfrow=c(1,3))
#plot(NLSest, type="fit", show="im")
#plot(SLSest, type="fit", show="im")

#unlink("log")
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
                      start=init,lower=nlin_lower, upper=nlin_upper,
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
  
  loss_df=data.frame(NLSmc=unlist(NLSmc_im_loss_vals),SLSmc=unlist(SLSmc_im_loss_vals),
                     NLSest_gamma11=NLS_im_vars['gamma11',],NLSest_f121=NLS_im_vars['f121',],NLSest_f131=NLS_im_vars['f131',],
                     NLSest_gamma12=NLS_im_vars['gamma12',],NLSest_f112=NLS_im_vars['f112',],NLSest_f122=NLS_im_vars['f122',],
                     NLSest_gamma13=NLS_im_vars['gamma13',],NLSest_f113=NLS_im_vars['f113',],NLSest_f133=NLS_im_vars['f133',],
                     NLSest_gamma22=NLS_im_vars['gamma22',],NLSest_f222=NLS_im_vars['f222',],
                     NLSest_gamma32=NLS_im_vars['gamma32',],NLSest_f332=NLS_im_vars['f332',],
                     SLSest_gamma11=SLS_im_vars['gamma11',],SLSest_f121=SLS_im_vars['f121',],SLSest_f131=SLS_im_vars['f131',],
                     SLSest_gamma12=SLS_im_vars['gamma12',],SLSest_f112=SLS_im_vars['f112',],SLSest_f122=SLS_im_vars['f122',],
                     SLSest_gamma13=SLS_im_vars['gamma13',],SLSest_f113=SLS_im_vars['f113',],SLSest_f133=SLS_im_vars['f133',],
                     SLSest_gamma22=SLS_im_vars['gamma22',],SLSest_f222=SLS_im_vars['f222',],
                     SLSest_gamma32=SLS_im_vars['gamma32',],SLSest_f332=SLS_im_vars['f332',]
  )  
  time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
  write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
  write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}


#plot(unlist(NLSmc_im_loss_vals),type='l')
#lines(unlist(SLSmc_im_loss_vals),col="red")