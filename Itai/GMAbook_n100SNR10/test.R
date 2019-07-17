rm(list=ls())
library(simode)
library(doRNG)
require(doParallel)
#set.seed(2000)

SNR <- 10
n <- 100
#priorInf=c(0.01,0.05,0.1)
priorInf=c(0.1)

vars <- paste0('x', 1)

eq1 <- 'gamma11*(x1^f121)'

equations <- c(eq1)
names(equations) <- vars

pars1 <- c('gamma11','f121')
pars <- c(pars1)
lin_pars <- c('gamma11')
nlin_pars <- setdiff(pars,lin_pars)

theta1 <- c(0.4,-1)
theta <- c(theta1)
names(theta) <- pars

x0 <- c(0.5)
names(x0) <- vars

time <- seq(0,4,length.out=n)

model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]

sigma_x <- sd(x_det)
sigma <- signif(sigma_x / SNR, digits=2)

print(sigma)

#obs <- list()
#for(i in 1:length(vars)) {
#  obs[[i]] <- x_det + rnorm(n,0,sigma)
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

#lower <- c(0, -1.5, -1.5, 0, 0, -1.5, 0, 0, -1.5, 0, 0, -1.5, 0, 0, 0, 0, -1.5, 0, 0)
#lower <- c(0, -1.5, -1.5, 0, 0, -1.5, 0, 0, -1.5, 0, 0, -1.5, 0, 0, 0, 0, -1.5, 0, 0)

#lower <- 0.9*c(theta[lin_pars],theta[nlin_pars])
#names(lower) <- pars
#nlin_lower <- lower[nlin_pars]
#upper <- c(6, 0, 0, 6, 1.5, 0, 6, 1.5, 0, 6, 1, 0, 6, 1.5, 6, 1.5, 0, 6, 1.5)
#upper <- c(10, 0, 0, 10, 1.5, 0, 10, 1.5, 0, 10, 1, 0, 10, 1.5, 10, 1.5, 0, 10, 1.5)
#upper <- c(10^6, 0, 0, 10^6, 1.5, 0, 10^6, 1.5, 0, 10^6, 1, 0, 10^6, 1.5, 10^6, 1.5, 0, 10^6, 1.5)

#upper <- 1.1*c(theta[lin_pars],theta[nlin_pars])
#names(upper) <- pars
#nlin_upper <- upper[nlin_pars]

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
N <- 12
set.seed(123)
registerDoParallel(cores=16)

args <- c('equations', 'pars', 'time', 'x0', 'theta',
          'nlin_pars', 'x_det', 'vars', 'sigma')


results <- list()

for(ip in 1:1){
  
  results <- foreach(j=1:N, .packages='simode') %dorng% {
    # for(j in 1:N) {
    
    SLSmc <- NULL
    NLSmc <- NULL
    
    while (TRUE) {
      #print("beginloop")
      obs <- list()
      for(i in 1:length(vars)) {
        obs[[i]] <- x_det + rnorm(n,0,sigma)
      }
      names(obs) <- vars
      
      
      nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                         + 0.00001*abs(theta[nlin_pars]))
      names(nlin_init) <- nlin_pars
      lin_init <- rnorm(length(theta[lin_pars]),theta[lin_pars],
                        + priorInf[ip]*abs(theta[lin_pars]))
      names(lin_init) <- lin_pars
      init <- c(lin_init, nlin_init)
      
      ptimeNLS <- system.time({
        #NLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
        #                nlin_pars=nlin_pars,
        #                lower=lower, upper=upper, start=init,
        #                im_method = "non-separable",
        #                simode_ctrl=simode.control(optim_type = "im"))})
      NLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                      nlin_pars=nlin_pars,
                      start=init,
                      im_method = "non-separable",
                      simode_ctrl=simode.control(optim_type = "im"),
                      im_optim_control = list(maxit = 20000))})
      
      if (is.null(NLSmc) || !is.numeric(NLSmc$im_pars_est)) {
        print("should repeat NLS call")
        next
      }
      ptimeSLS <- system.time({
        SLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                        nlin_pars=nlin_pars,
                        start=nlin_init,
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
                     NLSest_gamma11=NLS_im_vars['gamma11',],NLSest_f121=NLS_im_vars['f121',],
                     SLSest_gamma11=SLS_im_vars['gamma11',],SLSest_f121=SLS_im_vars['f121',])  
  time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
  write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
  write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}


#plot(unlist(NLSmc_im_loss_vals),type='l')
#lines(unlist(SLSmc_im_loss_vals),col="red")