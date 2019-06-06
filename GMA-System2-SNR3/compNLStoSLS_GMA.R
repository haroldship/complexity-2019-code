rm(list=ls())
library(simode)
library(doRNG)
require(doParallel)
set.seed(2000)

vars <- paste0('Z', 1:34)

# i1 <- 0
# v1 <- 'alpha1*(Z1^g11)*(Z2^g21)*Z17'
# v2 <- 'alpha2*(Z2^g22)*Z18'
# v3 <- 'alpha3*(Z2^g23)*(Z3^g33)*Z19'
# v4 <- 'alpha4*(Z3^g34)*(Z4^g44)*Z20'
# v5 <- 'alpha5*(Z4^g45)*(Z5^g55)*(Z10^g105)*Z21'
# v6 <- 'alpha6*(Z5^g56)*(Z6^g66)*Z22'
# v7 <- 'alpha7*(Z6^g67)*Z23'
# v8 <- 'alpha8*(Z3^g38)*Z24'
# v9 <- 'alpha9*(Z4^g49)*(Z7^g79)*Z25'
# v10 <- 'alpha10*(Z4^g410)*Z26'
# v11 <- 'alpha11*(Z7^g711)*(Z8^g811)*Z20'
# v12 <- 'alpha12*(Z7^g712)*(Z9^g912)*Z27'
# v13 <- 'alpha13*(Z8^g813)*(Z10^g1013)*Z28'
# v14 <- 'alpha14*(Z9^g914)*(Z10^g1014)*Z20'
# v15 <- 'alpha15*(Z10^g1015)*(Z11^g1115)*(Z4^g415)*Z21'
# v16 <- 'alpha16*(Z11^g1116)*(Z12^g1216)*Z22'
# v17 <- 'alpha17*(Z12^g1217)*Z29'
# v18 <- 'alpha18*(Z9^g918)*Z30'
# v19 <- 'alpha19*(Z11^g1119)*(Z13^g1319)*Z31'
# v20 <- 'alpha20*(Z12^g1220)*(Z14^g1420)*Z31'
# v21 <- 'alpha21*(Z14^g1421)*Z32'
# v22 <- 'alpha22*(Z13^g1322)*(Z15^g1522)*Z27'
# v23 <- 'alpha23*(Z14^g1423)*(Z16^g1623)*Z27'
# v24 <- 'alpha24*(Z15*g1524)*(Z16*g1624)*Z22'
# v25 <- 'alpha25*(Z16*g1625)*Z33'
# v26 <- 'alpha26*(Z10^g1026)*(Z12^g1226)*(Z4^g426)*Z34'

# from 2015 paper
# with I1 = I2 = 0
# eq1 <- 'i1-v1'
# eq2 <- 'v1-v2-v3'
# eq3 <- 'v3-v4-v8'
# eq4 <- 'v4-v5-v9-v10'
# eq5 <- 'v5-v6'
# eq6 <- 'v6-v7'
# eq7 <- 'v9-v11-v12'
# eq8 <- 'v11-v13'
# eq9 <- 'v12-v14-v18'
# eq10 <- 'v13+v14-v15-v26'
# eq11 <- 'v15-v16-v19'
# eq12 <- 'v16+v26-v17-v20'
# eq13 <- 'v19-v22'
# eq14 <- 'v20-v21-v23'
# eq15 <- 'v22-v24'
# eq16 <- 'v23+v24-v25'
eq1 <- '-alpha1*(Z1^g11)*(Z2^g21)*Z17'
eq2 <- 'alpha1*(Z1^g11)*(Z2^g21)*Z17-alpha2*(Z2^g22)*Z18-alpha3*(Z2^g23)*(Z3^g33)*Z19'
eq3 <- 'alpha3*(Z2^g23)*(Z3^g33)*Z19-alpha4*(Z3^g34)*(Z4^g44)*Z20-alpha8*(Z3^g38)*Z24'
eq4 <- 'alpha4*(Z3^g34)*(Z4^g44)*Z20-alpha5*(Z4^g45)*(Z5^g55)*(Z10^g105)*Z21-alpha9*(Z4^g49)*(Z7^g79)*Z25-alpha10*(Z4^g410)*Z26'
eq5 <- 'alpha5*(Z4^g45)*(Z5^g55)*(Z10^g105)*Z21-alpha6*(Z5^g56)*(Z6^g66)*Z22'
eq6 <- 'alpha6*(Z5^g56)*(Z6^g66)*Z22-alpha7*(Z6^g67)*Z23'
eq7 <- 'alpha9*(Z4^g49)*(Z7^g79)*Z25-alpha11*(Z7^g711)*(Z8^g811)*Z20-alpha12*(Z7^g712)*(Z9^g912)*Z27'
eq8 <- 'alpha11*(Z7^g711)*(Z8^g811)*Z20-alpha13*(Z8^g813)*(Z10^g1013)*Z28'
eq9 <- 'alpha12*(Z7^g712)*(Z9^g912)*Z27-alpha14*(Z9^g914)*(Z10^g1014)*Z20-alpha18*(Z9^g918)*Z30'
eq10 <- 'alpha13*(Z8^g813)*(Z10^g1013)*Z28+alpha14*(Z9^g914)*(Z10^g1014)*Z20-alpha15*(Z10^g1015)*(Z11^g1115)*(Z4^g415)*Z21-alpha26*(Z10^g1026)*(Z12^g1226)*(Z4^g426)*Z34'
eq11 <- 'alpha15*(Z10^g1015)*(Z11^g1115)*(Z4^g415)*Z21-alpha16*(Z11^g1116)*(Z12^g1216)*Z22-alpha19*(Z11^g1119)*(Z13^g1319)*Z31'
eq12 <- 'alpha16*(Z11^g1116)*(Z12^g1216)*Z22+alpha26*(Z10^g1026)*(Z12^g1226)*(Z4^g426)*Z34-alpha17*(Z12^g1217)*Z29-alpha20*(Z12^g1220)*(Z14^g1420)*Z31'
eq13 <- 'alpha19*(Z11^g1119)*(Z13^g1319)*Z31-alpha22*(Z13^g1322)*(Z15^g1522)*Z27'
eq14 <- 'alpha20*(Z12^g1220)*(Z14^g1420)*Z31-alpha21*(Z14^g1421)*Z32-alpha23*(Z14^g1423)*(Z16^g1623)*Z27'
eq15 <- 'alpha22*(Z13^g1322)*(Z15^g1522)*Z27-alpha24*(Z15*g1524)*(Z16*g1624)*Z22'
eq16 <- 'alpha23*(Z14^g1423)*(Z16^g1623)*Z27+alpha24*(Z15*g1524)*(Z16*g1624)*Z22-alpha25*(Z16*g1625)*Z33'
eq17 <- '0'
eq18 <- '0'
eq19 <- '0'
eq20 <- '0'
eq21 <- '0'
eq22 <- '0'
eq23 <- '0'
eq24 <- '0'
eq25 <- '0'
eq26 <- '0'
eq27 <- '0'
eq28 <- '0'
eq29 <- '0'
eq30 <- '0'
eq31 <- '0'
eq32 <- '0'
eq33 <- '0'
eq34 <- '0'
equations <- c(eq1,eq2,eq3,eq4,eq5,eq6,eq7,eq8,eq9,eq10,eq11,eq12,eq13,eq14,eq15,eq16)
equations <- c(equations,eq17,eq18,eq19,eq20,eq21,eq22,eq23,eq24,eq25,eq26,eq27,eq28,eq29,eq30,eq31,eq32,eq33,eq34)
names(equations) <- vars

pars1 <- c('alpha1','g11','g21')
pars2 <- c('alpha2','g22')
pars3 <- c('alpha3','g23','g33')
pars4 <- c('alpha4','g34','g44')
pars5 <- c('alpha5','g45','g55','g105')
pars6 <- c('alpha6','g56','g66')
pars7 <- c('alpha7','g67')
pars8 <- c('alpha8','g38')
pars9 <- c('alpha9','g49','g79')
pars10 <- c('alpha10','g410')
pars11 <- c('alpha11','g711','g811')
pars12 <- c('alpha12','g712','g912')
pars13 <- c('alpha13','g813','g1013')
pars14 <- c('alpha14','g914','g1014')
pars15 <- c('alpha15','g1015','g1115','g415')
pars16 <- c('alpha16','g1116','g1216')
pars17 <- c('alpha17','g1217')
pars18 <- c('alpha18','g918')
pars19 <- c('alpha19','g1119','g1319')
pars20 <- c('alpha20','g1220','g1420')
pars21 <- c('alpha21','g1421')
pars22 <- c('alpha22','g1322','g1522')
pars23 <- c('alpha23','g1423','g1623')
pars24 <- c('alpha24','g1524','g1624')
pars25 <- c('alpha25','g1625')
pars26 <- c('alpha26','g1026','g1226','g426')

pars <- c(pars1,pars2,pars3,pars4,pars5,pars6,pars7,pars8,pars9,pars10,pars11,pars12,pars13,pars14)
pars <- c(pars,pars15,pars16,pars17,pars18,pars19,pars20,pars21,pars22,pars23,pars24,pars25,pars26)

lin_pars <- paste0('alpha',1:26)
nlin_pars <- setdiff(pars,lin_pars)

theta_lin1 <- c(0.5233,0.1053,0.15,0.2711,0.1832,0.003,0.0042)
theta_lin2 <- c(0.0058,0.2265,0.0024,0.1054,0.1095,0.1452,0.2681)
theta_lin3 <- c(0.0771,0.0881,0.0168,0.002,0.2212,0.0402,0.0002)
theta_lin4 <- c(0.0392,0.1573,0.0712,0.1154,0.0814)
theta_lin <- c(theta_lin1, theta_lin2, theta_lin3, theta_lin4)
names(theta_lin) <- lin_pars
theta_nlin1 <-c(0.2813,-0.1406,0.0846,0.8240,-0.4120,0.5669,-0.2835,0.0710,-0.0355,-0.4505,0.9630,-0.4815)
theta_nlin2 <-c(0.4040,0.5759,0.6118,-0.3059,0.6398,0.6277,-0.3138,0.6414,-0.3207,0.4885,-0.2442,0.2046)
theta_nlin3 <-c(-0.1023,0.9009,-0.4505,-0.0355,0.5198,-0.2599,0.7121,0.6982,0.0160,-0.0080,0.6973,-0.3487)
theta_nlin4 <-c(0.8535,0.7673,-0.3836,0.1043,-0.0521,0.5080,-0.2540,0.2855,0.7116,-0.3558,-0.0355)
theta_nlin <- c(theta_nlin1,theta_nlin2,theta_nlin3,theta_nlin4)
names(theta_nlin) <- nlin_pars
theta <- c(theta_lin, theta_nlin)

x0 <- c(rep(100, 16), rep(1, 34-16))
names(x0) <- vars

n <- 100
time <- seq(0,1,length.out=n)

model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]

SNR <- 3
sigma_x <- apply(x_det, 2, sd)
sigma <- signif(sigma_x / SNR, digits=2)

print(sigma)

obs <- list()
for(i in 1:length(vars)) {
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma[i])
}
names(obs) <- vars

# v1 <- 'alpha1*(Z1^g11)*(Z2^g21)*Z17'
V1 <- theta['alpha1']*(x_det[,'Z1']^theta['g11'])*(x_det[,'Z2']^theta['g21'])*x_det[,'Z17']
V1_hat <- theta['alpha1']*(obs$Z1^theta['g11'])*(obs$Z2^theta['g21'])*obs$Z17

# v4 <- 'alpha4*(Z3^g34)*(Z4^g44)*Z20'
V4 <- theta['alpha4']*(x_det[,'Z3']^theta['g34'])*(x_det[,'Z4']^theta['g44'])*x_det[,'Z20']
V4_hat <- theta['alpha4']*(obs$Z3^theta['g34'])*(obs$Z4^theta['g44'])*obs$Z20

# v7 <- 'alpha7*(Z6^g67)*Z23'
V7 <- theta['alpha7']*(x_det[,'Z6']^theta['g67'])*x_det[,'Z23']
V7_hat <- theta['alpha7']*(obs$Z6^theta['g67'])*obs$Z23

# v9 <- 'alpha9*(Z4^g49)*(Z7^g79)*Z25'
V9 <- theta['alpha9']*(x_det[,'Z4']^theta['g49'])*(x_det[,'Z7']^theta['g79'])*x_det[,'Z25']
V9_hat <- theta['alpha9']*(obs$Z4^theta['g49'])*(obs$Z7^theta['g79'])*obs$Z25

# v17 <- 'alpha17*(Z12^g1217)*Z29'
V17 <- theta['alpha17']*(x_det[,'Z12']^theta['g1217'])*x_det[,'Z29']
V17_hat <- theta['alpha17']*(obs$Z12^theta['g1217'])*obs$Z29

par(mfrow=c(2,2))
plot(time, V4,'l', ylab="V4", ylim=c(0,1.10))
#points(time, V4_hat, pch=1)
plot(time, V7,'l', ylab="V7", ylim=c(0,1.10))
#points(time, V7_hat, pch=1)
plot(time, V9,'l', ylab="V9", ylim=c(0,1.10))
#points(time, V9_hat, pch=1)
plot(time, V17,'l', ylab="V17", ylim=c(0,1.10))
#points(time, V17_hat, pch=1)


pars_min <- c(0, -1.1, -1.1, 0, 0, -1.1, 0, 0, -1.1, 0, 0, -1.1, 0, 0, 0, 0, -1.1, 0, 0)
#pars_min <- pars_min * 2
names(pars_min) <- pars
pars_max <- c(6, 0, 0, 6, 1, 0, 6, 1, 0, 6, 1, 0, 6, 1, 6, 1, 0, 6, 1)
#pars_max <- pars_max * 2
names(pars_max) <- pars

priorInf=c(0.1,1,3,5)

nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                   + priorInf[1]*abs(theta[nlin_pars]))
names(nlin_init) <- nlin_pars

NLSest <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
       nlin_pars=nlin_pars, start=nlin_init, lower=pars_min, upper=pars_max,
       im_method = "non-separable",
       simode_ctrl=simode.control(optim_type = "im"))
par(mfrow=c(1,3))
plot(NLSest, type="fit", show="im")
SLSest <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                 nlin_pars=nlin_pars, start=nlin_init, lower=pars_min, upper=pars_max,
                 simode_ctrl=simode.control(optim_type = "im"))
plot(SLSest, type="fit", show="im")

unlink("log")
N <- 50
set.seed(1000)
cl <- makeForkCluster(8, outfile="log")
registerDoParallel(cl)

args <- c('equations', 'pars', 'time', 'x0', 'theta',
          'nlin_pars', 'x_det', 'vars', 'sigma')


results <- list()

for(ip in 1:4){
  
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
                         + priorInf[ip]*abs(theta[nlin_pars]))
      names(nlin_init) <- nlin_pars

      ptimeNLS <- system.time({
        NLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                        nlin_pars=nlin_pars, start=nlin_init,
                        lower=pars_min, upper=pars_max,
                        im_method = "non-separable",
                        simode_ctrl=simode.control(optim_type = "im"))})
      if (is.null(NLSmc) || !is.numeric(NLSmc$im_pars_est)) {
        print("should repeat NLS call")
        next
      }
      ptimeSLS <- system.time({
        SLSmc <- simode(equations=equations, pars=pars, fixed=x0, time=time, obs=obs,
                        nlin_pars=nlin_pars, start=nlin_init,
                        lower=pars_min, upper=pars_max,
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
                     NLSest_gamma21=NLS_im_vars['gamma21',],NLSest_f211=NLS_im_vars['f211',],NLSest_f221=NLS_im_vars['f221',],
                     NLSest_gamma22=NLS_im_vars['gamma22',],NLSest_f222=NLS_im_vars['f222',],
                     NLSest_gamma31=NLS_im_vars['gamma31',],NLSest_f311=NLS_im_vars['f311',],NLSest_f331=NLS_im_vars['f331',],
                     NLSest_gamma32=NLS_im_vars['gamma32',],NLSest_f332=NLS_im_vars['f332',],
                     SLSest_gamma11=SLS_im_vars['gamma11',],SLSest_f121=SLS_im_vars['f121',],SLSest_f131=SLS_im_vars['f131',],
                     SLSest_gamma12=SLS_im_vars['gamma12',],SLSest_f112=SLS_im_vars['f112',],SLSest_f122=SLS_im_vars['f122',],
                     SLSest_gamma13=SLS_im_vars['gamma13',],SLSest_f113=SLS_im_vars['f113',],SLSest_f133=SLS_im_vars['f133',],
                     SLSest_gamma21=SLS_im_vars['gamma21',],SLSest_f211=SLS_im_vars['f211',],SLSest_f221=SLS_im_vars['f221',],
                     SLSest_gamma22=SLS_im_vars['gamma22',],SLSest_f222=SLS_im_vars['f222',],
                     SLSest_gamma31=SLS_im_vars['gamma31',],SLSest_f311=SLS_im_vars['f311',],SLSest_f331=SLS_im_vars['f331',],
                     SLSest_gamma32=SLS_im_vars['gamma32',],SLSest_f332=SLS_im_vars['f332',]
  )  
  time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
  write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
  write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
}

stopCluster(cl)
#plot(unlist(NLSmc_im_loss_vals),type='l')
#lines(unlist(SLSmc_im_loss_vals),col="red")