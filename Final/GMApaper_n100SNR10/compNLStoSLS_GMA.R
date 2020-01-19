rm(list=ls())
library(simode)

n <- 100
SNR <- 10
priorInf=c(0.1,0.2,0.5)

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

sigma_x <- apply(x_det, 2, sd)
sigma <- signif(sigma_x / SNR, digits=2)
print(sigma)

lin_lower <- rep(0, length(lin_pars))
names(lin_lower) <- lin_pars
lin_upper <- rep(1, length(lin_pars))
names(lin_upper) <- lin_pars

nlin_lower <- ifelse(theta[nlin_pars] >= 0, 0, -1.5)
names(nlin_lower) <- nlin_pars
nlin_upper <- ifelse(theta[nlin_pars] >= 0, 1.5, 0)
names(nlin_upper) <- nlin_pars

lower <- c(lin_lower, nlin_lower)
upper <- c(lin_upper, nlin_upper)

N <- 50
library(doRNG)
require(doParallel)
registerDoParallel(cores=16)
args <- c('equations', 'pars', 'time', 'x0', 'theta',
          'nlin_pars', 'x_det', 'vars', 'sigma')

set.seed(1000)
sls_results <- foreach(j=1:N, .packages='simode', .export=args) %dorng% {
  # for(j in 1:N) {
  obs <- list()
  for(i in 1:length(vars)) {
    obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
  }
  names(obs) <- vars
  
  nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
                     + priorInf[1]*abs(theta[nlin_pars]))
  names(nlin_init) <- nlin_pars
  
  ptimeSLS <- system.time({
    SLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs,
                    fixed=x0, nlin=nlin_pars,
                    lower=lower, upper=upper, start=nlin_init,
                    simode_ctrl=simode.control(optim_type = "im"))})
  
  list(SLSmc=SLSmc,ptimeSLS=ptimeSLS)
}

SLS_im_vars=sapply(sls_results,function(x) x$SLSmc$im_pars_est)
SLSmc_im_loss_vals <- sapply(sls_results,function(x) x$SLSmc$im_loss)

loss_df=data.frame(SLSmc=unlist(SLSmc_im_loss_vals),
                   SLSest_alpha1=SLS_im_vars['alpha1',],SLSest_g11=SLS_im_vars['g11',],SLSest_g21=SLS_im_vars['g21',],
                   SLSest_alpha2=SLS_im_vars['alpha2',],SLSest_g22=SLS_im_vars['g22',],
                   SLSest_alpha3=SLS_im_vars['alpha3',],SLSest_g23=SLS_im_vars['g23',],SLSest_g33=SLS_im_vars['g33',],
                   SLSest_alpha4=SLS_im_vars['alpha4',],SLSest_g34=SLS_im_vars['g34',],SLSest_g44=SLS_im_vars['g44',],
                   SLSest_alpha5=SLS_im_vars['alpha5',],SLSest_g45=SLS_im_vars['g45',],SLSest_g55=SLS_im_vars['g55',],SLSest_g105=SLS_im_vars['g105',],
                   SLSest_alpha6=SLS_im_vars['alpha6',],SLSest_g56=SLS_im_vars['g56',],SLSest_g66=SLS_im_vars['g66',],
                   SLSest_alpha7=SLS_im_vars['alpha7',],SLSest_g67=SLS_im_vars['g67',],
                   SLSest_alpha8=SLS_im_vars['alpha8',],SLSest_g38=SLS_im_vars['g38',],
                   SLSest_alpha9=SLS_im_vars['alpha9',],SLSest_g49=SLS_im_vars['g49',],SLSest_g79=SLS_im_vars['g79',],
                   SLSest_alpha10=SLS_im_vars['alpha10',],SLSest_g410=SLS_im_vars['g410',],
                   SLSest_alpha11=SLS_im_vars['alpha11',],SLSest_g711=SLS_im_vars['g711',],SLSest_g811=SLS_im_vars['g811',],
                   SLSest_alpha12=SLS_im_vars['alpha12',],SLSest_g712=SLS_im_vars['g712',],SLSest_g912=SLS_im_vars['g912',],
                   SLSest_alpha13=SLS_im_vars['alpha13',],SLSest_g813=SLS_im_vars['g813',],SLSest_g1013=SLS_im_vars['g1013',],
                   SLSest_alpha14=SLS_im_vars['alpha14',],SLSest_g914=SLS_im_vars['g914',],SLSest_g1014=SLS_im_vars['g1014',],
                   SLSest_alpha15=SLS_im_vars['alpha15',],SLSest_g1015=SLS_im_vars['g1015',],SLSest_g1115=SLS_im_vars['g1115',],SLSest_g415=SLS_im_vars['g415',],
                   SLSest_alpha16=SLS_im_vars['alpha16',],SLSest_g1116=SLS_im_vars['g1116',],SLSest_g1216=SLS_im_vars['g1216',],
                   SLSest_alpha17=SLS_im_vars['alpha17',],SLSest_g1217=SLS_im_vars['g1217',],
                   SLSest_alpha18=SLS_im_vars['alpha18',],SLSest_g918=SLS_im_vars['g918',],
                   SLSest_alpha19=SLS_im_vars['alpha19',],SLSest_g1119=SLS_im_vars['g1119',],SLSest_g1319=SLS_im_vars['g1319',],
                   SLSest_alpha20=SLS_im_vars['alpha20',],SLSest_g1220=SLS_im_vars['g1220',],SLSest_g1420=SLS_im_vars['g1420',],
                   SLSest_alpha21=SLS_im_vars['alpha21',],SLSest_g1421=SLS_im_vars['g1421',],
                   SLSest_alpha22=SLS_im_vars['alpha22',],SLSest_g1322=SLS_im_vars['g1322',],SLSest_g1522=SLS_im_vars['g1522',],
                   SLSest_alpha23=SLS_im_vars['alpha23',],SLSest_g1423=SLS_im_vars['g1423',],SLSest_g1623=SLS_im_vars['g1623',],
                   SLSest_alpha24=SLS_im_vars['alpha24',],SLSest_g1524=SLS_im_vars['g1524',],SLSest_g1624=SLS_im_vars['g1624',],
                   SLSest_alpha25=SLS_im_vars['alpha25',],SLSest_g1625=SLS_im_vars['g1625',],
                   SLSest_alpha26=SLS_im_vars['alpha26',],SLSest_g1026=SLS_im_vars['g1026',],SLSest_g1226=SLS_im_vars['g1226',],SLSest_g426=SLS_im_vars['g426',]
)

write.csv(loss_df, file = paste0("sls_results.csv"))

# for(ip in 1:3){
#   
#   set.seed(1000)
#   
#   results <- foreach(j=1:N, .packages='simode', .export=args) %dorng% {
#     # for(j in 1:N) {
#     obs <- list()
#     for(i in 1:length(vars)) {
#       obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
#     }
#     names(obs) <- vars
#     
#     nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],
#                        + priorInf[1]*abs(theta[nlin_pars]))
#     names(nlin_init) <- nlin_pars
#     lin_init <- rnorm(length(lin_pars),theta[lin_pars],priorInf[ip]*abs(theta[lin_pars]))
#     names(lin_init) <- lin_pars
#     init <- c(lin_init, nlin_init)
#     #names(init) <- pars
#     
#     ptimeNLS <- system.time({
#       NLSmc <- simode(equations=equations, pars=pars, time=time, obs=obs,
#                       fixed=x0, nlin=nlin_pars,
#                       lower=lower, upper=upper, start=init,
#                       im_method = "non-separable",
#                       simode_ctrl=simode.control(optim_type = "im"))})
#     
#     list(NLSmc=NLSmc,ptimeNLS=ptimeNLS)
#   }
#   
#   
#   NLSmc_im_loss_vals <- sapply(results,function(x) x$NLSmc$im_loss)
#   SLSmc_im_loss_vals <- sapply(sls_results,function(x) x$SLSmc$im_loss)
#   NLS_im_vars=sapply(results,function(x) x$NLSmc$im_pars_est)
#   SLS_im_vars=sapply(sls_results,function(x) x$SLSmc$im_pars_est)
#   NLSmc_time=list()
#   SLSmc_time=list()
#   NLSmc_sse=list()
#   SLSmc_sse=list()
#   for (mc in 1:N){
#     NLSmc_time[mc]<-  results[[mc]]$ptimeNLS[3]
#     SLSmc_time[mc]<-  sls_results[[mc]]$ptimeSLS[3]
#   }
#   #mean(unlist(NLSmc_im_loss_vals))
#   #mean(unlist(SLSmc_im_loss_vals))
#   #mean(unlist(NLSmc_time))
#   #mean(unlist(SLSmc_time))
#   
#   
#   loss_df=data.frame(NLSest_alpha1=NLS_im_vars['alpha1',],NLSest_g11=NLS_im_vars['g11',],NLSest_g21=NLS_im_vars['g21',],
#                      NLSest_alpha2=NLS_im_vars['alpha2',],NLSest_g22=NLS_im_vars['g22',],
#                      NLSest_alpha3=NLS_im_vars['alpha3',],NLSest_g23=NLS_im_vars['g23',],NLSest_g33=NLS_im_vars['g33',],
#                      NLSest_alpha4=NLS_im_vars['alpha4',],NLSest_g34=NLS_im_vars['g34',],NLSest_g44=NLS_im_vars['g44',],
#                      NLSest_alpha5=NLS_im_vars['alpha5',],NLSest_g45=NLS_im_vars['g45',],NLSest_g55=NLS_im_vars['g55',],NLSest_g105=NLS_im_vars['g105',],
#                      NLSest_alpha6=NLS_im_vars['alpha6',],NLSest_g56=NLS_im_vars['g56',],NLSest_g66=NLS_im_vars['g66',],
#                      NLSest_alpha7=NLS_im_vars['alpha7',],NLSest_g67=NLS_im_vars['g67',],
#                      NLSest_alpha8=NLS_im_vars['alpha8',],NLSest_g38=NLS_im_vars['g38',],
#                      NLSest_alpha9=NLS_im_vars['alpha9',],NLSest_g49=NLS_im_vars['g49',],NLSest_g79=NLS_im_vars['g79',],
#                      NLSest_alpha10=NLS_im_vars['alpha10',],NLSest_g410=NLS_im_vars['g410',],
#                      NLSest_alpha11=NLS_im_vars['alpha11',],NLSest_g711=NLS_im_vars['g711',],NLSest_g811=NLS_im_vars['g811',],
#                      NLSest_alpha12=NLS_im_vars['alpha12',],NLSest_g712=NLS_im_vars['g712',],NLSest_g912=NLS_im_vars['g912',],
#                      NLSest_alpha13=NLS_im_vars['alpha13',],NLSest_g813=NLS_im_vars['g813',],NLSest_g1013=NLS_im_vars['g1013',],
#                      NLSest_alpha14=NLS_im_vars['alpha14',],NLSest_g914=NLS_im_vars['g914',],NLSest_g1014=NLS_im_vars['g1014',],
#                      NLSest_alpha15=NLS_im_vars['alpha15',],NLSest_g1015=NLS_im_vars['g1015',],NLSest_g1115=NLS_im_vars['g1115',],NLSest_g415=NLS_im_vars['g415',],
#                      NLSest_alpha16=NLS_im_vars['alpha16',],NLSest_g1116=NLS_im_vars['g1116',],NLSest_g1216=NLS_im_vars['g1216',],
#                      NLSest_alpha17=NLS_im_vars['alpha17',],NLSest_g1217=NLS_im_vars['g1217',],
#                      NLSest_alpha18=NLS_im_vars['alpha18',],NLSest_g918=NLS_im_vars['g918',],
#                      NLSest_alpha19=NLS_im_vars['alpha19',],NLSest_g1119=NLS_im_vars['g1119',],NLSest_g1319=NLS_im_vars['g1319',],
#                      NLSest_alpha20=NLS_im_vars['alpha20',],NLSest_g1220=NLS_im_vars['g1220',],NLSest_g1420=NLS_im_vars['g1420',],
#                      NLSest_alpha21=NLS_im_vars['alpha21',],NLSest_g1421=NLS_im_vars['g1421',],
#                      NLSest_alpha22=NLS_im_vars['alpha22',],NLSest_g1322=NLS_im_vars['g1322',],NLSest_g1522=NLS_im_vars['g1522',],
#                      NLSest_alpha23=NLS_im_vars['alpha23',],NLSest_g1423=NLS_im_vars['g1423',],NLSest_g1623=NLS_im_vars['g1623',],
#                      NLSest_alpha24=NLS_im_vars['alpha24',],NLSest_g1524=NLS_im_vars['g1524',],NLSest_g1624=NLS_im_vars['g1624',],
#                      NLSest_alpha25=NLS_im_vars['alpha25',],NLSest_g1625=NLS_im_vars['g1625',],
#                      NLSest_alpha26=NLS_im_vars['alpha26',],NLSest_g1026=NLS_im_vars['g1026',],NLSest_g1226=NLS_im_vars['g1226',],NLSest_g426=NLS_im_vars['g426',],
#                      SLSest_alpha1=SLS_im_vars['alpha1',],SLSest_g11=SLS_im_vars['g11',],SLSest_g21=SLS_im_vars['g21',],
#                      SLSest_alpha2=SLS_im_vars['alpha2',],SLSest_g22=SLS_im_vars['g22',],
#                      SLSest_alpha3=SLS_im_vars['alpha3',],SLSest_g23=SLS_im_vars['g23',],SLSest_g33=SLS_im_vars['g33',],
#                      SLSest_alpha4=SLS_im_vars['alpha4',],SLSest_g34=SLS_im_vars['g34',],SLSest_g44=SLS_im_vars['g44',],
#                      SLSest_alpha5=SLS_im_vars['alpha5',],SLSest_g45=SLS_im_vars['g45',],SLSest_g55=SLS_im_vars['g55',],SLSest_g105=SLS_im_vars['g105',],
#                      SLSest_alpha6=SLS_im_vars['alpha6',],SLSest_g56=SLS_im_vars['g56',],SLSest_g66=SLS_im_vars['g66',],
#                      SLSest_alpha7=SLS_im_vars['alpha7',],SLSest_g67=SLS_im_vars['g67',],
#                      SLSest_alpha8=SLS_im_vars['alpha8',],SLSest_g38=SLS_im_vars['g38',],
#                      SLSest_alpha9=SLS_im_vars['alpha9',],SLSest_g49=SLS_im_vars['g49',],SLSest_g79=SLS_im_vars['g79',],
#                      SLSest_alpha10=SLS_im_vars['alpha10',],SLSest_g410=SLS_im_vars['g410',],
#                      SLSest_alpha11=SLS_im_vars['alpha11',],SLSest_g711=SLS_im_vars['g711',],SLSest_g811=SLS_im_vars['g811',],
#                      SLSest_alpha12=SLS_im_vars['alpha12',],SLSest_g712=SLS_im_vars['g712',],SLSest_g912=SLS_im_vars['g912',],
#                      SLSest_alpha13=SLS_im_vars['alpha13',],SLSest_g813=SLS_im_vars['g813',],SLSest_g1013=SLS_im_vars['g1013',],
#                      SLSest_alpha14=SLS_im_vars['alpha14',],SLSest_g914=SLS_im_vars['g914',],SLSest_g1014=SLS_im_vars['g1014',],
#                      SLSest_alpha15=SLS_im_vars['alpha15',],SLSest_g1015=SLS_im_vars['g1015',],SLSest_g1115=SLS_im_vars['g1115',],SLSest_g415=SLS_im_vars['g415',],
#                      SLSest_alpha16=SLS_im_vars['alpha16',],SLSest_g1116=SLS_im_vars['g1116',],SLSest_g1216=SLS_im_vars['g1216',],
#                      SLSest_alpha17=SLS_im_vars['alpha17',],SLSest_g1217=SLS_im_vars['g1217',],
#                      SLSest_alpha18=SLS_im_vars['alpha18',],SLSest_g918=SLS_im_vars['g918',],
#                      SLSest_alpha19=SLS_im_vars['alpha19',],SLSest_g1119=SLS_im_vars['g1119',],SLSest_g1319=SLS_im_vars['g1319',],
#                      SLSest_alpha20=SLS_im_vars['alpha20',],SLSest_g1220=SLS_im_vars['g1220',],SLSest_g1420=SLS_im_vars['g1420',],
#                      SLSest_alpha21=SLS_im_vars['alpha21',],SLSest_g1421=SLS_im_vars['g1421',],
#                      SLSest_alpha22=SLS_im_vars['alpha22',],SLSest_g1322=SLS_im_vars['g1322',],SLSest_g1522=SLS_im_vars['g1522',],
#                      SLSest_alpha23=SLS_im_vars['alpha23',],SLSest_g1423=SLS_im_vars['g1423',],SLSest_g1623=SLS_im_vars['g1623',],
#                      SLSest_alpha24=SLS_im_vars['alpha24',],SLSest_g1524=SLS_im_vars['g1524',],SLSest_g1624=SLS_im_vars['g1624',],
#                      SLSest_alpha25=SLS_im_vars['alpha25',],SLSest_g1625=SLS_im_vars['g1625',],
#                      SLSest_alpha26=SLS_im_vars['alpha26',],SLSest_g1026=SLS_im_vars['g1026',],SLSest_g1226=SLS_im_vars['g1226',],SLSest_g426=SLS_im_vars['g426',]
#   )  
#   time_df=data.frame(NLStime=unlist(NLSmc_time),SLStime=unlist(SLSmc_time))
#   write.csv(loss_df, file = paste0(ip, "-NLStoSLSloss.csv"))
#   write.csv(time_df, file = paste0(ip, "-NLStoSLStime.csv"))
# }

