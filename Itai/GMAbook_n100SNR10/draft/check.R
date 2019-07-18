rm(list=ls())

d0 <- read.csv("2-NLStoSLSloss.csv")
attach(d0)

hist(NLSest_gamma11)
hist(SLSest_gamma11)
var(NLSest_gamma11)
var(SLSest_gamma11)
v1=var(NLSest_gamma11)/var(SLSest_gamma11)
v1

hist(NLSest_gamma12)
hist(SLSest_gamma12)
var(NLSest_gamma12)
var(SLSest_gamma12)
v2=var(NLSest_gamma12)/var(SLSest_gamma12)
v2

hist(NLSest_gamma13)
hist(SLSest_gamma13)

hist(NLSest_gamma21)
hist(SLSest_gamma21)

hist(NLSest_gamma22)
hist(SLSest_gamma22)

hist(NLSest_gamma31)
hist(SLSest_gamma31)

hist(NLSest_gamma32)
hist(SLSest_gamma32)





#vlin
#vnonlin