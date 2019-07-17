rm(list=ls())

d0 <- read.csv("1-NLStoSLSloss.csv")
attach(d0)

hist(NLSest_alpha)
hist(SLSest_alpha)
var(NLSest_alpha)
var(SLSest_alpha)
v1=var(NLSest_alpha)/var(SLSest_alpha)

hist(NLSest_beta)
hist(SLSest_beta)
var(NLSest_beta)
var(SLSest_beta)
v2=var(NLSest_beta)/var(SLSest_beta)


hist(NLSest_gamma)
hist(SLSest_gamma)
var(NLSest_gamma)
var(SLSest_gamma)
v3=var(NLSest_gamma)/var(SLSest_gamma)

hist(NLSest_delta)
hist(SLSest_delta)
var(NLSest_delta)
var(SLSest_delta)
v4=var(NLSest_delta)/var(SLSest_delta)

vlin=v1+v2+v3+v4

hist(NLSest_epsilon)
hist(SLSest_epsilon)
var(NLSest_epsilon)
var(SLSest_epsilon)
v5=var(NLSest_epsilon)/var(SLSest_epsilon)

hist(NLSest_omega)
hist(SLSest_omega)
var(NLSest_omega)
var(SLSest_omega)
v6=var(NLSest_omega)/var(SLSest_omega)

vnonlin=v5+v6

vlin
vnonlin