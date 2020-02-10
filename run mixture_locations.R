rm(list=ls(all=TRUE))
library('MCMCpack')
set.seed(20)

setwd('U:\\GIT_models\\mixture_socioecon')
source('mixture_locations aux functions.R')
source('mixture_locations main function.R')
dat=read.csv('fake data.csv',as.is=T)
loc.id=dat$loc.id
nloc=max(loc.id)
ntot=nrow(dat)

ind=grep('y',colnames(dat))
dat1=data.matrix(dat[,ind])
nmax.group=15 #maximum number of groups

#priors
gamma1=0.1
alpha=0.1

#number of iterations
ngibbs=1000

#run gibbs sampler
mod=mixture_locations(dat1=dat1,nmax.group=nmax.group,loc.id=loc.id,
                      gamma1=gamma1,alpha=alpha,ngibbs=ngibbs)

