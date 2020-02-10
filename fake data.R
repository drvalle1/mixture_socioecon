rm(list=ls(all=TRUE))
library('MCMCpack')
set.seed(7)

setwd('U:\\GIT_models\\mixture_socioecon')
nloc=30
nobs.loc=rpois(nloc,lambda=30)+1
ntot=sum(nobs.loc)

#generate z's
ngroup=7
theta.true=theta=rdirichlet(nloc,rep(0.1,ngroup))
boxplot(theta.true)

res=numeric()
for (i in 1:nloc){
  k=rmultinom(1,size=nobs.loc[i],prob=theta[i,])
  tmp=rep(1:ngroup,k)
  tmp1=data.frame(z=tmp,loc.id=i)
  res=rbind(res,tmp1)
}
z.true=res$z

#for multinomial questions
nquestions=18
nmultin.cat=c(rep(2,15),5,3,7)
psi=list()
for (i in 1:nquestions){
  psi[[i]]=rdirichlet(ngroup,rep(0.1,nmultin.cat[i]))
}
psi.true=psi

#generate multinomial data
dat=matrix(NA,ntot,nquestions)
for (i in 1:ntot){
  tmp1=rep(NA,nquestions)
  for (j in 1:nquestions){
    tmp=rmultinom(1,size=1,prob=psi[[j]][res$z[i],])  
    tmp1[j]=which(tmp==1)
  }
  dat[i,]=tmp1
}
colnames(dat)=paste0('y',1:nquestions)

#combine and export results
fim=cbind(res,dat)
ind=which(colnames(fim)=='z')
fim1=fim[,-ind]

setwd('U:\\GIT_models\\mixture_socioecon')
write.csv(fim1,'fake data.csv',row.names=F)