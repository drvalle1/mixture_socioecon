#look at z's
res=data.frame(zestim=mod$z[ngibbs,],ztrue=z.true)
res1=table(res); res1

#re-order data
ind=numeric()
for (i in 1:ncol(res1)){
  tmp=which(res1[,i]==max(res1[,i]))
  ind=c(ind,tmp)
}
res1[ind,]

#look at theta
theta.estim=matrix(mod$theta[ngibbs,],nloc,nmax.group)
plot(theta.true,theta.estim[,ind])

#look at psi
psi.estim=mod$psi[ngibbs,]

#convert to original list
ncat=apply(dat1,2,max)
ind.aux=1:length(psi.estim)
nquest=ncol(dat1)
psi.estim1=list()
for (i in 1:nquest){
  ind1=1:(nmax.group*ncat[i])
  tmp=psi.estim[ind1]
  psi.estim1[[i]]=matrix(tmp,nmax.group,ncat[i])
  psi.estim=psi.estim[-ind1]
}

psi.true1=psi.true[[1]]
psi.estim2=psi.estim1[[1]][ind,]
for (i in 2:nquest){
  psi.true1=cbind(psi.true1,psi.true[[i]])
  psi.estim2=cbind(psi.estim2,psi.estim1[[i]][ind,])
}
plot(psi.true1,psi.estim2)