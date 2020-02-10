#look at z's
res=data.frame(zestim=zestim,ztrue=z.true)
res1=table(res); res1

#re-order data
ind=numeric()
for (i in 1:ncol(res1)){
  tmp=which(res1[,i]==max(res1[,i]))
  ind=c(ind,tmp)
}
res1[ind,]

#look at theta
plot(theta.true,theta.estim[,ind])

#look at psi
psi.true1=psi.true[[i]]
psi.estim1=psi.estim[[i]][ind,]

for (i in 2:nquest){
  psi.true1=cbind(psi.true1,psi.true[[i]])
  psi.estim1=cbind(psi.estim1,psi.estim[[i]][ind,])
}
plot(psi.true1,psi.estim1)