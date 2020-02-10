sample.psi=function(z,dat1,nquest.cat,ngroup,nquest,alpha,psi){
  for (i in 1:ngroup){
    cond=z==i
    soma=sum(cond)
    
    for (j in 1:nquest){
      if (soma==0) res=rep(alpha,nquest.cat[j])
      if (soma==1) {
        dat2=dat1[cond,j]
        res=rep(0,nquest.cat[j])
        res[dat2]=1
        res=res+alpha
      }
      if (soma>1){
        dat2=dat1[cond,j]
        tmp1=table(dat2)
        res=rep(0,nquest.cat[j])
        ind=as.numeric(names(tmp1))
        res[ind]=tmp1+alpha
      }
      psi[[j]][i,]=rdirichlet(1,res)
    }
  }
  psi
}
#------------------------------
sample.v=function(z,loc.id,nloc,ngroup,gamma1){
  #get number of z's in each location and each group
  res=matrix(0,nloc,ngroup)
  for (i in 1:nloc){
    cond=loc.id==i
    z1=z[cond]
    z2=table(z1)
    ind=as.numeric(names(z2))
    res[i,ind]=z2
  }
  
  #get z_il>k
  tmp=t(apply(res[,ngroup:1],1,cumsum))[,ngroup:1]
  
  #generate vlk
  tmp1=rbeta((ngroup-1)*nloc,res[,-ngroup]+1,tmp[,-1]+gamma1)
  tmp2=matrix(tmp1,nloc,ngroup-1)
  cbind(tmp2,1)
}
#------------------------------
v.to.theta=function(v,ngroup,nloc){
  theta=matrix(NA,nloc,ngroup)
  theta[,1]=v[,1]
  tmp=1-v[,1]
  for (i in 2:ngroup){
    theta[,i]=v[,i]*tmp
    tmp=tmp*(1-v[,i])
  }
  theta
}
#------------------------------
sample.z=function(log.theta,log.psi,ntot,log.p.new.group,z,ngroup,
                  loc.id,nquest,dat1){
  for (i in 1:ntot){
    max.z=max(z)
    log.psi1=rep(0,ngroup)
    for (j in 1:nquest){
      log.psi1=log.psi1+log.psi[[j]][,dat1[i,j]]
    }
    if (max.z==ngroup){
      log.theta1=log.theta[loc.id[i],]
      log.probs=log.psi1+log.theta1
    }
    if (max.z<ngroup){
      log.theta1=log.theta[loc.id[i],1:(max.z+1)]
      log.psi2=log.psi1[1:max.z]
      log.probs=c(log.psi2,log.p.new.group)+log.theta1  
    }
    log.probs1=log.probs-max(log.probs)
    probs1=exp(log.probs1)
    probs2=probs1/sum(probs1)
    tmp=rmultinom(1,size=1,prob=probs2)
    z[i]=which(tmp==1)
  }
  z
}