mixture_locations=function(dat1,nmax.group,loc.id,gamma1,alpha,ngibbs){
  nloc=max(loc.id)
  ntot=nrow(dat1)
  nquest=ncol(dat1)

  #number of categories for each questions
  nquest.cat=apply(dat1,2,max)
  
  #get initial values
  ngroup=nmax.group
  z=sample(1:ngroup,size=ntot,replace=T)
  theta=matrix(1/ngroup,nloc,ngroup)
  psi=list()
  for (i in 1:nquest){
    psi[[i]]=matrix(1/nquest.cat[i],ngroup,nquest.cat[i])
  }
  
  #useful calculations
  log.p.new.group=-sum(log(nquest.cat))
  lo=0.000000001
  hi=1-lo
  
  #MCMC stuff
  store.z=matrix(NA,ngibbs,ntot)
  store.theta=matrix(NA,ngibbs,ngroup*nloc)
  store.psi=matrix(NA,ngibbs,ngroup*sum(nquest.cat))
  reorder1=50
  
  #run gibbs sampler
  for (i in 1:ngibbs){
    print(i)
    
    #re-order stuff from time to time
    if (i%%reorder1==0){
      ind=order(apply(theta,2,median),decreasing=T)
      theta=theta[,ind]
      for (j in 1:nquest) psi[[j]]=psi[[j]][ind,]
      znew=z
      for (j in 1:ngroup){
        cond=z==ind[j]
        znew[cond]=j
      }
      z=znew
    }  
    
    #sample psi
    psi=sample.psi(z=z,dat1=dat1,nquest.cat=nquest.cat,ngroup=ngroup,
                   nquest=nquest,alpha=alpha,psi=psi)
    
    #sample v
    v=sample.v(z=z,loc.id=loc.id,nloc=nloc,ngroup=ngroup,
               gamma1=gamma1)
    theta=v.to.theta(v=v,ngroup=ngroup,nloc=nloc)

    #to avoid numerical issues with log
    log.psi=list()
    for (j in 1:nquest){
      psi.tmp=psi[[j]]
      cond=psi.tmp<lo; psi.tmp[cond]=lo
      cond=psi.tmp>hi; psi.tmp[cond]=hi
      log.psi[[j]]=log(psi.tmp)
    }
    cond=theta<lo;   theta[cond]=lo
    cond=theta>hi;   theta[cond]=hi
    
    #sample z
    log.theta=log(theta)
    z=sample.z(log.theta=log.theta,log.psi=log.psi,
               ntot=ntot,log.p.new.group=log.p.new.group,z=z,ngroup=ngroup,
               loc.id=loc.id,nquest=nquest,dat1=dat1)
    
    #store results
    store.z[i,]=z
    store.theta[i,]=theta
    store.psi[i,]=unlist(psi)
  }
  list(z=store.z,theta=store.theta,psi=store.psi)
}
