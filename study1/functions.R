library("MASS")

#### functions ####
## Cumulative Distribution Function (CDF) of compound mixture distribution (nnmax) 
## for the MAX of k independent draws from normal distribution with sd=sdm and mean ~ N(mean, sd)

pnnmax_scalar = function(x, mean, sd, sdm, k) {
  f=function(mean2) dnorm(mean2, mean=mean, sd=sd)*pnorm(x, mean=mean2, sd=sdm)^k
  tryCatch(integrate(Vectorize(f), lower=-Inf, upper=Inf)$value, error = function(e) NA)
}
pnnmax=Vectorize(pnnmax_scalar)

## Power Density Function (PDF) of nnmax
dnnmax_scalar = function(x, mean, sd, sdm, k, delta=.00001) {
  f=function(mean2) {
    f=function(x) {dnorm(mean2, mean=mean, sd=sd)*pnorm(x, mean=mean2, sd=sdm)^k} 
    (f(x+delta/2*sd)-f(x-delta/2*sd))/(delta*sd)
  }
  tryCatch(integrate(Vectorize(f), lower=-Inf, upper=Inf)$value, error = function(e) NA)
}
dnnmax=Vectorize(dnnmax_scalar)

## find sdm & k of the nnmax distribution given the mean & sd of the parent distribution
nnmax.mle = function(data, mean=0, sd=1, sdm=1, k=1, method="BFGS", trace=6, report=1) {
  f=function(data, sdm, k) dnnmax(x=data, mean=mean, sd=sd, sdm=sdm, k=k)
  list(fitted=fitdistr(data, f, method=method, start=list(sdm=sdm, k=k), 
           control=list(trace=trace, REPORT=report)),
    fixed=list(mean=mean, sd=sd))
}

## median of nnmax
median_nnmax_scalar=function(mean, sd, sdm, k, range=10){
  f=function(x) abs(pnnmax(x, mean, sd, sdm, k)-.5)
  m=optimize(f, interval=c(mean-sd*range, mean+sd*range))
  return(m$minimum)
}

median_nnmax=Vectorize(median_nnmax_scalar)

## fit k and sdm specific for three datasets with different sample sizes
nnmax3.mle = function(data, mean=list(0,0,0), sd=1, sdm=list(1,1,1), k=1, method="BFGS", trace=6, report=1) {
  nll=function(sdm1, sdm2, sdm3, k) {
    sum(-log(dnnmax(x=data[[1]], mean=mean[[1]], sd=sd, sdm=sdm1, k=k))) + 
      sum(-log(dnnmax(x=data[[2]], mean=mean[[2]], sd=sd, sdm=sdm2, k=k))) +
      sum(-log(dnnmax(x=data[[3]], mean=mean[[3]], sd=sd, sdm=sdm3, k=k)))
  }
  
  fit=mle(nll, start=list(sdm1=sdm[[1]], sdm2=sdm[[2]], sdm3=sdm[[3]], k=k), nobs=NROW(data),
          method=method, control=list(trace=trace, REPORT=report))
  list(fitted=fit, fixed=list(mean=mean, sd=sd))
}

## fit k with sdm constrained identical for three datasets with different sample sizes
nnmax3constr.mle = function(data, mean=list(0,0,0), sd=1, sdm=1, k=1, method="BFGS", trace=6, report=1) {
  nll=function(sdm, k) {
    sum(-log(dnnmax(x=data[[1]], mean=mean[[1]], sd=sd, sdm=sdm, k=k))) + 
      sum(-log(dnnmax(x=data[[2]], mean=mean[[2]], sd=sd, sdm=sdm, k=k))) +
      sum(-log(dnnmax(x=data[[3]], mean=mean[[3]], sd=sd, sdm=sdm, k=k)))
  }
  
  fit=mle(nll, start=list(sdm=sdm, k=k), nobs=NROW(data),
          method=method, control=list(trace=trace, REPORT=report))
  list(fitted=fit, fixed=list(mean=mean, sd=sd))
}

## fit k with sdm specific for two datasets with different sample sizes
nnmax2.mle = function(data, mean=list(0,0), sd=1, sdm=list(1,1), k=1, method="BFGS", trace=6, report=1) {
  nll=function(sdm1, sdm2, k) {
    sum(-log(dnnmax(x=data[[1]], mean=mean[[1]], sd=sd, sdm=sdm1, k=k))) + 
      sum(-log(dnnmax(x=data[[2]], mean=mean[[2]], sd=sd, sdm=sdm2, k=k))) 
  }
  
  fit=mle(nll, start=list(sdm1=sdm[[1]], sdm2=sdm[[2]], k=k), nobs=NROW(data),
          method=method, control=list(trace=trace, REPORT=report))
  list(fitted=fit, fixed=list(mean=mean, sd=sd))
}

## fit k and sdm constrained identical for two datasets with different sample sizes
nnmax2constr.mle = function(data, mean=list(0,0), sd=1, sdm=1, k=1, method="BFGS", trace=6, report=1) {
  nll=function(sdm, k) {
    sum(-log(dnnmax(x=data[[1]], mean=mean[[1]], sd=sd, sdm=sdm, k=k))) + 
      sum(-log(dnnmax(x=data[[2]], mean=mean[[2]], sd=sd, sdm=sdm, k=k)))
  }
  
  fit=mle(nll, start=list(sdm=sdm, k=k), nobs=NROW(data),
          method=method, control=list(trace=trace, REPORT=report))
  list(fitted=fit, fixed=list(mean=mean, sd=sd))
}

# fit approximate estimation based on ICC, k & zeta on three datasests with different n
nnmax3approx.mle = function(data, ICC=.50, k=1, zeta=.01, pr=.02, n=c(5000, 20000, 10^5),
                            method="BFGS", trace=6, report=1) {
  nll=function(zeta) {
    sum(-log(dnnmax(x=data[[1]], mean=zeta*sqrt(n[[1]]*pr), sd=sqrt(ICC), sdm=sqrt(1-ICC), k=k))) + 
      sum(-log(dnnmax(x=data[[2]], mean=zeta*sqrt(n[[2]]*pr), sd=sqrt(ICC), sdm=sqrt(1-ICC), k=k))) +
      sum(-log(dnnmax(x=data[[3]], mean=zeta*sqrt(n[[3]]*pr), sd=sqrt(ICC), sdm=sqrt(1-ICC), k=k)))
  }
  
  fit=mle(nll, start=list(zeta=zeta), nobs=NROW(data),
          method=method, control=list(trace=trace, REPORT=report))
  list(fitted=fit, fixed=list(sd=sd, sdm=sdm, k=k, n=n, pr=pr))
}

# fit approximate estimation based on ICC, k & zeta on two datasests with different n
nnmax2approx.mle = function(data, ICC=.50, k=1, zeta=.01, pr=.02, n=c(5000, 20000),
                            method="BFGS", trace=6, report=1) {
  nll=function(zeta) {
    sum(-log(dnnmax(x=data[[1]], mean=zeta*sqrt(n[[1]]*pr), sd=sqrt(ICC), sdm=sqrt(1-ICC), k=k))) + 
      sum(-log(dnnmax(x=data[[2]], mean=zeta*sqrt(n[[2]]*pr), sd=sqrt(ICC), sdm=sqrt(1-ICC), k=k))) 
  }
  
  fit=mle(nll, start=list(zeta=zeta), nobs=NROW(data),
          method=method, control=list(trace=trace, REPORT=report))
  list(fitted=fit, fixed=list(ICC=ICC, k=k, n=n, pr=pr))
}
