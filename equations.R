#### Equations included in the thesis: ####
#### "P-hacking in academic research and its implications for statistical inference"
#### by: Michael Ingre, 2016

# eq1-4 are variations of Bayes' theorem
eq1 = function(a, ba, b) ba*a/b 

eq2 = function(h, eh, e) eh*h/e

eq3 = function(alpha, beta, theta) theta*(1-beta) / (theta*(1-beta) + alpha*(1-theta))

eq4 = function(alpha, beta, theta) 1-((1-theta)*(1-alpha)) / ((1-theta)*(1-alpha) + beta*theta)

# expected value above critical threshold to model publication bias
eq5 = function(mu, sigma, alpha){
  critical = function(mu, sigma, alpha){qnorm(1-alpha/2, mu, sigma)}
  mu+sigma*dnorm(critical(mu=0, sigma=1, alpha=alpha)) /
    (1-pnorm(critical(mu=0, sigma=1, alpha=alpha)))
}

# statistical power
eq6= function(){}

## variance of the log odds ratio
eq7 = function(n, p1, p2){
  1/(p1*p2*n) +
  1/((1-p1)*p2*n) +
  1/((1-p1)*(1-p2)*n) +
  1/(p1*(1-p2)*n)
}

## convert OR to RR
eq8 = function(or, p) or / (1 - p + (p * or))

## CDF of compound distribution
eq9 = function(x, mu_b, sigma_b, sigma_w, k) {
  f=function(mu_w) dnorm(mu_w, mean=mu_b, sd=sigma_b)*pnorm(x, mean=mu_w, sd=sigma_w)^k
  integrate(Vectorize(f), lower=-Inf, upper=Inf)$value
}

## CDF of compound finite mixture distribution
## NB: this equation has not been validated on data!
eq10 = function(x, mu_b, sigma_b, sigma_w, k, zeta) {
  f=function(mu_w) dnorm(mu_w, mean=mu_b, sd=sigma_b)*sum(pnorm(x, mean=mu_w+zeta, sd=sigma_w)^k)/length(zeta)
  zeta = zeta-(mean(zeta)) # cenetring the bias vector on mu_w
  tryCatch(integrate(Vectorize(f), lower=-Inf, upper=Inf)$value, error = function(e) NA)
}

#### Higher level functions based on the above equations #####

bayes = function(theta=.5, beta = .05, alpha = .05, N=1, positive=1) {
  b=eq3
  if (positive==0) { b=eq4}
  p=theta
  if (N>0) { 
    for (i in seq(2,N+1)){
      p[i]= b(theta=p[i-1], alpha=alpha, beta=beta)
    } 
  }
  return(p[length(p)])
}

ORtoRR=function(or, p) eq8(or=or, p=p)
varlnor=function(n, p1, p2) eq7(n=n, p1=p1, p2=p2)