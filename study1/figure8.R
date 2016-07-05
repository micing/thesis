source("study1/functions.R")
source("equations.R")
png(filename = "output/figure8.png",width = 550, height = 900)
## parameters for medium bias
zeta_m=.03
ICC_m=.50
k_m=20

## parameters for low bias
zeta_l=.015
ICC_l=.75
k_l=15

## minimal confounding
zeta_minimal=.0075

#### prepare data for plotting ####
x=seq(100, 5000, 100)

# minimal p-hacking bias with zero confounding
min_zero_bias=median_nnmax(mean=rep(0, length(x)), sd=sqrt(ICC_l), sdm=sqrt(1-ICC_l), k=k_l)/2

## standardized bias
bias = list(
  min=median_nnmax(mean=min_zero_bias+zeta_minimal*sqrt(x), sd=sqrt(ICC_l), sdm=sqrt(1-ICC_l), k=1),
  ll=median_nnmax(mean=zeta_l*sqrt(x), sd=sqrt(ICC_l), sdm=sqrt(1-ICC_l), k=k_l),
  lm=median_nnmax(mean=zeta_m*sqrt(x), sd=sqrt(ICC_l), sdm=sqrt(1-ICC_l), k=k_l),
  ml=median_nnmax(mean=zeta_l*sqrt(x), sd=sqrt(ICC_m), sdm=sqrt(1-ICC_m), k=k_m),
  mm=median_nnmax(mean=zeta_m*sqrt(x), sd=sqrt(ICC_m), sdm=sqrt(1-ICC_m), k=k_m)
)

## type-1 error rate
type1=list(
  min=100-pnnmax(1.96, mean=min_zero_bias+zeta_minimal*sqrt(x), sd=sqrt(ICC_l), sdm=sqrt(1-ICC_l), k=1)*100,
  ll=100-pnnmax(1.96, mean=zeta_l*sqrt(x), sd=sqrt(ICC_l), sdm=sqrt(1-ICC_l), k=k_l)*100,
  lm=100-pnnmax(1.96, mean=zeta_m*sqrt(x), sd=sqrt(ICC_l), sdm=sqrt(1-ICC_l), k=k_l)*100,
  ml=100-pnnmax(1.96, mean=zeta_l*sqrt(x), sd=sqrt(ICC_m), sdm=sqrt(1-ICC_m), k=k_m)*100,
  mm=100-pnnmax(1.96, mean=zeta_m*sqrt(x), sd=sqrt(ICC_m), sdm=sqrt(1-ICC_m), k=k_m)*100
)

## Relative risks
d=read.csv("data/breast_cancer/breast_cancer_population.csv")
d=subset(d, d$age>25 & d$age<=70)
p=d$n/sum(d$n)
pr=sum(p*d$incidence100k10y/100000) # probability of cancer case

## sigma is based on the study by Åkerstedt et al (2013) featured in figure 1
sigma=sqrt(varlnor(n=x/pr, p1=pr, p2=305/13656))

rr = list(
  min=ORtoRR(exp(bias[["min"]]*sigma), p=pr),
  ll=ORtoRR(exp(bias[["ll"]]*sigma), p=pr),
  lm=ORtoRR(exp(bias[["lm"]]*sigma), p=pr),
  ml=ORtoRR(exp(bias[["ml"]]*sigma), p=pr),
  mm=ORtoRR(exp(bias[["mm"]]*sigma), p=pr)
)

#### plotit  ####

plotit = function(x, d, log="", pr=.0231596, legpos="topleft", 
                  ylim=c(0,1), ylab="", xlab1="", xlab2="") {
  
plot(x, d[["mm"]], ylim=ylim, type="n", log=log, ylab=ylab, xlab="", cex=1.3)
nlab=c(5000, 10000, 20000, 50000, 100000, 200000)
ntxt=c("5K", "10K", "20K", "50K", "100K", "200K")
  axis(1, at=nlab*pr, line=2.3, labels=ntxt)
  
mtext("Number of cases / sample size in thousands (K)", side=1, line=4.7 , cex=1)
  lines(x, d[["min"]], lty=1, lwd=2)
  lines(x, d[["ll"]], lty=2, lwd=2)
  lines(x, d[["lm"]], lty=3, lwd=2)
  lines(x, d[["ml"]], lty=4, lwd=2)
  lines(x, d[["mm"]], lty=5, lwd=2)

legend(legpos,
         rev(c("min/min", "low/low", "low/med", "med/low", "med/med")),
         bty="n", bg=NA, cex=.8, y.intersp=1.5,
         lty=rev(c(1, 2, 3, 4, 5)),
         lwd=c(2),
         seg.len=4,
         inset=c(.03,.03)
         )
}

op=par(no.readonly=TRUE) 
par(mfcol=c(3,1), mar=c(6,5.5,1,2), cex=1)
plotit(x, type1, log="x", ylim=c(0,100), ylab="Positive type-1 error rate (%)")
plotit(x, bias, log="x", ylim=c(0,4), ylab="Expected standardized bias (z-score)")
plotit(x, rr, log="xy", ylim=c(1,3), legpos="topright", ylab="Expected Relative Risk (RR)\n*based on Åkerstedt et al (2013)")

par(op)
dev.off()
