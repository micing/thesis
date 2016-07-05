library("lme4")
source("study1/functions.R")
load("data/breast_cancer_sim/aggregated_simulations")
load("data/breast_cancer_sim/sim_10_8_1_100")
load("data/breast_cancer_sim/sim_10_8_1_20")
load("data/breast_cancer_sim/sim_10_8_1_5")
load("data/breast_cancer_sim/nnmax_zadj_3x10_8_1")
lmerfit5z = lmer(z ~ 1 + (1 | sim), data=sim_10_8_1_5)
lmerfit20z = lmer(z ~ 1 + (1 | sim), data=sim_10_8_1_20)
lmerfit100z = lmer(z ~ 1 + (1 | sim), data=sim_10_8_1_100)
lmerfit5 = lmer(zadj ~ 1 + (1 | sim), data=sim_10_8_1_5)
lmerfit20 = lmer(zadj ~ 1 + (1 | sim), data=sim_10_8_1_20)
lmerfit100 = lmer(zadj ~ 1 + (1 | sim), data=sim_10_8_1_100)

png(filename = "output/figure5.png",width = 550, height = 450)
op=par(no.readonly=TRUE) 

a=aggregated

a$sim_10_8_1_5$max$z_constant_bias=a$sim_10_8_1_5$max$zadj+fixef(lmerfit5z)[["(Intercept)"]]
a$sim_10_8_1_20$max$z_constant_bias=a$sim_10_8_1_5$max$zadj+fixef(lmerfit20z)[["(Intercept)"]]
a$sim_10_8_1_100$max$z_constant_bias=a$sim_10_8_1_5$max$zadj+fixef(lmerfit100z)[["(Intercept)"]]




par(mfcol=c(3,3), mar=c(3,4,2.5,.5))

plotit = function(x, df, main, ylim=c(0,.5), xlim=c(-3,3.5), sub="", sig=FALSE) {
  h=hist(x, breaks=30, prob=TRUE, main=main, xlab="", xlim=xlim, ylim=ylim)
  if (sig) {
    sigx=seq(qnorm(.975), max(x), .05)
    sigd=df(sigx)
    p=integrate(df, qnorm(.975), Inf)[[1]]+integrate(df,  -Inf, -qnorm(.975))[[1]]
    polygon(x=c(sigx, rev(sigx)), y=c(sigd, rep(0, length(sigd))), col="firebrick3", lty=1, density=40)
    legend("topright", legend=paste0(round(p, 2)*100, "%"), 
           fill="firebrick3", bty="n")
  }
  text(2.5, ylim[2]*.8, sub, cex=.9)
  xfit=seq(min(x),max(x),length=length(h$breaks)-1) 
  dfit=df(xfit)
  zero=xfit*0
  lines(xfit, dfit)
  lines(xfit, zero)
  
}

df=function(x) dnorm(x, mean=0, sd=nnmax_zadj_3x10_8_1$fixed$sd)
plotit(x=ranef(lmerfit5)$sim$"(Intercept)", df=df, sub="", main=expression(paste("Between (", phi[B], ") N=5K")))
df=function(x) dnorm(x, mean=0, sd=nnmax_zadj_3x10_8_1$fitted[[1]][["sdm"]])
plotit(x=residuals(lmerfit5), df=df, ylim=c(0, 1), sub="", main=expression(paste("Within (", phi[W], ")")))
df=function(x) dnnmax(x, mean=fixef(lmerfit5z)[["(Intercept)"]],
                      sd=nnmax_zadj_3x10_8_1$fixed$sd,
                      sdm=nnmax_zadj_3x10_8_1$fitted[[1]][["sdm"]],
                      k=nnmax_zadj_3x10_8_1$fitted[[1]][["k"]])
plotit(x=a$sim_10_8_1_5$max$z_constant_bias, df=df, sub="", sig=TRUE, main=expression(paste("Max (", psi, ")")))

df=function(x) dnorm(x, mean=0, sd=nnmax_zadj_3x10_8_1$fixed$sd)
plotit(x=ranef(lmerfit20)$sim$"(Intercept)", df=df, sub="", main=expression(paste("Between (", phi[B], ") N=20K")))
df=function(x) dnorm(x, mean=0, sd=nnmax_zadj_3x10_8_1$fitted[[1]][["sdm"]])
plotit(x=residuals(lmerfit20), df=df, ylim=c(0, 1), sub="", main=expression(paste("Within (", phi[W], ")")))
df=function(x) dnnmax(x, mean=fixef(lmerfit20z)[["(Intercept)"]],
                      sd=nnmax_zadj_3x10_8_1$fixed$sd,
                      sdm=nnmax_zadj_3x10_8_1$fitted[[1]][["sdm"]],
                      k=nnmax_zadj_3x10_8_1$fitted[[1]][["k"]])
plotit(x=a$sim_10_8_1_20$max$z_constant_bias, df=df, sub="", sig=TRUE, main=expression(paste("Max (", psi, ")")))

df=function(x) dnorm(x, mean=0, sd=nnmax_zadj_3x10_8_1$fixed$sd)
plotit(x=ranef(lmerfit100)$sim$"(Intercept)", df=df, sub="", main=expression(paste("Between (", phi[B], ") N=100K")))
df=function(x) dnorm(x, mean=0, sd=nnmax_zadj_3x10_8_1$fitted[[1]][["sdm"]])
plotit(x=residuals(lmerfit100), df=df, ylim=c(0, 1), sub="", main=expression(paste("Within (", phi[W], ")")))
df=function(x) dnnmax(x, mean=fixef(lmerfit100z)[["(Intercept)"]],
                      sd=nnmax_zadj_3x10_8_1$fixed$sd,
                      sdm=nnmax_zadj_3x10_8_1$fitted[[1]][["sdm"]],
                      k=nnmax_zadj_3x10_8_1$fitted[[1]][["k"]])
plotit(x=a$sim_10_8_1_100$max$z_constant_bias, df=df, sub="", sig=TRUE, main=expression(paste("Max (", psi, ")")))


par(op)
dev.off()