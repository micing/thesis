source("study1/functions.R")
library("lme4")
library("stats4")
load("data/breast_cancer_sim/aggregated_simulations")
load("data/breast_cancer_sim/sim_10_8_3_20")
load("data/breast_cancer_sim/sim_10_8_3_5")
a=aggregated

### prepare data ###

d20=sim_10_8_3_20
d5=sim_10_8_3_5

d20$sim=d20$sim+10000
d5$sim=d5$sim+20000

d=c(d20, d5)

zadj20=aggregated$sim_10_8_3_20$max$zadj
zadj5=aggregated$sim_10_8_3_5$max$zadj

zadj=c(zadj20, zadj5)

z20=aggregated$sim_10_8_3_20$max$z
z5=aggregated$sim_10_8_3_5$max$z

# note: this is a list for nnmax2
z=list(z20, z5)

#### fit models ####

lmerfit = lmer(zadj ~ 1 + (1 | sim), data=d)
lmerfit5 = lmer(z ~ 1 + (1 | sim), data=d5)
lmerfit20 = lmer(z ~ 1 + (1 | sim), data=d20)


nnmax_zadj_2x10_8_3 = nnmax.mle(zadj, mean=fixef(lmerfit)[["(Intercept)"]],
                                sd=as.data.frame(VarCorr(lmerfit))$sdcor[[1]],
                                sdm=as.data.frame(VarCorr(lmerfit))$sdcor[[2]]*1.33,
                                k=15)
save(nnmax_zadj_2x10_8_3, file="data/breast_cancer_sim/nnmax_zadj_2x10_8_3")

m=list(mean2=fixef(lmerfit20)[["(Intercept)"]],
       mean3=fixef(lmerfit5)[["(Intercept)"]])
s=list(sdm2=as.data.frame(VarCorr(lmerfit20))$sdcor[[2]]*1.33,
       sdm3=as.data.frame(VarCorr(lmerfit5))$sdcor[[2]]*1.33)

nnmax_z_2x10_8_3_constr = nnmax2constr.mle(z, mean=m,
                                           sd=as.data.frame(VarCorr(lmerfit))$sdcor[[1]],
                                           sdm=as.data.frame(VarCorr(lmerfit20))$sdcor[[2]]*1.33,
                                           k=15)
save(nnmax_z_2x10_8_3_constr, file="data/breast_cancer_sim/nnmax_z_2x10_8_3_constr")

nnmax_z_2x10_8_3 = nnmax2.mle(z, mean=m, sd=as.data.frame(VarCorr(lmerfit))$sdcor[[1]], sdm=s, k=15)
save(nnmax_z_2x10_8_3, file="data/breast_cancer_sim/nnmax_z_2x10_8_3")

