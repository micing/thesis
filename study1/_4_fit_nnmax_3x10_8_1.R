source("study1/functions.R")
library("lme4")
library("stats4")
load("data/breast_cancer_sim/aggregated_simulations")
load("data/breast_cancer_sim/sim_10_8_1_100")
load("data/breast_cancer_sim/sim_10_8_1_20")
load("data/breast_cancer_sim/sim_10_8_1_5")
a=aggregated

### prepare data ###

d100=sim_10_8_1_100
d20=sim_10_8_1_20
d5=sim_10_8_1_5

d20$sim=d20$sim+10000
d5$sim=d5$sim+20000

d=c(d100, d20, d5)

zadj100=aggregated$sim_10_8_1_100$max$zadj
zadj20=aggregated$sim_10_8_1_20$max$zadj
zadj5=aggregated$sim_10_8_1_5$max$zadj

zadj=c(zadj100, zadj20, zadj5)

z100=aggregated$sim_10_8_1_100$max$z
z20=aggregated$sim_10_8_1_20$max$z
z5=aggregated$sim_10_8_1_5$max$z

# note: this is a list for nnmax3
z=list(z100, z20, z5)

#### fit models ####

lmerfit = lmer(zadj ~ 1 + (1 | sim), data=d)
lmerfit5 = lmer(z ~ 1 + (1 | sim), data=d5)
lmerfit20 = lmer(z ~ 1 + (1 | sim), data=d20)
lmerfit100 = lmer(z ~ 1 + (1 | sim), data=d100)

nnmax_zadj_3x10_8_1 = nnmax.mle(zadj, mean=fixef(lmerfit)[["(Intercept)"]],
                            sd=as.data.frame(VarCorr(lmerfit))$sdcor[[1]],
                            sdm=as.data.frame(VarCorr(lmerfit))$sdcor[[2]]*1.33,
                            k=15)
save(nnmax_zadj_3x10_8_1, file="data/breast_cancer_sim/nnmax_zadj_3x10_8_1")

m=list(mean1=fixef(lmerfit100)[["(Intercept)"]],
       mean2=fixef(lmerfit20)[["(Intercept)"]],
       mean3=fixef(lmerfit5)[["(Intercept)"]])
s=list(sdm1=as.data.frame(VarCorr(lmerfit100))$sdcor[[2]]*1.33,
       sdm2=as.data.frame(VarCorr(lmerfit20))$sdcor[[2]]*1.33,
       sdm3=as.data.frame(VarCorr(lmerfit5))$sdcor[[2]]*1.33)

nnmax_z_3x10_8_1_constr = nnmax3constr.mle(z, mean=m,
                                   sd=as.data.frame(VarCorr(lmerfit))$sdcor[[1]],
                                   sdm=as.data.frame(VarCorr(lmerfit20))$sdcor[[2]]*1.33,
                                   k=15)
save(nnmax_z_3x10_8_1_constr, file="data/breast_cancer_sim/nnmax_z_3x10_8_1_constr")

nnmax_z_3x10_8_1 = nnmax3.mle(z, mean=m, sd=as.data.frame(VarCorr(lmerfit))$sdcor[[1]], sdm=s, k=15)
save(nnmax_z_3x10_8_1, file="data/breast_cancer_sim/nnmax_z_3x10_8_1")

