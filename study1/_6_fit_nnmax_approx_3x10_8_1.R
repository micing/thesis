source("equations.R")
source("study1/functions.R")
load("data/breast_cancer_sim/nnmax_zadj_3x10_8_1")
load("data/breast_cancer_sim/nnmax_z_3x10_8_1")
load("data/breast_cancer_sim/aggregated_simulations")
a=aggregated
data=list(a$sim_10_8_1_5$max$z, a$sim_10_8_1_20$max$z, a$sim_10_8_1_100$max$z)

d=read.csv("data/breast_cancer/breast_cancer_population.csv")
d=subset(d, d$age>25 & d$age<=70)
p=d$n/sum(d$n)
pr=sum(p*d$incidence100k10y/100000)


ICC=nnmax_zadj_3x10_8_1$fixed$sd^2/
  (nnmax_zadj_3x10_8_1$fixed$sd^2 + nnmax_zadj_3x10_8_1$fitted[[1]][["sdm"]]^2)

nnmax_3x10_8_1_approx = nnmax3approx.mle(data=data, ICC=ICC, 
                                         k=nnmax_z_3x10_8_1$fitted@coef[["k"]],
                                         zeta=.6/sqrt(20000*pr), pr=pr)

save(nnmax_3x10_8_1_approx, file="data/breast_cancer_sim/nnmax_3x10_8_1_approx")