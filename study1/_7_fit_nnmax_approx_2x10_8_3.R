source("study1/functions.R")
load("data/breast_cancer_sim/nnmax_zadj_2x10_8_3")
load("data/breast_cancer_sim/nnmax_z_2x10_8_3")
load("data/breast_cancer_sim/aggregated_simulations")
a=aggregated
data=list(a$sim_10_8_3_5$max$z, a$sim_10_8_3_20$max$z)

d=read.csv("data/breast_cancer/breast_cancer_population.csv")
d=subset(d, d$age>25 & d$age<=70)
p=d$n/sum(d$n)
pr1=sum(p*d$incidence100k10y/100000)
d=subset(d, d$age>25 & d$age<=65)
p=d$n/sum(d$n)
pr2=sum(p*d$incidence100k10y/100000)
d=subset(d, d$age>25 & d$age<=60)
p=d$n/sum(d$n)
pr3=sum(p*d$incidence100k10y/100000)
pr=mean(c(pr1, pr2, pr3))


ICC=nnmax_zadj_2x10_8_3$fixed$sd^2/
  (nnmax_zadj_2x10_8_3$fixed$sd^2 + nnmax_zadj_2x10_8_3$fitted[[1]][["sdm"]]^2)

nnmax_2x10_8_3_approx = nnmax2approx.mle(data=data, ICC=ICC,
                                         k=nnmax_z_2x10_8_3$fitted@coef[["k"]],
                                         zeta=.6/sqrt(20000*pr), pr=pr)

save(nnmax_2x10_8_3_approx, file="data/breast_cancer_sim/nnmax_2x10_8_3_approx")