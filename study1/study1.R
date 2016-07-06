source("equations.R")
source("study1/functions.R")
#library("lmtest")

#source("study1/figure2.R")

# absolute and relative risks of 30, 50 & 60 year olds
data=read.csv("data/breast_cancer/breast_cancer_population.csv")
d=data
r50=d[d$age==50, ]$incidence100k10y/1000
r60=d[d$age==60, ]$incidence100k10y/1000
r30=d[d$age==30, ]$incidence100k10y/1000

rr5030=r50/r30
rr6030=r60/r30

## print estimates mentioned in text
r50
r60
r30
rr5030
rr6030

#### results ####

## table 2
#source("study1/table2.R")
load("data/breast_cancer_sim/aggregated_simulations")
a=aggregated

## type-1 eror rate for simulation with 192 models
mean(a$sim_8_8_3_20$max$sig)

## figure 
#source("study1/figure3.R")
d=read.csv("data/breast_cancer/breast_cancer_population.csv")
d=subset(d, d$age>25 & d$age<=70)
p=d$n/sum(d$n)
prb=sum(p*d$incidence100k10y/100000)

x=a$sim_10_8_3_20$max$b
x1=a$sim_10_8_3_20$max$b[a$sim_10_8_3_20$max$pos==1]

sigRR=signif(eq8(or=exp(x1), p=prb), 3)
meanRR=eq8(or=exp(mean(x)), p=prb)

## mean RR and min/max RR for observed significant risks
meanRR
min(sigRR)
max(sigRR)

## table 3
#source("study1/table3.R")

## table 4
#source("study1/table4.R")

## figure 4
#source("study1/figure4.R")

load("data/breast_cancer_sim/nnmax_zadj_3x10_8_1")

nnmax_zadj_3x10_8_1

## figure 5
#source("study1/figure5.R")

## figure 6
#source("study1/figure6.R")

## figure 7
#source("study1/figure7.R")

## table 5
#source("study1/table5.R")
median_nnmax(mean=0.425, sd=0.785, sdm=1.141, k=21.1)

##table 6
#source("study1/table6.R")

## figure 8
source("study1/figure8.R")

# range of bias plotted for minimla bias
bias$min

# range of RRs plotted for minimal bias
rr$min

