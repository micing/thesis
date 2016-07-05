library("lme4")
load("data/breast_cancer_sim/sim_10_8_3_20")
outfile="output/table4.csv"

d=sim_10_8_3_20
 
lmer_zadj = lmer(zadj ~ 1 + (1 | sim), data=d )
lmer_z = lmer(z ~ 1 + (1 | sim), data=d )
lmer_b = lmer(b ~ 1 + (1 | sim), data=d )

table4 = rbind(
  data.frame(
    model="b",
    mean=fixef(lmer_b)[["(Intercept)"]],
    sdb=as.data.frame(VarCorr(lmer_b))$sdcor[[1]],
    sdw=as.data.frame(VarCorr(lmer_b))$sdcor[[2]],
    icc=as.data.frame(VarCorr(lmer_b))$sdcor[[1]]^2/
      sum(as.data.frame(VarCorr(lmer_b))$sdcor^2)),
  data.frame(
    model="z",
    mean=fixef(lmer_z)[["(Intercept)"]],
    sdb=as.data.frame(VarCorr(lmer_z))$sdcor[[1]],
    sdw=as.data.frame(VarCorr(lmer_z))$sdcor[[2]],
    icc=as.data.frame(VarCorr(lmer_z))$sdcor[[1]]^2/
      sum(as.data.frame(VarCorr(lmer_z))$sdcor^2)),
  data.frame(
  model="zadj",
  mean=fixef(lmer_zadj)[["(Intercept)"]],
  sdb=as.data.frame(VarCorr(lmer_zadj))$sdcor[[1]],
  sdw=as.data.frame(VarCorr(lmer_zadj))$sdcor[[2]],
  icc=as.data.frame(VarCorr(lmer_zadj))$sdcor[[1]]^2/
    sum(as.data.frame(VarCorr(lmer_zadj))$sdcor^2))

  )

write.csv(table4, file=outfile)
  