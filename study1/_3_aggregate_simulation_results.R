## Read simulation data
sim_10_8_3_5=read.csv("data/breast_cancer_sim/sim_all_10-8-3_n5k_i10k.csv", stringsAsFactors = FALSE)
sim_10_8_3_20=read.csv("data/breast_cancer_sim/sim_all_10-8-3_n20k_i10k.csv", stringsAsFactors = FALSE)
sim_10_8_1_100=read.csv("data/breast_cancer_sim/sim_all_10-8-1_n100k_i10k.csv", stringsAsFactors = FALSE)

## check to see that the exposure var was not dropped and replaced with age in any model
# unique(sim10_8_3_5$exposurevar) 
# unique(sim10_8_3_20$exposurevar) # this simulation did not save exposurevar
# unique(sim10_8_1_100$exposurevar) 

simulations=list(
      sim_10_8_3_5 = sim_10_8_3_5,
      sim_10_8_3_20 = sim_10_8_3_20, 
    
      sim_8_8_3_5 = subset(sim_10_8_3_5, age %in% c(1,2,4,5,6,7,8,9)),
      sim_8_8_3_20 = subset(sim_10_8_3_20, age %in% c(1,2,4,5,6,7,8,9)),
          
      sim_10_8_1_5 = subset(sim_10_8_3_5, subset %in% c(1)),
      sim_10_8_1_20 = subset(sim_10_8_3_20, subset %in% c(1)),
      sim_10_8_1_100 = sim_10_8_1_100
)


aggregated=list()
for (i in 1:length(simulations)) {
  s=simulations[[i]]
  s$z=s$b/s$se
  s$set=s$age*100+s$night*10+s$subset
  s$sig=s$p<.05
  s$pos=as.numeric(s$p<.05 & s$b>0)
  s$neg=as.numeric(s$p<.05 & s$b<0)
  
  a=mean_set=aggregate(s, by=list(s$set), mean)
  setbias = data.frame(set=a$set, zbias=a$z, bbias=a$b)
  s=merge(s, setbias, by=c("set"))
  s$zadj=s$z-s$zbias
  s$badj=s$b-s$bbias
  
  s=subset(s, select=c(sim, age, night, subset, set, b, bbias, badj, se, z, zbias, zadj, p, neg, pos, sig))
  simulations[[i]]=s

  
  aggregated[[names(simulations)[[i]]]]= list(
    
    max=aggregate(s, by=list(s$sim), max),
    min=aggregate(s, by=list(s$sim), min),
    mean=aggregate(s, by=list(s$sim), mean),
    sum=aggregate(s, by=list(s$sim), sum),
    
    mean_set=aggregate(s, by=list(s$set), mean),
    mean_age=aggregate(s, by=list(s$age), mean),
    mean_night=aggregate(s, by=list(s$night), mean),
    mean_subset=aggregate(s, by=list(s$subset), mean),
    
    sd_set=aggregate(s, by=list(s$set), sd),
    sd_age=aggregate(s, by=list(s$age), sd),
    sd_night=aggregate(s, by=list(s$night), sd),
    sd_subset=aggregate(s, by=list(s$subset), sd)
    
  )
}

save(simulations, file="data/breast_cancer_sim/simulations")
save(aggregated, file="data/breast_cancer_sim/aggregated_simulations")

sim_10_8_1_100=simulations$sim_10_8_1_100
save(sim_10_8_1_100, file="data/breast_cancer_sim/sim_10_8_1_100")



