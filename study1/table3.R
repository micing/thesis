load("data/breast_cancer_sim/aggregated_simulations")
outfile="output/table3.csv"
a=aggregated

p_hack_summary=function(d, n, label="") {
  s=d$max
  t=data.frame(
    total=NA,
    age=length(unique(d$mean_set$age)),
    exposure=length(unique(d$mean_set$night)),
    dataset=length(unique(d$mean_set$subset)),
    n=n,
    z=mean(d$max$z), 
    negative=mean(d$max$neg),
    positive=mean(d$max$pos),
    any=mean(d$max$sig) 
  ) 
  t$total=t$age*t$exposure*t$dataset
  return(t)
}


table3= rbind(p_hack_summary(a$sim_10_8_3_20, "20k"),
              p_hack_summary(a$sim_10_8_3_5, "5k"),
              p_hack_summary(a$sim_10_8_1_100, "100k"),
              p_hack_summary(a$sim_10_8_1_20, "20k"),
              p_hack_summary(a$sim_10_8_1_5,  "5k")
              )
              
write.csv(table3, file=outfile)