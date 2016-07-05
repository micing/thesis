load("data/breast_cancer_sim/aggregated_simulations")
outfile="output/table2.csv"

a=aggregated

m=a$sim_10_8_3_20$mean_age
s=a$sim_10_8_3_20$sd_age
age=data.frame(group="age", model=m$age,  z=m$z,  neg=m$neg, pos=m$pos, sig=m$sig)

m=a$sim_10_8_3_20$mean_night
s=a$sim_10_8_3_20$sd_night
night=data.frame(group="night", model=m$night,  z=m$z,  neg=m$neg, pos=m$pos, sig=m$sig)

m=a$sim_10_8_3_20$mean_subset
s=a$sim_10_8_3_20$sd_subset
sub=data.frame(group="subset", model=m$subset,  z=m$z, neg=m$neg, pos=m$pos, sig=m$sig)

t=rbind(age, night, sub)
table2=rbind(t, data.frame(
  group="mean", model="all", z=mean(t$z),  neg=mean(t$neg), pos=mean(t$pos), sig=mean(t$sig)) )

write.csv(table2, file=outfile)




