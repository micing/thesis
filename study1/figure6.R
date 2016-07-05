source("study1/functions.R")
load("data/breast_cancer_sim/aggregated_simulations")
a=aggregated

png(filename = "output/figure6.png",width = 550, height = 450)

op=par(no.readonly=TRUE) 
par(mar=c(4.5,4,1,.5))

hist(a$sim_10_8_3_20$mean_set$z, breaks=30, xlim=c(-.3,1.3), main="", xlab="Observed bias (z-score)")

par(op)
dev.off()