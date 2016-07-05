data=read.csv("data/breast_cancer/breast_cancer_population.csv")
png(filename = "output/figure2.png",width = 550, height = 450)
op=par(no.readonly=TRUE) 

d=data
par(mar=c(4,4,2,4)+.2)
plot(d$age, d$n, type="n", ylab="Number of women in the population", xlab="Age")
polygon(c(d$age, rev(d$age)), c(d$n, rep(0,length(d$n))),col="steelblue3", density = NULL)
par(new=TRUE)

plot(d$age, d$incidence100k, xaxt="n", yaxt="n", type="n", ylab="", xlab="", ylim=c(0, 570))
axis(4)
mtext("Breast cancer incidence",side=4,line=3)

lines(d$age, d$incidence100k , lwd=3)
lines(d$age, d$incidence100k10y/10, lwd=3, lty=3)

legend("topright", 
       c("Age distribution",
         "Breast cancer", "incidence per", "100k women",
         "10 years incidence", "per 10k women"),
       bty="n", bg=NA, cex=.8, y.intersp=1.6,
       lty=c(1,1,NA, NA,3,NA),
       lwd=c(5, 3,NA, NA,3,NA),
       col=c("steelblue3", "black", "black", "black", "black",  "black"),
       inset=c(0,0)
       )

dev.off()
par(op)
