source("study1/functions.R")
source("equations.R")
load("data/breast_cancer_sim/aggregated_simulations")
png(filename = "output/figure3.png", width = 550, height = 450)

op=par(no.readonly=TRUE) 
par(mfrow=c(1,2), mar=c(5,4,2,.5))

a=aggregated

x=a$sim_10_8_3_20$max$b
x1=a$sim_10_8_3_20$max$b[a$sim_10_8_3_20$max$pos==1]
x0=a$sim_10_8_3_20$max$b[a$sim_10_8_3_20$max$pos==0]

h=hist(x, breaks=100, plot=FALSE)
h0=hist(x0, breaks=h$breaks, plot=FALSE)
h1=hist(x1, breaks=h$breaks, plot=FALSE)

d=read.csv("data/breast_cancer/breast_cancer_population.csv")
d=subset(d, d$age>25 & d$age<=70)
p=d$n/sum(d$n)
prb=sum(p*d$incidence100k10y/100000)

RRbreaks= signif(eq8(or=exp(h$breaks), p=prb), 2)
sigRR=signif(eq8(or=exp(x1), p=prb), 2)
meanRR=eq8(or=exp(mean(x)), p=prb)

b=barplot(rbind(h0$counts, h1$counts),
          col=c("steelblue3", "firebrick3"), names.arg=RRbreaks[-1],
          density=c(50, 100), space=0,las=1,main="",
          ylab="Frequency", xlab="Observed Relative Risks (RR)")

legend("topright", 
       c("Observed risk is", "significant (p<.05)" , "",  "Observed risk is",  "non-significant"), 
       col=c("firebrick3", NA, NA, "steelblue3", NA) , 
       cex=.7, bty="n" , xjust=0, inset=c(.00,0), y.intersp=1.15,
       lwd=c(5, NA, NA, 5, NA))


sig=a$sim_10_8_3_20$sum$sig
s=sig[sig>0 & sig<100 ]

hist(s,breaks=c(seq(0, 100)), xaxt="n", main="",
     xlab="Number of significant (p<.05)\n estimates observed per study")
axis(1, at=c(1,20,40,60,80,100))

text(60, 400, cex=.7, paste0(round(length(sig[sig>0])/100),
                             "% of studies observed \nat least one significant estimate", "\n\n",
                             round(length(sig[sig>=5])/100),
                             "% of studies observed \nat least five significant estimates", "\n\n",
                             round(length(sig[sig>100])/100),
                             "% of studies observed \nmore than 100 significant estimates"))

dev.off()
par(op)
