#### Figure 1 ####
source("equations.R")

png(filename = "output/figure1.png", width = 550, height = 450)
op=par(no.readonly=TRUE) 

# data from Kivimäki et al (2012) on job strain and coronary heart disease
obs_n_a = 197473
obs_rr_a = 1.23
obs_upper_a =1.37
obs_lower_a =1.1
exposed_a = 30214/197473
disease_a = 2358/197473

# data from Åkerstedt et al (2013) on night work and breast cancer
obs_n_b = 13656
obs_rr_b = 1.77
obs_upper_b =3.04
obs_lower_b =1.03
exposed_b = 305/13656
disease_b = 463/13656

# create data to plot based on equation 5, 7 & 8
n=seq(5000, 5*10^5, 1000)
se_a=sqrt(eq7(n, exposed_a, disease_a)) ## eq 7
se_b=sqrt(eq7(n, exposed_b, disease_b))
or_a = exp(eq5(mu=0,sigma=se_a, alpha=.05)) ## eq 5
or_b = exp(eq5(mu=0,sigma=se_b, alpha=.05))
rr_a=eq8(or_a, disease_a) # eq 10
rr_b=eq8(or_b, disease_b)

# plot figure
plot(n, rr_b, type = "n", xlim=c(5*10^3,5*10^5), ylim=c(1, 3), 
     log="xy", xaxt="n", yaxt="n",
     ylab="Expected relative risk (RR)", xlab="Study sample size in thousands (k)")
axis(1, label=c("5k",  "10k", "20k", "40k", "100k", "200k", "400k"), 
     at=c(5*10^3,10^4,  2*10^4, 4*10^4, 10^5, 2*10^5, 4*10^5))
axis(2, at=c(1, 1.2, 1.5, 2, 3, 5 ))

lines(n, rr_a, lwd=3 )
lines(n, rr_b, lwd=3, col="firebrick3", lty=2)

points(obs_n_a, obs_rr_a, type="p", pch=19)
points(obs_n_b, obs_rr_b, col="firebrick3", type="p", pch=19)

segments(x0=obs_n_a, y0=obs_upper_a, x1=obs_n_a, y1=obs_lower_a, lwd=2)
segments(x0=obs_n_b, y0=obs_upper_b, x1=obs_n_b, y1=obs_lower_b, col="firebrick3", lwd=2)

legend("topright", 
       c("Scenario A:", "Kivimäki et al (2012)", "",
         "Scenario B:", "Åkerstedt et al (2013)"),
       col=c("black", "black", NA, "firebrick3", "firebrick3"),
       lty=c(1, NA, NA, 2, NA),
       pch=c(NA,19,NA,NA,19),
       lwd=3,
       cex=.8, bty="n")

par(op)
dev.off()


# calculate expeted RRs mentioned in text for 10k and 100k subjects
pred_a_n10k = eq8(exp(eq5(mu=0,sigma=sqrt(
  eq7(10^4, exposed_a, disease_a)), alpha=.05)), 
  p=disease_b)
pred_b_n10k = eq8(exp(eq5(mu=0,sigma=sqrt(
  eq7(10^4, exposed_b, disease_b)), alpha=.05)), 
  p=disease_b)

pred_a_n100k = eq8(exp(eq5(mu=0,sigma=sqrt(
  eq7(10^5, exposed_a, disease_a)), alpha=.05)), 
  p=disease_b)
pred_b_n100k = eq8(exp(eq5(mu=0,sigma=sqrt(
  eq7(10^5, exposed_b, disease_b)), alpha=.05)), 
  p=disease_b)

# print results
pred_a_n10k
pred_b_n10k
pred_a_n100k
pred_b_n100k