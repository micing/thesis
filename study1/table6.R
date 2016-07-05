source("equations.R")
source("study1/functions.R")
load("data/breast_cancer_sim/nnmax_2x10_8_3_approx")
load("data/breast_cancer_sim/nnmax_3x10_8_1_approx")

outfile="output/table6.csv"

makerow=function(label, est, N) {
  data.frame(
    label=label,
    N=N,
    n=est$fixed$pr*N,
    zeta=est$fitted@coef[["zeta"]],
    ICC=est$fixed$ICC,
    k=est$fixed$k,
    bias=median_nnmax(mean=est$fitted@coef[["zeta"]]*sqrt(est$fixed$pr*N),
                      sd=sqrt(est$fixed$ICC),sdm=sqrt(1-est$fixed$ICC),k=est$fixed$k),
    postype1=1-pnnmax(qnorm(.975), mean=est$fitted@coef[["zeta"]]*sqrt(est$fixed$pr*N),
                      sd=sqrt(est$fixed$ICC),sdm=sqrt(1-est$fixed$ICC),k=est$fixed$k)
  )
  
}


#### Table ###
table6=rbind(
  makerow(label="240, 20K", nnmax_2x10_8_3_approx, N=20000),
  makerow(label="240, 5K", nnmax_2x10_8_3_approx, N=5000),
  makerow(label="80, 100K", nnmax_3x10_8_1_approx, N=100000),
  makerow(label="80, 20K", nnmax_3x10_8_1_approx, N=20000),
  makerow(label="80, 5K", nnmax_3x10_8_1_approx, N=5000)

)

write.csv(table6, file=outfile)
