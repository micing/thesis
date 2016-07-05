source("equations.R")
source("study1/functions.R")
load("data/breast_cancer_sim/nnmax_zadj_2x10_8_3")
load("data/breast_cancer_sim/nnmax_z_2x10_8_3")
load("data/breast_cancer_sim/nnmax_zadj_3x10_8_1")
load("data/breast_cancer_sim/nnmax_z_3x10_8_1")

outfile="output/table5.csv"


makerow=function(label, est, n) {
  data.frame(
    label=label,
    mean=est$mean,
    sd=est$sd,
    sdm=est$sdm,
    k=est$k,
    bias=median_nnmax(mean=est$mean,sd=est$sd,sdm=est$sdm,k=est$k),
    postype1=1-pnnmax(qnorm(.975), mean=est$mean,sd=est$sd,sdm=est$sdm,k=est$k)
  )
  
}

no_bias=list()
### prepare results ###
nnmax_240_20=list(no_bias=list(), constant_bias=list(), residual_bias=list())
nnmax_240_5=list(no_bias=list(), constant_bias=list(), residual_bias=list())
nnmax_80_100=list(no_bias=list(), constant_bias=list(), residual_bias=list())
nnmax_80_20=list(no_bias=list(), constant_bias=list(), residual_bias=list())
nnmax_80_5=list(no_bias=list(), constant_bias=list(), residual_bias=list())

## 240_20 ####
n=nnmax_240_20$no_bias
n$mean=nnmax_zadj_2x10_8_3$fixed$mean
n$sd=nnmax_zadj_2x10_8_3$fixed$sd
n$sdm=nnmax_zadj_2x10_8_3$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_2x10_8_3$fitted$estimate[["k"]]
nnmax_240_20$no_bias=n

n=nnmax_240_20$constant_bias
n$mean=nnmax_z_2x10_8_3$fixed$mean$mean2
n$sd=nnmax_z_2x10_8_3$fixed$sd
n$sdm=nnmax_zadj_2x10_8_3$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_2x10_8_3$fitted$estimate[["k"]]
nnmax_240_20$constant_bias=n

n=nnmax_240_20$residual_bias
n$mean=nnmax_z_2x10_8_3$fixed$mean$mean2
n$sd=nnmax_z_2x10_8_3$fixed$sd
n$sdm= nnmax_z_2x10_8_3$fitted@coef[["sdm1"]]
n$k= nnmax_z_2x10_8_3$fitted@coef[["k"]]
nnmax_240_20$residual_bias=n

## 240_5 ####
n=nnmax_240_5$no_bias
n$mean=nnmax_zadj_2x10_8_3$fixed$mean
n$sd=nnmax_zadj_2x10_8_3$fixed$sd
n$sdm=nnmax_zadj_2x10_8_3$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_2x10_8_3$fitted$estimate[["k"]]
nnmax_240_5$no_bias=n

n=nnmax_240_5$constant_bias
n$mean=nnmax_z_2x10_8_3$fixed$mean$mean3
n$sd=nnmax_z_2x10_8_3$fixed$sd
n$sdm=nnmax_zadj_2x10_8_3$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_2x10_8_3$fitted$estimate[["k"]]
nnmax_240_5$constant_bias=n

n=nnmax_240_5$residual_bias
n$mean=nnmax_z_2x10_8_3$fixed$mean$mean3
n$sd=nnmax_z_2x10_8_3$fixed$sd
n$sdm= nnmax_z_2x10_8_3$fitted@coef[["sdm2"]]
n$k= nnmax_z_2x10_8_3$fitted@coef[["k"]]
nnmax_240_5$residual_bias=n

## 80_100 ####
n=nnmax_80_100$no_bias
n$mean=nnmax_zadj_3x10_8_1$fixed$mean
n$sd=nnmax_zadj_3x10_8_1$fixed$sd
n$sdm=nnmax_zadj_3x10_8_1$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_3x10_8_1$fitted$estimate[["k"]]
nnmax_80_100$no_bias=n

n=nnmax_80_100$constant_bias
n$mean=nnmax_z_3x10_8_1$fixed$mean$mean1
n$sd=nnmax_z_3x10_8_1$fixed$sd
n$sdm=nnmax_zadj_3x10_8_1$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_3x10_8_1$fitted$estimate[["k"]]
nnmax_80_100$constant_bias=n

n=nnmax_80_100$residual_bias
n$mean=nnmax_z_3x10_8_1$fixed$mean$mean1
n$sd=nnmax_z_3x10_8_1$fixed$sd
n$sdm= nnmax_z_3x10_8_1$fitted@coef[["sdm1"]]
n$k= nnmax_z_3x10_8_1$fitted@coef[["k"]]
nnmax_80_100$residual_bias=n

## 80_20 ####
n=nnmax_80_20$no_bias
n$mean=nnmax_zadj_3x10_8_1$fixed$mean
n$sd=nnmax_zadj_3x10_8_1$fixed$sd
n$sdm=nnmax_zadj_3x10_8_1$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_3x10_8_1$fitted$estimate[["k"]]
nnmax_80_20$no_bias=n

n=nnmax_80_20$constant_bias
n$mean=nnmax_z_3x10_8_1$fixed$mean$mean2
n$sd=nnmax_z_3x10_8_1$fixed$sd
n$sdm=nnmax_zadj_3x10_8_1$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_3x10_8_1$fitted$estimate[["k"]]
nnmax_80_20$constant_bias=n

n=nnmax_80_20$residual_bias
n$mean=nnmax_z_3x10_8_1$fixed$mean$mean2
n$sd=nnmax_z_3x10_8_1$fixed$sd
n$sdm= nnmax_z_3x10_8_1$fitted@coef[["sdm2"]]
n$k= nnmax_z_3x10_8_1$fitted@coef[["k"]]
nnmax_80_20$residual_bias=n

## 80_5 ####
n=nnmax_80_5$no_bias
n$mean=nnmax_zadj_3x10_8_1$fixed$mean
n$sd=nnmax_zadj_3x10_8_1$fixed$sd
n$sdm=nnmax_zadj_3x10_8_1$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_3x10_8_1$fitted$estimate[["k"]]
nnmax_80_5$no_bias=n

n=nnmax_80_5$constant_bias
n$mean=nnmax_z_3x10_8_1$fixed$mean$mean3
n$sd=nnmax_z_3x10_8_1$fixed$sd
n$sdm=nnmax_zadj_3x10_8_1$fitted$estimate[["sdm"]]
n$k=nnmax_zadj_3x10_8_1$fitted$estimate[["k"]]
nnmax_80_5$constant_bias=n

n=nnmax_80_5$residual_bias
n$mean=nnmax_z_3x10_8_1$fixed$mean$mean3
n$sd=nnmax_z_3x10_8_1$fixed$sd
n$sdm= nnmax_z_3x10_8_1$fitted@coef[["sdm3"]]
n$k= nnmax_z_3x10_8_1$fitted@coef[["k"]]
nnmax_80_5$residual_bias=n

#### Table ###
table5=rbind(
  
  makerow(label="nnmax_240_20$residual_bias", nnmax_240_20$residual_bias, n=20000),
  makerow(label="nnmax_240_5$residual_bias", nnmax_240_5$residual_bias, n=5000),
  makerow(label="nnmax_80_100$residual_bias", nnmax_80_100$residual_bias, n=100000),
  makerow(label="nnmax_80_20$residual_bias", nnmax_80_20$residual_bias, n=20000),
  makerow(label="nnmax_80_5$residual_bias", nnmax_80_5$residual_bias, n=5000),
  
  makerow(label="nnmax_240_20$constant_bias", nnmax_240_20$constant_bias, n=20000),
  makerow(label="nnmax_240_5$constant_bias", nnmax_240_5$constant_bias, n=5000),
  makerow(label="nnmax_80_100$constant_bias", nnmax_80_100$constant_bias, n=100000),
  makerow(label="nnmax_80_20$constant_bias", nnmax_80_20$constant_bias, n=20000),
  makerow(label="nnmax_80_5$constant_bias", nnmax_80_5$constant_bias, n=5000),
  
  makerow(label="nnmax_240_20$no_bias", nnmax_240_20$no_bias, n=20000),
  makerow(label="nnmax_240_5$no_bias", nnmax_240_5$no_bias, n=5000),
  makerow(label="nnmax_80_100$no_bias", nnmax_80_100$no_bias, n=100000),
  makerow(label="nnmax_80_20$no_bias", nnmax_80_20$no_bias, n=20000),
  makerow(label="nnmax_80_5$no_bias", nnmax_80_5$no_bias, n=5000)
      
)

write.csv(table5, file=outfile)
