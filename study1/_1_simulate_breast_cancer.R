library("zoo")
  
N = 10^5 ## data sample size
iter= 2500 ## number of simulations
firstsave=2 ## first save to check data
saveInterval = 100 ## save sim data to disk
clearAfterSave = TRUE ## clears data in memory after save
maxAge = 70 ## define the population age range
minAge = 25
evernight = .3 ## probability of ever working night
workstart = 19 ## age at start of working life

include = list(age=c(1,2,3,4,5,6,7,8,9,10),
              night=c(1,2,3,4,5,6,7,8),
              subset=c(1,2,3)) ## models to include in the simulation run

# include = list(age=c(1,2,3,4,5,6,7,8,9,10),
#                night=c(1,2,3,4,5,6,7,8),
#                subset=c(1))


#### functions #######

agevars = function(d){
  
  d$age2 = d$age^2 ## square
  d$ageMedian = as.numeric(d$age > median(d$age))
  
  tertile2 = quantile(d$age, prob=.33, na.rm = TRUE)
  tertile3 = quantile(d$age, prob=.66, na.rm = TRUE)
  quartile2 = quantile(d$age, prob=.25, na.rm = TRUE)
  quartile4 = quantile(d$age, prob=.75, na.rm = TRUE)
  
  d$age2tertile = as.numeric(d$age>tertile2)
  d$age3tertile = as.numeric(d$age>tertile3)
  d$age2quartile = as.numeric(d$age>quartile2)
  d$age3quartile = as.numeric(d$ageMedian)
  d$age4quartile = as.numeric(d$age>quartile4)
  
  d$age20=as.numeric(d$age>20)
  d$age25=as.numeric(d$age>25)
  d$age30=as.numeric(d$age>30)
  d$age35=as.numeric(d$age>35)
  d$age40=as.numeric(d$age>40)
  d$age45=as.numeric(d$age>45)
  d$age50=as.numeric(d$age>50)
  d$age55=as.numeric(d$age>55)
  d$age60=as.numeric(d$age>60)
  d$age65=as.numeric(d$age>65)

  return(d)
}

nightvars = function(d) {
  
  d$nightMedian = as.numeric(d$nightyrs>median(d$nightyrs[d$nightyrs>0], na.rm=TRUE))
  tertile = quantile(d$nightyrs[d$nightyrs>0], prob=.66, na.rm = TRUE)
  quartile = quantile(d$nightyrs[d$nightyrs>0], prob=.75, na.rm = TRUE)
  d$night3tertile = as.numeric(d$nightyrs>tertile)
  d$night4quartile = as.numeric(d$nightyrs>quartile)
  
  d$night = as.numeric(d$nightyrs>0)
  d$night5 = as.numeric(d$nightyrs > 5)
  d$night10 = as.numeric(d$nightyrs > 10)
  d$night15 = as.numeric(d$nightyrs > 15)
  d$night20 = as.numeric(d$nightyrs > 20)
  d$night25 = as.numeric(d$nightyrs > 25)
  d$night30 = as.numeric(d$nightyrs > 30)
  
  return(d)
}

#### define models ####

agemodels = list( "age", "age + age2", 
                  
                  "ageMedian", "age2tertile + age3tertile" , "age2quartile + age3quartile + age4quartile",
                  
                  "age30 + age35 + age40 + age45 + age50 + age55 + age60 + age65",
                  "age35 + age40 + age45 + age50 + age55 + age60 + age65",
                  "age40 + age45 + age50 + age55 + age60 + age65",
                  "age45 + age50 + age55 + age60 + age65",
                  "age50 + age55 + age60 + age65" )

nightmodels = list("nightMedian" , 
                   "night3tertile" , 
                   "night4quartile" ,
                   "night10",
                   "night15",
                   "night20",
                   "night25",
                   "night30")     

#### read model data ####

cancer_sweden=read.csv("data/breast_cancer/breast_cancer_sweden.csv")
age_sweden=read.csv("data/breast_cancer/age_sweden.csv")
ageFemale = data.frame(age=age_sweden$age, n=age_sweden$female)
incAge=data.frame(age=approx(cancer_sweden$age, cancer_sweden$incidence, seq(1,100))$x,
                   incidence100k=approx(cancer_sweden$age, cancer_sweden$incidence, seq(1,100))$y)

incAge=subset(incAge, age>=10)
incAge$incidence100k10y = rollsum(incAge$incidence, k=10, align="left", fill=NA)
incAge$cancerProb = incAge$incidence100k/10^5
incAge$cancerProb10y = incAge$incidence100k10y/10^5

population=merge(incAge, ageFemale, by="age" )
population$sampleProb=population$n/sum(population$n)
pop_sub=subset(population, age>=minAge & age<=maxAge)
pop=data.frame(age=pop_sub$age, prob=pop_sub$cancerProb10y)
pop=agevars(pop)

#write.csv(population, "data/breast_cancer/breast_cancer_population.csv")

#### begin simulation ####

t1=proc.time()

simulation = data.frame()
notsaved = 0
runid=as.numeric(Sys.time())

for (i in seq(1, iter )) {

  print(paste0("Starting ieration: ", i, "  ", signif((proc.time()-t1)[[3]]/(i-1),3), " sec  ", Sys.time() ))
  notsaved = notsaved + 1
  
  d = merge(data.frame(age=sample(pop_sub$age, N, replace=T, prob=pop_sub$sampleProb)), pop)
  d$nightyrs = (runif(N)< evernight) * runif(N)*(d$age-workstart)
  d$cancer = runif(N) < d$prob
  d=nightvars(d)
  sd = list(d, subset(d, d$age<65), subset(d, d$age<60))

  for (a in seq(1, length(agemodels))) {
    for (n in seq(1, length(nightmodels))) {
      for (s in seq(1, length(sd))) {
        if (a %in% include[["age"]] & n %in% include[["night"]] & s %in% include[["subset"]]){
            m=glm(formula(paste0("cancer ~ ", nightmodels[n], " + ", agemodels[a])), family=binomial("logit"), data=sd[[s]])
            simulation = rbind(simulation, data.frame(
              sim=i,
              age=a,
              night=n,
              subset=s,
              b=summary(m)$coefficients[[2,1]],
              se=summary(m)$coefficients[[2,2]],
              p=summary(m)$coefficients[[2,4]],
              r=cor(sd[[s]]$nightyrs, sd[[s]]$age),
              cases=mean(sd[[s]]$cancer)*dim(sd[[s]])[[1]],
              n=dim(sd[[s]])[[1]],
              exposurevar=rownames(summary(m)$coefficients)[[2]],
              exposed = mean(sd[[s]][, rownames(summary(m)$coefficients)[[2]] ]),
              nightworkers = mean(sd[[s]]$night)
          
          ))
        }
      }
    }
  }
  if (notsaved>=saveInterval | i == iter | i==firstsave) {
    write.csv(simulation, paste0("sim_res_", i, "_", runid , ".csv"))
    if (i != firstsave) {notsaved = 0}
    if (clearAfterSave & i != firstsave & i != iter) {
      runid=as.numeric(Sys.time())
      simulation=data.frame()
    }
  }
}

t3=proc.time()

t3-t1
