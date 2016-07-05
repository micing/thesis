sim_dir = "sim_breast_cancer_10-8-1_n100k_i10k"
sim_res_dir=paste0("./data/", sim_dir,"/")
sim_out_dir = "./data/breast_cancer_sim/"
files=list.files(sim_res_dir, pattern="sim_res*")
outfile=paste0("sim_all", substr(sim_dir, 18, 100), ".csv")

sim=data.frame()
for (f in files) {
  print(paste0("Reading file: ", f))
  s=read.csv(paste0(sim_res_dir,f))
  s$sim=s$sim-min(s$sim)+1+length(unique(sim$sim))
  sim=rbind(sim, s)
}
write.csv(sim, paste0("data/breast_cancer_sim/", outfile))

s=subset(sim, select=c(age, night, subset, b, se, p, r, cases, n))
table(duplicated(s))
