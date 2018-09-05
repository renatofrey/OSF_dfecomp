### This script runs the regression models and is written to be run on a Linux cluster with a SLURM queueing system (it will be called by runjobs.R)
### (c) Renato Frey

t1 <- Sys.time()

myseed <- sample(1:1000000, 1)
set.seed(myseed)

args = (commandArgs(TRUE))
print(args)
if (length(args) == 0) {
  print("No arguments supplied.")
  ##supply default values
  task = NULL
  job = NULL
  d = NULL
  n = NULL
  mode = NULL
  decs = NULL
} else {
  for (i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}

job <- as.numeric(job)
d <- as.numeric(d)
n <- as.numeric(n)
decs <- as.numeric(decs)

#library(lme4)
library(BEST)
library(rstanarm)

#ncores <- parallel::detectCores()
options(mc.cores = 3)

# simulate data
source("sim_data.R")


set.seed(myseed)

predictors2 <- "1 + cond * mode + gamble_ind + (1+mode+gamble_ind | participant)"
n_predictors <- 9

for (i in 1:3) {
  
  if (i == 1) {
    DV_label <- "Sample size" 
    
    # Bayesian implementation of the mixed-effecst model
    mod_rstan <- stan_glmer(
      chains = 3,
      iter = 2000,
      formula = paste("samples ~ ", predictors2, sep=""),
      data = simdecs,
      family = poisson(link="identity"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5))
    # launch_shinystan(mod_rstan)
  }
  
  if (i == 2) {
    DV_label <- "Choice of H" 
    
    # Bayesian implementation of the mixed-effecst model
    mod_rstan <- stan_glmer(
      chains = 3,
      iter = 2000,
      formula = paste("H ~ ", predictors2, sep=""),
      data = stat_perdec,
      family = binomial(link="log"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5))
    # launch_shinystan(mod_rstan)
  }
  
  if (i == 3) {
    DV_label <- "Choice of Hexp" 
    
    # Bayesian implementation of the mixed-effecst model
    mod_rstan <- stan_glmer(
      chains = 3,
      iter = 2000,
      formula = paste("Hexp ~ ", predictors2, sep=""),
      data = stat_perdec,
      family = binomial(link="log"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5))
    # launch_shinystan(mod_rstan)
  }
  
  
  mcmc <- as.data.frame(mod_rstan)[,1:n_predictors]
  
  estimates <- NULL
  for (curr_cond in c("kind", "wick0.20", "wick0.15", "wick0.10")) {
    for (curr_mode in c("solo", "comp")) {
      
      curr_mcmc <- mcmc[,"(Intercept)"]
      
      if (curr_mode == "comp") curr_mcmc <- curr_mcmc + mcmc[,"modecomp"]
      if (curr_cond != "kind") curr_mcmc <- curr_mcmc + mcmc[,paste("cond", curr_cond, sep="")]
      
      interact <- paste("cond", curr_cond, ":", "mode", curr_mode, sep="")
      if (is.element(interact, colnames(mcmc))) curr_mcmc <- curr_mcmc + mcmc[,interact]
      
      estimates <- rbind(estimates, data.frame(cond=curr_cond, mode=curr_mode, mean=mean(curr_mcmc), rbind(hdi(curr_mcmc)[1:2])))
      
    }
  }
  
  
  t2 <- Sys.time()
    
  if (i == 2 | i == 3) estimates[,3:5] <- exp(estimates[,3:5])
  
  
  
  if (i == 1) save(mod_rstan, DV_label, mcmc, estimates, task, job, d, d_real, n, mode, decs, simdecs, t1, t2, myseed, file=paste("~/Data/dfe_comp/powersim_search/model_samplesize_", formatC(job, width=3, flag="0"), ".Rdata", sep=""))

  if (i == 2) save(mod_rstan, DV_label, mcmc, estimates, task, job, t1, t2, myseed, file=paste("~/Data/dfe_comp/powersim_search/model_H_", formatC(job, width=3, flag="0"), ".Rdata", sep=""))
  
  if (i == 3) save(mod_rstan, DV_label, mcmc, estimates, task, job, t1, t2, myseed, file=paste("~/Data/dfe_comp/powersim_search/model_Hexp_", formatC(job, width=3, flag="0"), ".Rdata", sep=""))
  
}
