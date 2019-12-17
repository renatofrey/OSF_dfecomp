### This script runs the regression models
### Renato Frey

args <- (commandArgs(TRUE))
if (length(args) == 0) {
  sel_i <- 1:4
  print("No arguments supplied")
} else {
  print(args)
  for (k in 1:length(args)) {
    eval(parse(text=args[[k]]))
  }
}


set.seed(777)

library(lme4)
library(rstanarm)
options(mc.cores = parallel::detectCores())

load("../data/main/prepared.Rdata")

# relevel factors
stat_perdec$mode <- relevel(stat_perdec$mode, ref = "solo")
stat_perdec$cond <- relevel(stat_perdec$cond, ref = "p00")

# center trial on 0
stat_perdec$gamble_ind <- stat_perdec$gamble_ind - 1

for (i in sel_i) {
  
  # three-way interaction if DV = sample size
  if (i == 1) predictors <- "1 + cond * mode * gamble_ind + (1 + gamble_ind | participant)"
  if (i != 1) predictors <- "1 + cond * mode + gamble_ind + (1 + gamble_ind |  participant)"
  
  if (i == 1) {
    DV_label <- "Sample size" 
    f <- paste("samples ~ ", predictors, sep="")
    
    mod_rstan <- stan_glmer(
      chains = 3,
      iter = 2000,
      formula = f,
      data = stat_perdec,
      family = poisson(link="identity"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5),
      cores = 3)
    # launch_shinystan(mod_rstan)
  }
  
  if (i == 2) {
    DV_label <- "Efficiency" 
    f <- paste("samp_diff ~ ", predictors, sep="")
    
    mod_rstan <- stan_lmer(
      chains = 3,
      iter = 2000,
      formula = f,
      data = stat_perdec,
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5),
      cores = 3)
    # launch_shinystan(mod_rstan)
  }
  
  if (i == 3) {
    DV_label <- "Choice of Hexp" 
    f <-  paste("Hexp ~ ", predictors, sep="")
    
    mod_rstan <- stan_glmer(
      chains = 3,
      iter = 2000,
      formula = f,
      data = stat_perdec,
      family = binomial(link="log"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5),
      cores = 3)
    # launch_shinystan(mod_rstan)
  }
  
  if (i == 4) {
    DV_label <- "Choice of H" 
    f <- paste("H ~ ", predictors, sep="")
    
    mod_rstan <- stan_glmer(
      chains = 3,
      iter = 2000,
      formula = f,
      data = stat_perdec,
      family = binomial(link="log"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5),
      cores = 3)
    # launch_shinystan(mod_rstan)
  }
  
  if (i == 1) save(mod_rstan, DV_label, f, file="../objects/main/model_samplesize.Rdata")
  if (i == 2) save(mod_rstan, DV_label, f, file="../objects/main/model_efficiency.Rdata")
  if (i == 3) save(mod_rstan, DV_label, f, file="../objects/main/model_Hexp.Rdata")
  if (i == 4) save(mod_rstan, DV_label, f, file="../objects/main/model_H.Rdata")
  
}
