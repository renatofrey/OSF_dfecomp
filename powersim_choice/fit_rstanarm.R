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
  sim = NULL
  n = NULL
} else {
  for (i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}

job <- as.numeric(job)
sim <- as.numeric(sim)
n <- as.numeric(n)


load("../data/main/sim_choices.Rdata")

library(reshape2)
library(rstanarm)
library(BEST)
options(mc.cores = 3)

sens <- "sens=0.9"

ind <- 1:n + (sim - 1) * 30

kind_solo <- as.data.frame(hist_H[["p_rare=0"]][[sens]][[6/2]][ind,])
kind_solo$cond <- "p00"
kind_solo$mode <- "solo"
kind_comp <- as.data.frame(hist_H[["p_rare=0"]][[sens]][[2/2]][ind,])
kind_comp$cond <- "p00"
kind_comp$mode <- "comp"

wickmod_solo <- as.data.frame(hist_H[["p_rare=0.4"]][[sens]][[12/2]][ind,])
wickmod_solo$cond <- "p40"
wickmod_solo$mode <- "solo"
wickmod_comp <- as.data.frame(hist_H[["p_rare=0.4"]][[sens]][[2/2]][ind,])
wickmod_comp$cond <- "p40"
wickmod_comp$mode <- "comp"

wickext_solo <- as.data.frame(hist_H[["p_rare=0.2"]][[sens]][[18/2]][ind,])
wickext_solo$cond <- "p20"
wickext_solo$mode <- "solo"
wickext_comp <- as.data.frame(hist_H[["p_rare=0.2"]][[sens]][[2/2]][ind,])
wickext_comp$cond <- "p20"
wickext_comp$mode <- "comp"

simdata_wide <- rbind(kind_solo, kind_comp, wickmod_solo, wickmod_comp, wickext_solo, wickext_comp)
simdata_wide$participant <- paste("sim", formatC(as.numeric(row.names(simdata_wide)), flag="0", width=3), sep="_")

simdata <- melt(simdata_wide, id.vars=c("participant", "cond", "mode"))
colnames(simdata) <- gsub("variable", "gamble_ind", colnames(simdata))
colnames(simdata) <- gsub("value", "H", colnames(simdata))
levels(simdata$gamble_ind) <- 1:8
simdata$gamble_ind <- as.numeric(simdata$gamble_ind)
simdata <- simdata[order(simdata$participant,simdata$gamble_ind),]
rownames(simdata) <- 1:nrow(simdata)

props <- tapply(simdata$H, list(simdata$cond, simdata$mode), mean, na.rm=T)
props <- props[c("p00", "p40", "p20"), c("solo", "comp")]
print(props)

simdata$H <- as.factor(simdata$H)
simdata$participant <- as.factor(simdata$participant)
simdata$cond <- as.factor(simdata$cond)
simdata$mode <- as.factor(simdata$mode)
simdata$mode <- relevel(simdata$mode, ref="solo")

simdata <- subset(simdata, !is.na(simdata$H))


set.seed(myseed)

predictors <- "1 + cond * mode + (1 | participant)"
n_pred <- 9

# center trial on 0
simdata$gamble_ind <- simdata$gamble_ind - 1

DV_label <- "Choice of H" 

t1 <- Sys.time()
mod_rstan <- stan_glmer(
  chains = 3,
  iter = 2000,
  formula = paste("H ~ ", predictors, sep=""),
  data = simdata,
  family = binomial(link="log"),
  prior_intercept = normal(location = 0, scale = 10),
  prior = normal(location = 0, scale = 2.5))
t2 <- Sys.time()

mcmc <- as.data.frame(mod_rstan)[,1:n_pred]

estimates <- NULL
for (curr_cond in c("kind", "p40", "p20")) {
  for (curr_mode in c("solo", "comp")) {
    
    curr_mcmc <- mcmc[,"(Intercept)"]
    
    if (curr_mode == "comp") curr_mcmc <- curr_mcmc + mcmc[,"modecomp"]
    if (curr_cond != "kind") curr_mcmc <- curr_mcmc + mcmc[,paste("cond", curr_cond, sep="")]
    
    interact <- paste("cond", curr_cond, ":", "mode", curr_mode, sep="")
    if (is.element(interact, colnames(mcmc))) curr_mcmc <- curr_mcmc + mcmc[,interact]
    
    estimates <- rbind(estimates, data.frame(cond=curr_cond, mode=curr_mode, mean=mean(curr_mcmc), rbind(hdi(curr_mcmc)[1:2])))
    
  }
}

estimates[,3:5] <- exp(estimates[,3:5])

save(mod_rstan, props, DV_label, simdata, mcmc, estimates, task, job, n, t1, t2, myseed, file=paste("~/Data/dfe_comp/powersim_choice/model_H_", formatC(job, width=3, flag="0"), ".Rdata", sep=""))