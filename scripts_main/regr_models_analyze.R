### This script summarizes the results from the regression models
### Renato Frey

library(HDInterval)

est <- list()
diff_ref <- list()

for (i in 1:4) {
  
  if (i == 1) load("../objects/main/model_samplesize.Rdata")
  if (i == 2) load("../objects/main/model_efficiency.Rdata")
  if (i == 3) load("../objects/main/model_Hexp.Rdata")
  if (i == 4) load("../objects/main/model_H.Rdata")
  
  if (i <= 2) r_prec <- 1 else r_prec <- 2
  
  print(DV_label)
  print(f)
  
  mcmc <- as.data.frame(mod_rstan)[,1:15]
  
  mcmcs <- NULL
  estimates <- NULL
  for (curr_cond in c("p00", "p40", "p20")) {
    for (curr_mode in c("solo", "comp")) {
      
      curr_mcmc <- 0
      
      if (curr_mode == "comp") curr_mcmc <- curr_mcmc + mcmc[,"modecomp"]
      if (curr_cond != "p00") curr_mcmc <- curr_mcmc + mcmc[,paste("cond", curr_cond, sep="")]
      
      interact <- paste("cond", curr_cond, ":", "mode", curr_mode, sep="")
      if (is.element(interact, colnames(mcmc))) curr_mcmc <- curr_mcmc + mcmc[,interact]
      
      curr_mcmc <- curr_mcmc + mcmc[,"(Intercept)"]
      
      mcmcs <- cbind(mcmcs, new=curr_mcmc)
      colnames(mcmcs)[ncol(mcmcs)] <- paste(curr_mode, curr_cond, sep="_")
      
      estimates <- rbind(estimates, data.frame(cond=curr_cond, mode=curr_mode, median=median(curr_mcmc), rbind(hdi(curr_mcmc)[1:2])))
      
    }
  }
  
  if (is.element(i, c(3,4))) {
    mcmcs <- exp(mcmcs)
    estimates[,3:5] <- exp(estimates[,3:5])
  }

  estimates[,3:5] <- round(estimates[,3:5], r_prec)
  
  # posterior differences to reference condition (solitary / kind)
  diffs <- mcmcs[,2:ncol(mcmcs)] - mcmcs[,1]
  diffs <- t(rbind(median=apply(diffs, 2, median), apply(diffs, 2, hdi)))
  diffs <- round(diffs[c(2,4,1,3,5),], r_prec)

  print(estimates)
  print(diffs)
    
  if (i == 1) {
    est[["samplesize"]] <- estimates
    diff_ref[["samplesize"]] <- diffs
  }
  if (i == 2) {
    est[["efficiency"]] <- estimates
    diff_ref[["efficiency"]] <- diffs
  }
  if (i == 3) {
    est[["Hexp"]] <- estimates
    diff_ref[["Hexp"]] <- diffs
  }
  if (i == 4) {
    est[["H"]] <- estimates
    diff_ref[["H"]] <- diffs
  }
  
}

save(est, diff_ref, file="../objects/main/estimates.Rdata")