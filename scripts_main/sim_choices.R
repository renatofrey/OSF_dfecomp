### This script runs the simulation analysis for the propotions of H-choices
### (c) Renato Frey

pdf(file="../output/main/simulation.pdf", height=5, width=10)
par(mfrow=c(1,3))

envs <- c(0, .4, .2)

#sensitivities <- c(1,.85,.7)
sensitivities <- c(1,.9,.8)

n_samples <- seq(1, 10, by=1)

n_exp <- 1000
n_part <- 30
n_sims <- n_exp * n_part

# prepare empty lists
l1 <- list()
l1[paste("ss=", n_samples, sep="")] <- NA
l2 <- list()
l2[paste("sens=", sensitivities, sep="")] <- list(l1)
hist_H <- list()
hist_H[paste("p_rare=", envs, sep="")] <- list(l2)

p_ind <- 1

for (p_rare in envs) {
  if (p_rare == 0) df <- read.csv(file="../data/main/dps_p00.csv", check.names=F)
  if (p_rare == 0.4) df <- read.csv(file="../data/main/dps_p40.csv", check.names=F)
  if (p_rare == 0.2) df <- read.csv(file="../data/main/dps_p20.csv", check.names=F)

  
  set.seed(1234)
  
  n_gambles <- ncol(df) / 2
  
  if (!exists("p_ind")) p_ind <- ""
  
  #cols <- heat.colors(length(sensitivities))
  #cols <- c("red", adjustcolor("red", alpha.f=.75), adjustcolor("red", alpha.f=.5))
  cols <- viridis::viridis(10)[c(8,6,4)]
  
  chances <- list()

  for (s in 1:length(sensitivities)) {
    
    sens <- sensitivities[s]
    
    HLdiff <- data.frame()
    sim_choice <- matrix(NA, ncol=n_gambles, nrow=n_sims)
    sim_HLdiff <- matrix(NA, ncol=n_gambles, nrow=n_sims)
    
    for (n in n_samples) {
      print(paste("p_rare=", p_rare, " sens=", sens, " n=",n, sep=""))
      for (i in 1:n_gambles) {
        for (sim in 1:n_sims) {
          outA <- df[sample(1:1000, n, replace=T), paste(i, "H", sep="")]
          outB <- df[sample(1:1000, n, replace=T), paste(i, "L", sep="")]
          meanA <- mean(outA)
          meanB <- mean(outB)
          HL_diff <- round(meanA - meanB, 2)
          choice <- NA
          
          if (meanA > meanB) choice <- sample(c(1,0), 1, prob=c(sens,1-sens))
          if (meanA < meanB) choice <- sample(c(0,1), 1, prob=c(sens,1-sens))
          sim_choice[sim, i] <- choice
          sim_HLdiff[sim, i] <- HL_diff
        }
      }
      
      hist_H[[paste("p_rare=",p_rare,sep="")]][[paste("sens=",sens,sep="")]][[paste("ss=",n,sep="")]] <- sim_choice
      
      ind_exp <- rep(1:n_exp, each=n_part)
      curr_chances <- aggregate(sim_choice, by=list(ind_exp), mean, na.rm=T)[,2:(n_gambles+1)]
      curr_chances <- round(curr_chances, 3)
      row.names(curr_chances) <- paste("exp", row.names(curr_chances), sep="")
      
      chances[[paste("sens", sens, sep="_")]][[paste("ss", n, sep="_")]] <- curr_chances
    }
    
    if (T) {
      
      if (s == 1) {
        
        if (p_rare == 0.0) p_title <- "Kind environment:\np(rare) = 0"
        if (p_rare == 0.4) p_title <- "Moderately wicked environment:\np(rare) = 0.4"
        if (p_rare == 0.2) p_title <- "Extremely wicked environment:\np(rare) = 0.2"
        
        plot(1:length(n_samples), type="n", col="blue", xlab="Number of samples", ylab="Probability of choosing H", xaxt="n", ylim=c(0,1), las=1, main=p_title)
        axis(1, at=1:n, n_samples*2, cex.axis=1)
        abline(h=.5, lty=3)
      }
      
      curr_sens <- chances[[paste("sens", sens, sep="_")]]
      gambles_avg <- lapply(curr_sens, function(x) {apply(x, 1, mean)}) # average chance across all 8 gambles
      
      p_means <- as.numeric(lapply(gambles_avg, mean))
      
      #p_cil <- as.numeric(lapply(gambles_avg, min))
      #p_ciu <- as.numeric(lapply(gambles_avg, max))
      
      p_sds <- as.numeric(lapply(gambles_avg, sd))
      p_cil <- p_means - p_sds
      p_ciu <- p_means + p_sds
      
      points(p_means, type="b", col=cols[s], lwd=3)
      points(p_ciu, type="l", col=cols[s], lty=3, pch=18, cex=.5)
      points(p_cil, type="l", col=cols[s], lty=3, pch=18, cex=.5)
      
    }
  }
  
  legend("bottomright", 1, sensitivities, lty=1, col=cols, box.lty=0, title="Sensitivity", lwd=3)

  p_ind <- p_ind + 1
}

save(hist_H, file="../data/main/sim_choices.Rdata")

dev.off()
