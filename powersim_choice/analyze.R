### This script summarizes the results of the prospective design analysis for choice
### (c) Renato Frey

library(BEST)

ROPE_width <- 0.2
sel_effects <- c("condp40", "condp20", "modecomp", "condp40:modecomp", "condp20:modecomp")

mycol_rgb <- cbind(t(col2rgb("red")), alpha=150)
mycol <- rgb(mycol_rgb[,1], mycol_rgb[,2], mycol_rgb[,3], mycol_rgb[,4], maxColorValue=255)
cols_cred <- data.frame("credible"="seagreen", "not credible"=adjustcolor("seagreen", alpha.f=.5), "inconclusive"="khaki")

files <- list.files("../objects/scicore/powersim_choice/", full.names=T)
durations <- NULL
results <- list()
stat_all <- NULL
for (effect in sel_effects) {
  
  print(effect)
  pdf(file=paste("../output/main/powersim_choice_", gsub("\\.", "_", effect), ".pdf", sep=""), height=4, width=9)
  par(mfrow=c(2,3), mar=c(5,5,2,2), mgp=c(2.7,.8,0))
  results_tmp <- NULL

  for (file in files) {

    load(file)
    
    r_prec <- 2
    coefs <- round(summary(mod_rstan)[,c("mean", "2.5%", "97.5%")], r_prec)
    coefs <- round(rbind("(Intercept)"=exp(coefs[1,]), exp(coefs[1,] + coefs[2:nrow(coefs),]) - exp(coefs[1,])), r_prec)
    
    curr_coef <- coefs[effect,]
    
    durations <- c(durations, t2-t1)
    #print(warn)
    
    posterior <- exp(mcmc[,effect]) - exp(mcmc[,"(Intercept)"])
    #posterior <-  mcmc[,effect]
    HDI <- as.numeric(hdi(posterior))

    
    HDI <- as.numeric(curr_coef[2:3])
    
    
    
    ROPE <- 0 + c(ROPE_width * -1, ROPE_width)
    
    POST_inROPE <- posterior > ROPE[1] & posterior < ROPE[2]
    POST_inROPE <- as.numeric(round(prop.table(table(POST_inROPE))[2], 2))
    if (is.na(POST_inROPE)) POST_inROPE <- 0
    
    if (HDI[1] > ROPE[1] & HDI[2] < ROPE[2]) inference <- "not credible" else if (HDI[1] < ROPE[1] & HDI[2] < ROPE[1]) inference <- "credible" else if (HDI[1] > ROPE[2] & HDI[2] > ROPE[2]) inference <- "credible" else inference <- "inconclusive"
    
    if (is.element(file, files[1:prod(par()$mfcol)])) {
      #Sys.sleep(1)
      
      y.max <- 800
      
      h <- hist(posterior, breaks=seq(-2, 2, by=.01), xlim=c(-1, 1), ylim=c(0, y.max), col=mycol, border=0, las=1, xlab="Proportion of H-choices", ylab="Posterior samples (freq.)", main=paste("Simulation:", job))
      
      rect(ROPE[1], 0, ROPE[2], y.max, density=20, col="lightgrey", border=0)
      text(0, y.max-50, "ROPE", col="darkgrey")
      
      
      #lines(x=ROPE, y=rep(-5, 2), lwd=5)
      #abline(v=ROPE[1], col="lightgrey")
      #abline(v=ROPE[2], col="lightgrey")

      text(-6, y.max-50, inference, col=as.character(cols_cred[,gsub(" ", ".", inference)]), xpd=T, cex=1)
      
      lines(x=HDI, y=rep(0, 2), lwd=4, col="darkred", lend=1)
    }
    
    
    results_tmp <- rbind(results_tmp, data.frame(job,
                                                 n,
                                                 ROPEl=ROPE[1],
                                                 ROPEu=ROPE[2],
                                                 median=median(posterior),
                                                 HDIl=HDI[1],
                                                 HDIu=HDI[2],
                                                 POST_inROPE,
                                                 inference))

  }
  
  dev.off()  
  results[[effect]] <- results_tmp
  
  results_tmp$inference <- factor(results_tmp$inference, levels=c("credible","not credible","inconclusive"))
  stat <- tapply(results_tmp$inference, list(results_tmp$inference, results_tmp$n), length)
  stat[is.na(stat)] <- 0
  stat <- stat / matrix(rep(colSums(stat), nrow(stat)), nrow=nrow(stat), byrow=T)
  stat_all <- cbind(stat_all, stat)
  
  
  print(props)
  print(stat)
  
  
}



pdf("../output/main/powersim_choice.pdf", width=8)

p_stat <- stat_all
#p_stat <- p_stat[,colnames(p_stat) != "100"]
p_stat <- p_stat[,ncol(p_stat):1]

#colnames(p_stat) <- NULL

par(mgp=c(0,.5,0), mar=c(5,3,3,1))
b <- barplot(p_stat, beside=F, col=as.character(unlist(cols_cred)), las=1, xlab="", ylab="", border=F, horiz=T, space=c(.5,.075,.075))

text(.5, -2, "Proportion of simulation outcomes (power)", xpd=T)
text(-.075, mean(b), "Predictors / Sample size", srt=90, xpd=T)

title("Dependent variable: Choice of higher-EV option", adj=0.5, line=1.5, xpd=T, cex.main=1.5)

legend(0, 19.6, xpd=T, pch=15, cex=.8, pt.cex=1.5, col=as.character(unlist(cols_cred)), c("Conclusive evidence (credible effect)", "Conclusive evidence (absence of effect)", "Inconclusive evidence"), box.lty=0, horiz=T)

#text(x=rep(0, length(b)/3), y=b[seq(length(b)-1, 1, by=-3)], xpd=T, sel_effects, pos=4, col="gray95")

lab <- rev(sel_effects)

lab <- gsub("condp40:modecomp", "Environment: Moderately wicked x Search mode: Competitive", lab)
lab <- gsub("condp20:modecomp", "Environment: Extremely wicked x Search mode: Competitive", lab)
lab <- gsub("condp40", "Environment: Moderately wicked", lab)
lab <- gsub("condp20", "Environment: Extremely wicked", lab)
lab <- gsub("modecomp", "Search mode: Competitive", lab)

text(x=rep(0, length(b)/3), y=b[seq(2, length(b), by=3)], xpd=T, lab, pos=4, col="gray95")

dev.off()

print(round(p_stat, 2))

if (file.exists("../output/main/powersim_search.pdf")) {
  system("pdfjam ../output/main/powersim_search.pdf ../output/main/powersim_choice.pdf --nup 2x1 --outfile ../output/main/powersim.pdf")
  system("pdfcrop ../output/main/powersim.pdf ../output/main/powersim.pdf")
}