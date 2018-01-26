### This script plots the histograms of the decision problems
### (c) Renato Frey

library(viridis)
cols <- c("cyan", "orange")


pdf(file="../output/main/env_dist.pdf", width=9, height=9)
par(mfcol=c(8,4), mar=c(2,3,2,1), mgp=c(0.5,.75,0))

for (i in 1:4) {
  if (i == 1) file_in <- "../data/main/dps_p00.csv"
  if (i == 2) file_in <- "../data/main/dps_p20.csv"
  if (i == 3) file_in <- "../data/main/dps_p15.csv"
  if (i == 4) file_in <- "../data/main/dps_p10.csv"
  
  data <- read.csv(file_in)

  for (dp in 1:8) {
    H <- data[,dp]
    L <- data[,dp+8]
    
    H_hist <- table(factor(H, levels=1:100))
    L_hist <- table(factor(L, levels=1:100))
    
    p_lwd <- 3
    if (i == 1) p_ylab = "Density" else p_ylab = ""
    plot(H_hist, type="h", col=cols[1], ylim=c(0,60), ylab=p_ylab, las=1, lwd=p_lwd, lend=1, xaxt="n", yaxt="n")
    points(L_hist, type="h", col=cols[2], lwd=p_lwd, lend=1)
    axis(1, seq(0,100,length.out=5))
    #axis(2, seq(0,60,length.out=4), las=1)
    
    if (dp == 1) title(c("Kind", "Mildly Wicked", "Moderately Wicked", "Extremely Wicked")[i])
    text(98, 70, paste("DP", dp, sep=""), cex=.9, xpd=T)
    
    
    ymax <- max(c(H_hist, L_hist)) + 3
    ymax <- 45
    ymax2 <- ymax-10
    
    
    if (dp == 1) legend("topright", c("EV(H)", "EV(L)"), col=c("cyan", "orange"), pch=18, pt.cex=1.5, box.lty=0)
    
    
    
    lines(c(mean(H), mean(L)), c(ymax, ymax), lty=1, xpd=T)
    points(mean(H), ymax, pch=18, col=cols[1], cex=1.5, xpd=T)
    points(mean(L), ymax, pch=18, col=cols[2], cex=1.5, xpd=T)
    text((mean(H)+mean(L))/2, ymax+10, round(mean(H)-mean(L), 1), xpd=T)
    
  }
}
dev.off()


