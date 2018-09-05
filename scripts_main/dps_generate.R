### This script generates the decision problems (DPs) of the different choice environments, and produces figures and tables depicting the resulting distributions
### (c) Renato Frey

library(viridis)

set.seed(123)

envs <- c(.0, .4, .2)
N <- 8

c <- 300

scale <- 1

a1_means <- c(10, 35, 60, 85)
a1_means <- c(a1_means, c-rev(a1_means))

b_diff <- 2
sd <- 1

cols <- plasma(2, begin=0.1, end=.7)
cH <- cols[2]
cL <- cols[1]

# plot DPs of all choice environments
pdf(width=10, height=11, file="../output/main/env_dist.pdf")
par(mgp=c(1.5,0.5,0))

d <- rbind(c(1,2,3))
for (i in 1:8) {
  d <- rbind(d, c(i+3, i+3+8, i+3+16))
  d <- rbind(d, c(i+3, i+3+8, i+3+16))
}
layout(d)

par(mar=c(0,2,4.5,1))
frame()
title("Kind environment:\np(rare) = 0")

frame()
title("Moderately wicked environment:\n p(rare) = 0.4")

frame()
title("Extremely wicked environment:\np(rare) = 0.2")

stats <- data.frame()
for (p_rare in envs) {

  if (p_rare == .0) a2_diff <- 0
  if (p_rare == .4) a2_diff <- 105
  if (p_rare == .2) a2_diff <- 210

  dps <- NULL  
  for (dp in 1:N) {
    
    
    par(mar=c(3,2,1,1))
    
    if (dp <= 4) sign <- 1 else sign <- -1

    a1 <- round(rnorm(n=1000*(1-p_rare), mean=a1_means[dp], sd=sd))
    
    if (p_rare != 0) a2 <- round(rnorm(n=1000*p_rare, mean=a1_means[dp]+sign*a2_diff, sd=sd)) else a2 <- NULL
    a <- c(a1,a2)
    
    b <- round(rnorm(n=1000, mean=a1_means[dp] + sign*b_diff, sd=sd))
    
    if (mean(a) > mean(b)) {
      H <- a
      L <- b
      cA <- cH
      cB <- cL
    }
    if (mean(a) < mean(b)) {
      H <- b
      L <- a
      cA <- cL
      cB <- cH
    }
    
    dps <- cbind(dps, H, L)
    colnames(dps)[(ncol(dps)-1):ncol(dps)] <- paste(dp, c("H", "L"), sep="")
    
    
    t_a1 <- table(a1)
    t_a2 <- table(a2)
    t_a <- table(a)
    t_b <- table(b)
    
    t_H <- table(H)
    t_L <- table(L)
    
    if (p_rare == 0) e <- "Kind"
    if (p_rare == 0.4) e <- "Moder. wicked"
    if (p_rare == 0.2) e <- "Extrem. wicked"
    
    prec <- 0
    
    stats <- rbind(stats, data.frame("Environment" = e,
                                "p(rare)" = p_rare,
                                "DP" = dp,
                                "M(H)" = round(mean(H), prec),
                                "M(L)" = round(mean(L), prec),
                                "SD(H)" = round(sd(H), prec),
                                "SD(L)" = round(sd(L), prec),
                                "Diff(EVs)" = round(mean(H)-mean(L), prec),
                                "Diff(Modes)" = abs(round(as.numeric(names(t_H[which.max(t_H)])) - as.numeric(names(t_L[which.max(t_L)])), prec)),
                                "N(H)" = length(unique(H)),
                                "N(L)" = length(unique(L)),
                                check.names=F ))
    
    p.lines <- function(t, col="black", l=1) {
      for (i in 1:length(t)) {
        x <- as.numeric(names(t))[i]
        y <- as.numeric(t[i])
        lines(c(x,x), c(0,y), col=col, lty=l, lwd=1, lend=1)
      }
    }
    
    y.max <- max(c(t_a, t_b)) + 20
    
    plot(0, 0, xlim=c(0,c), ylim=c(0, y.max+40), las=1, type="n", xlab="", ylab="", yaxt="n", xaxt="n", frame=0, bg="black")
    
    n <- 7
    axis(1, at=seq(0, c, length.out=n), seq(0, c/scale, length.out=n))
    
    text(x=0, y=y.max+100, xpd=T, paste("DP", dp, sep=""))
    
    if (p_rare == 0) text("Frequency", x=-20, y=y.max/2, srt=90, xpd=T)
    
    if (dp == 1) legend("topright", pch=c(18,20), c("EV(H)", "EV(L)"), col=c(cH, cL), cex=1.1, box.lty=0)
    
    points(x=mean(L), y=y.max+20, pch=20, col=cL, cex=2)
    points(x=mean(H), y=y.max+20, pch=18, col=cH, cex=1.7)
    
    p.lines(t_b, col=cB, l=1)
    
    p.lines(t_a1, col=cA, l=1)
    if (p_rare != 0) p.lines(t_a2, col=cA)
    
    if (p_rare == 0) sign <- sign * -1
    diff <- round(mean(H)-mean(L), 0) / scale
    if (dp <= 4) diff <- paste("+", diff, sep="") else diff <- paste("-", diff, sep="")
    if (dp <= 4) text(x=mean(H) + 20, y=y.max+20, diff, xpd=T)
    else text(x=mean(L) - 20, y=y.max+20, diff, xpd=T)
    
  }
  
  # save DPs as csv-Files
  write.csv(dps, file=paste("../data/main/dps_p", formatC(p_rare*100, width=2, flag="0"), ".csv", sep=""), row.names=F)
  
}

dev.off()



# generate a latex table with all statistical propreties of the DPs
library(xtable)

stats$bimodal <- "none"
stats$bimodal[which(stats$`SD(H)` > stats$`SD(L)` & stats$Environment != "Kind")] <- "H"
stats$bimodal[which(stats$`SD(H)` < stats$`SD(L)` & stats$Environment != "Kind")] <- "L"

tab <- stats[,-which(is.element(colnames(stats), c("N(H)", "N(L)")))]
tab <- apply(tab, 2, as.character)

xtab <- xtable(tab,
               caption="Summary of decision problems used in Studies 1 and 2.",
               label="tab:main_dps",
               type="latex",
               align=paste(c("l", rep("l", ncol(tab))), collapse="")
               )

tmp <- print(xtab,
             caption.placement = "top",
             include.rownames = F,
             sanitize.text.function = identity,
             file="") 
cat(tmp, file=paste("../output/main/tab_dps.tex", sep=""))

r_m <- range(c(stats$`M(H)`, stats$`M(L)`))
r_sd <- range(c(stats$`SD(H)`, stats$`SD(L)`))
print(stats)



# plot EVs and SDs of all choice options
pdf(file="../output/main/env_var.pdf", width=9, height=3)

par(mfcol=c(1,3))

for (p_rare in envs) {
  curr <- subset(stats, `p(rare)` == p_rare)
  ind_Hb <- which(curr$H == "a")
  
  curr$H_pch <- 18
  curr$L_pch <- 20
  
  if (p_rare == 0.0) tmp_cex <- 2.5 else tmp_cex <- 2
    
  plot(curr$`M(L)`, curr$`SD(L)`, col=cL, pch=curr$L_pch, las=1, xlim=c(0,c), ylim=c(0, r_sd[2]), xlab="Expected value", ylab="Standard deviation", cex=tmp_cex, xaxt="n")
  axis(1, at=seq(0, c, length.out=n), seq(0, c/scale, length.out=n))
  
  if (p_rare == 0) legend("topleft", pch=c(18,20), c("H-options", "L-options"), col=c(cH, cL), cex=1.3, box.lty=0)
  
  points(curr$`M(H)`, curr$`SD(H)`, col=cH, pch=curr$H_pch, cex=2) 
  if (p_rare == 0.0) title("Kind environment:\np(rare) = 0", cex.main=1.2)
  if (p_rare == 0.4) title("Moderately wicked environment:\np(rare) = 0.4", cex.main=1.2)
  if (p_rare == 0.2) title("Extremely wicked environment:\np(rare) = 0.2", cex.main=1.2)
    
  for (l in 1:nrow(curr)) {
    if (p_rare != 0) lines(x=c(curr[l, "M(H)"], curr[l, "M(L)"]), y=c(curr[l, "SD(H)"], curr[l, "SD(L)"]), lwd=0.5, col="darkgrey")
  }
}

dev.off()



# make a seperate plot with one example per choice environment
pdf(width=8, height=5, file="../output/main/env_dist2.pdf")

par(mfrow=c(3,1))


for (p_rare in envs) {
  
  if (p_rare == .0) a2_diff <- 0
  if (p_rare == .4) a2_diff <- 105
  if (p_rare == .2) a2_diff <- 210
  
  dps <- NULL  
  for (dp in 6) {
    
    
    par(mar=c(3,2,4,1))
    
    if (dp <= 4) sign <- 1 else sign <- -1
    
    a1 <- round(rnorm(n=1000*(1-p_rare), mean=a1_means[dp], sd=sd))
    
    if (p_rare != 0) a2 <- round(rnorm(n=1000*p_rare, mean=a1_means[dp]+sign*a2_diff, sd=sd)) else a2 <- NULL
    a <- c(a1,a2)
    
    b <- round(rnorm(n=1000, mean=a1_means[dp] + sign*b_diff, sd=sd))
    
    if (mean(a) > mean(b)) {
      H <- a
      L <- b
      cA <- cH
      cB <- cL
    }
    if (mean(a) < mean(b)) {
      H <- b
      L <- a
      cA <- cL
      cB <- cH
    }
    
    dps <- cbind(dps, H, L)
    colnames(dps)[(ncol(dps)-1):ncol(dps)] <- paste(dp, c("H", "L"), sep="")
    
    
    t_a1 <- table(a1)
    t_a2 <- table(a2)
    t_a <- table(a)
    t_b <- table(b)
    
    t_H <- table(H)
    t_L <- table(L)
    
    if (p_rare == 0) e <- "Kind"
    if (p_rare == 0.4) e <- "Moder. wicked"
    if (p_rare == 0.2) e <- "Extrem. wicked"
    
    prec <- 0
    
    p.lines <- function(t, col="black", l=1) {
      for (i in 1:length(t)) {
        x <- as.numeric(names(t))[i]
        y <- as.numeric(t[i])
        lines(c(x,x), c(0,y), col=col, lty=l, lwd=1.5, lend=1)
      }
    }
    
    y.max <- max(c(t_a, t_b)) + 20
    
    plot(0, 0, xlim=c(0,c), ylim=c(0, y.max+40), las=1, type="n", xlab="", ylab="", yaxt="n", xaxt="n", frame=0, bg="black")
    
    if (p_rare == 0) title("Kind environment: p(rare) = 0")
    if (p_rare == 0.4) title("Moderately wicked environment: p(rare) = 0.4")
    if (p_rare == 0.2)
      title("Extremely wicked environment: p(rare) = 0.2")
    
    
    n <- 7
    axis(1, at=seq(0, c, length.out=n), seq(0, c/scale, length.out=n))
    
    #text(x=0, y=y.max+100, xpd=T, paste("DP", dp, sep=""))
    
    text("Frequency", x=0, y=y.max/2, srt=90, xpd=T)
    
    if (p_rare == 0) legend("topright", pch=c(18,20), c("EV(H)", "EV(L)"), col=c(cH, cL), cex=1.3, pt.cex=2, box.lty=0)
    
    points(x=mean(L), y=y.max+20, pch=20, col=cL, cex=2.2)
    points(x=mean(H), y=y.max+20, pch=18, col=cH, cex=2)
    
    p.lines(t_b, col=cB, l=1)
    
    p.lines(t_a1, col=cA, l=1)
    if (p_rare != 0) p.lines(t_a2, col=cA)
    
    if (p_rare == 0) sign <- sign * -1
    diff <- round(mean(H)-mean(L), 0) / scale
    if (dp <= 4) diff <- paste("+", diff, sep="") else diff <- paste("-", diff, sep="")
    if (dp <= 4) text(x=mean(H) + 20, y=y.max+20, diff, xpd=T)
    else text(x=mean(L) - 8, y=y.max+20, diff, xpd=T)
    
  }
  
}

dev.off()