### This script plots the results for samplesize
### (c) Renato Frey

plot_aggr <- T

load("../data/main/prepared.Rdata")
load("../objects/main/estimates.Rdata")

pdf(width=10, height=4, file="../output/main/samplesize.pdf")


library(viridis)
cols <- viridis(2, begin=0.3, end=.6)

library(scales)
cols_trans <- alpha(cols, 0.4)

panel_offset <- -1

p.bars <- function(values, side, panel) {
  space <- 0.05
  if (side == "r") p_col <- cols[1]
  if (side == "l") p_col <- cols[2]
  if (panel == "b") values <- values - 11 + panel_offset
  tbl <- table(values)
  if (side == "l") tbl <- tbl * -1
  bottom <- (-0.5+space) + do.call('seq', as.list(range(values, na.rm=T)))
  top <- bottom + (2 * (0.5-space))
  left <- rep(0, length(bottom))
  right <- tbl
  rect(left, bottom, right, top, col=p_col, border=F)
}


# ranges
if (plot_aggr == T) {
  x.min <- -30
  x.max <- x.min * -1
  
  y.min <- 0
  y.max <- 40
} else {
  x.min <- -50
  x.max <- x.min * -1
  
  y.min <- 0
  y.max <- 60
}

# set up layout
nf <- layout(mat = matrix(c(1:4),1,4, byrow=TRUE),  width = c(0.3, rep(c(1), 3)))


# empty plot for y-axis label (first panel)
par(mar=c(0, 0, 0, 0))
plot(NA, xlim=c(0, 30), ylim=c(y.min, y.max), xaxt="n", yaxt="n", frame=F)
text(30, y.max/2, pos=3, "Search effort:\nSample size", cex=1.8, srt=90, xpd=T)


# actual plots in subsequent panels
for (i in 1:3) {
  
  if (i == 1) sel_cond <- "p00"
  if (i == 2) sel_cond <- "p40"
  if (i == 3) sel_cond <- "p20"
  
  p_labs <- c("Kind", "Moderately Wicked", "Extremely Wicked")
  
  par(mar=c(3, 2, 3, 1))
  
  # empty plot for labels
  plot(NA, xlim=c(x.min, x.max), ylim=c(y.min, y.max), xaxt="n", yaxt="n", main=p_labs[i], cex.main=1.5, xlab="N Participants", ylab="", mgp=c(1.8,0,0), frame=F)
  
  axis(1, abs(seq(x.min, x.max, length.out=5)), at=seq(x.min, x.max, length.out=5), mgp=c(1.7,0.7,0), cex.axis=1)
  
  if (plot_aggr == T) {
    text(0, y.max+.08, pos=2, "Solitary", xpd=T, cex=1.2)
    text(0, y.max+.08, pos=4, "Competitive", xpd=T, cex=1.2)
  } else {
    text(0, y.max+1.5, pos=2, "Solitary", xpd=T, cex=1.2)
    text(0, y.max+1.5, pos=4, "Competitive", xpd=T, cex=1.2)
  }
  
  # barplots
  round_dec <- 0
  if (plot_aggr == T) {
    p.bars(round(subset(stat_perpart, mode == "solo" & cond == sel_cond)$samples, round_dec), side="l", panel="t")
    p.bars(round(subset(stat_perpart, mode == "comp" & cond == sel_cond)$samples, round_dec), side="r", panel="t")
  } else {
    p.bars(subset(stat_perdec, mode == "solo" & cond == sel_cond)$samples, side="l", panel="t")
    p.bars(subset(stat_perdec, mode == "comp" & cond == sel_cond)$samples, side="r", panel="t")
  }
  #axis(2, at=c(1,4,7,10), las=1, mgp=c(1,0.6,0), cex.axis=1.2)
  axis(2, at=seq(0, y.max, length.out=5), las=1, mgp=c(1,0.6,0), cex.axis=1.2)
  lines(c(0,0), c(0,y.max), lwd=0.5)
  
  
  p_meanlength <- .75*x.max
  p_lwd <- 1
  
  for (sel_mode in c("solo", "comp")) {
    if (sel_mode == "solo") p_neg <- -1 else p_neg <- 1
    
    sel_estimates <- subset(est$samplesize, cond == sel_cond & mode == sel_mode)
    rect(p_neg * p_meanlength, sel_estimates[1,"lower"], 0, sel_estimates[1,"upper"], col="red", density=40, border=NA)
    lines(c(p_neg * p_meanlength,0), rep(sel_estimates[1,"median",1], 2), lwd=2, col="darkred")
  }
  
  

}

dev.off()