### This script plots the results for the proportions of Hexp-choices
### (c) Renato Frey

plot_aggr <- T

load("../data/pilot/prepared.Rdata")
load("../objects/pilot/model_Hexp.Rdata")

pdf(width=10, height=4, file="../output/pilot/choiceHexp.pdf")


library(viridis)
cols <- viridis(2, begin=0.3, end=.6)

library(scales)
cols_trans <- alpha(cols, 0.4)

# don't plot bars?
if (F) cols <- "white"

panel_offset <- -1

p.bars <- function(values, side, panel) {
  space <- 0.05
  if (side == "r") p_col <- cols[1]
  if (side == "l") p_col <- cols[2]
  if (panel == "b") values <- values - 11 + panel_offset
  
  values <- as.factor(values)
  levels(values) <- union(levels(values), c("0", "0.25", "0.5", "0.75", "1"))
  
  tbl <- table(values)
  if (side == "l") tbl <- tbl * -1
  
  #if (side == "r") p_add<- T else p_add = T
  #b <- barplot(tbl, horiz=T, col=p_col, border=0, las=1, xaxt="n", yaxt="n")
  
  bottom <- as.numeric(names(tbl)) - 0.03
  top <- as.numeric(names(tbl)) + 0.03
  left <- rep(0, length(bottom))
  right <- as.numeric(tbl)
  rect(left, bottom, right, top, col=p_col, border=F)
}


# ranges
if (plot_aggr == T) {
  x.min <- -50
  x.max <- x.min * -1
  
  y.min <- 0
  y.max <- 1
} else {
  x.min <- -40
  x.max <- x.min * -1
  
  y.min <- 0
  y.max <- 70
}

# set up layout
nf <- layout(mat = matrix(c(1:5),1,5, byrow=TRUE),  width = c(0.3, rep(c(1), 4)))


# empty plot for y-axis label (first panel)
par(mar=c(0, 0, 0, 0))
plot(NA, xlim=c(0, x.max), ylim=c(y.min, y.max), xaxt="n", yaxt="n", frame=F)
text(15, y.max/2, pos=3, "Proportion of Hexp-choices", cex=1.8, srt=90, xpd=T)


# actual plots in subsequent panels
for (i in 1:4) {
  
  if (i == 1) sel_cond <- "kind"
  if (i == 2) sel_cond <- "wick0.20"
  if (i == 3) sel_cond <- "wick0.15"
  if (i == 4) sel_cond <- "wick0.10"
  
  p_labs <- c("Kind", "Mildly Wicked", "Moderately Wicked", "Extremely Wicked")
  
  par(mar=c(3, 2, 3, 1))
  
  # empty plot for labels
  plot(NA, xlim=c(x.min, x.max), ylim=c(y.min-.1, y.max+.1), xaxt="n", yaxt="n", main=p_labs[i], cex.main=1.5, xlab="N Participants", ylab="", mgp=c(1.8,0,0), frame=F)
  
  #abline(h=0.5, lty=2, lwd=0.5)
  
  axis(1, abs(seq(x.min, x.max, length.out=5)), at=seq(x.min, x.max, length.out=5), mgp=c(1.7,0.7,0), cex.axis=1)
  
  if (plot_aggr == T) {
    text(0, y.max+.08, pos=2, "Solitary", xpd=T, cex=1.2)
    text(0, y.max+.08, pos=4, "Competitive", xpd=T, cex=1.2)
  } else {
    text(-17, y.max, pos=3, "Solitary", xpd=T, cex=1.2)
    text(22.5, y.max, pos=3, "Competitive", xpd=T, cex=1.2)
  }
  
  # barplots
  round_dec <- 2
  if (plot_aggr == T) {
    p.bars(round(subset(stat_perpart_long, mode == "solo" & cond == sel_cond)$Hexp, round_dec), side="l", panel="t")
    p.bars(round(subset(stat_perpart_long, mode == "comp" & cond == sel_cond)$Hexp, round_dec), side="r", panel="t")
  } else {
    p.bars(subset(stat_perdec, mode == "solo" & cond == sel_cond)$H, side="l", panel="t")
    p.bars(subset(stat_perdec, mode == "comp" & cond == sel_cond)$H, side="r", panel="t")
  }
  #axis(2, at=c(1,4,7,10), las=1, mgp=c(1,0.6,0), cex.axis=1.2)
  axis(2, at=seq(0, y.max, length.out=5), las=1, mgp=c(1,0.6,0), cex.axis=1.2)
  lines(c(0,0), c(0,y.max), lwd=0.5)
  
  
  if (T) {
  
    p_meanlength <- x.max
    p_lwd <- 1
    
    for (sel_mode in c("solo", "comp")) {
      if (sel_mode == "solo") p_neg <- -1 else p_neg <- 1
      
      sel_estimates <- subset(estimates, cond == sel_cond & mode == sel_mode)
      rect(p_neg * p_meanlength, sel_estimates[1,"lower"], 0, sel_estimates[1,"upper"], col="red", density=40, border=NA)
      lines(c(p_neg * p_meanlength,0), rep(sel_estimates[1,"mean",1], 2), lwd=2, col="darkred")
    }
      
  }
  
  
  
  
  if (F) {
    boxplot(list(de_pat[,pref_dvs[i]], de_sur[,pref_dvs[i]]), horizontal=F, outline=T, ylim=ylim, frame=F, xaxt="n", yaxt="n", col=cols[2:1])
    #axis(2, seq(min(values), max(values), length.out=4), las=1)
    
    boxplot(list(ch_pat[,pref_dvs[i]]-11+panel_offset, ch_sur[,pref_dvs[i]]-11+panel_offset), horizontal=F, outline=T, ylim=ylim, frame=F, xaxt="n", yaxt="n", col=cols[2:1], add=T)
    #axis(2, at=c(-10,-7,-4,-1), c(1, 4, 7, 10), las=1)
    
    text(c(1,2), c(11.5,11.5), pos=1, c("Pat.", "Sur."), xpd=T, cex=.8)
  }
  
  p_rawdens <- F
  
  p_maxwdith <- 500
  p_borders <- F
  p_cut <- 0.5
  p_bw <- 1
  
  if (F) {
    
    library(beanplot)
    
    b1 <- beanplot(de_sur_w[,pref_dvs[i]], bw=p_bw, what=c(0,1,0,0), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border=p_borders, ylab="Preference", ylim=c(1,10), yaxt="n", log="", col=list(c(cols_trans[1], "white", 1)), maxwidth=p_maxwdith, add=T, at=0, side="second", cut=p_cut)
    
    if (p_rawdens == T) beanplot(de_sur[,pref_dvs[i]], bw=p_bw, what=c(0,1,0,0), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border="orange", ylab="Preference", ylim=c(1,10), yaxt="n", log="", col=list(c(alpha("red", 0), "white", 1)), maxwidth=p_maxwdith, add=T, at=0, side="second", cut=p_cut)
    
    
    b2 <- beanplot(de_pat_w[,pref_dvs[i]], bw=p_bw, what=c(0,1,0,0), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border=p_borders, ylab="Preference", ylim=c(1,10), yaxt="n", log="", col=list(c(cols_trans[2], "white", 1)), maxwidth=p_maxwdith, add=T, at=0, side="first", cut=p_cut)
    
    if (p_rawdens == T) beanplot(de_pat[,pref_dvs[i]], bw=p_bw, what=c(0,1,0,0), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border="orange", ylab="Preference", ylim=c(1,10), yaxt="n", log="", col=list(c(alpha("red", 0), "white", 1)), maxwidth=p_maxwdith, add=T, at=0, side="first", cut=p_cut)
    
    
    b3 <- beanplot(ch_sur_w[,pref_dvs[i]]-11+panel_offset, bw=p_bw, what=c(0,1,0,0), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border=p_borders, ylab="Preference", ylim=c(1,10), yaxt="n", log="", col=list(c(cols_trans[1], "white", 1)), maxwidth=p_maxwdith, add=T, at=0, side="second", cut=p_cut)
    
    if (p_rawdens == T) beanplot(ch_sur[,pref_dvs[i]]-11+panel_offset, bw=p_bw, what=c(0,1,0,0), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border="orange", ylab="Preference", ylim=c(1,10), yaxt="n", log="", col=list(c(alpha("red", 0), "white", 1)), maxwidth=p_maxwdith, add=T, at=0, side="second", cut=p_cut)
    
    
    b4 <- beanplot(ch_pat_w[,pref_dvs[i]]-11+panel_offset, bw=p_bw, what=c(0,1,0,0), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border=p_borders, ylab="Preference", ylim=c(1,10), yaxt="n", log="", col=list(c(cols_trans[2], "white", 1)), maxwidth=p_maxwdith, add=T, at=0, side="first", cut=p_cut)
    
    if (p_rawdens == T) beanplot(ch_pat[,pref_dvs[i]]-11+panel_offset, bw=p_bw, what=c(0,1,0,0), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border="orange", ylab="Preference", ylim=c(1,10), yaxt="n", log="", col=list(c(alpha("red", 0), "white", 1)), maxwidth=p_maxwdith, add=T, at=0, side="first", cut=p_cut)
    
  }
  
  
  
  p_meanlength <- 350
  p_lwd <- 1
  
  # add HDIs?
  if (F) {
    min_diff <- .1
    
    if (F) {
      HDI_de_pat <- mcmc_stat[[pref_dvs[i]]]["de_pat",]
      if (diff(HDI_de_pat[2:3]) < min_diff) {HDI_de_pat[3] <- HDI_de_pat[1] - min_diff/2; HDI_de_pat[4] <- HDI_de_pat[1] + min_diff/2}
      HDI_de_sur <- mcmc_stat[[pref_dvs[i]]]["de_sur",]
      if (diff(HDI_de_sur[3:4]) < min_diff) {HDI_de_sur[3] <- HDI_de_sur[1] - min_diff/2; HDI_de_sur[4] <- HDI_de_sur[1] + min_diff/2}
      HDI_ch_pat <- mcmc_stat[[pref_dvs[i]]]["ch_pat",]
      if (diff(HDI_ch_pat[3:4]) < min_diff) {HDI_ch_pat[3] <- HDI_ch_pat[1] - min_diff/2; HDI_ch_pat[4] <- HDI_ch_pat[1] + min_diff/2}
      HDI_ch_sur <- mcmc_stat[[pref_dvs[i]]]["ch_sur",]
      if (diff(HDI_ch_sur[3:4]) < min_diff) {HDI_ch_sur[3] <- HDI_ch_sur[1] - min_diff/2; HDI_ch_sur[4] <- HDI_ch_sur[1] + min_diff/2}
    }
    
    HDI_de_pat <- unlist(means[paste(pref_dvs[i], "pat_de", sep="_"),])
    if (diff(HDI_de_pat[2:3]) < min_diff) {HDI_de_pat[2] <- HDI_de_pat[1] - min_diff/2; HDI_de_pat[3] <- HDI_de_pat[1] + min_diff/2}
    HDI_de_sur <- unlist(means[paste(pref_dvs[i], "sur_de", sep="_"),])
    if (diff(HDI_de_sur[2:3]) < min_diff) {HDI_de_sur[2] <- HDI_de_sur[1] - min_diff/2; HDI_de_sur[3] <- HDI_de_sur[1] + min_diff/2}
    HDI_ch_pat <- unlist(means[paste(pref_dvs[i], "pat_ch", sep="_"),])
    if (diff(HDI_ch_pat[2:3]) < min_diff) {HDI_ch_pat[2] <- HDI_ch_pat[1] - min_diff/2; HDI_ch_pat[3] <- HDI_ch_pat[1] + min_diff/2}
    HDI_ch_sur <- unlist(means[paste(pref_dvs[i], "sur_ch", sep="_"),])
    if (diff(HDI_ch_sur[2:3]) < min_diff) {HDI_ch_sur[2] <- HDI_ch_sur[1] - min_diff/2; HDI_ch_sur[3] <- HDI_ch_sur[1] + min_diff/2}
    
    
    rect(-p_meanlength, HDI_de_pat[2], 0, HDI_de_pat[3], col="red", density=50, border=NA)
    lines(c(-p_meanlength,0), rep(HDI_de_pat[1], 2), lwd=p_lwd)
    
    rect(0, HDI_de_sur[2], p_meanlength, HDI_de_sur[3],  col="red", density=50, border=NA)
    lines(c(0,p_meanlength), rep(HDI_de_sur[1], 2), lwd=p_lwd)
    
    rect(-p_meanlength, HDI_ch_pat[2]-11+panel_offset, 0, HDI_ch_pat[3]-11+panel_offset, col="red", density=50, border=NA)
    lines(c(-p_meanlength,0), rep(HDI_ch_pat[1]-11+panel_offset, 2), lwd=p_lwd)
    
    rect(0, HDI_ch_sur[2]-11+panel_offset, p_meanlength, HDI_ch_sur[3]-11+panel_offset, col="red", density=50, border=NA)
    lines(c(0,p_meanlength), rep(HDI_ch_sur[1]-11+panel_offset, 2), lwd=p_lwd)
    
  }
  
  # add mean lines?
  if (F) {
    lcol <- "orange"
    lines(c(0,p_meanlength), rep(mean(de_sur[,pref_dvs[i]]), 2), lwd=p_lwd, col=lcol)
    lines(c(-p_meanlength,0), c(mean(de_pat[,pref_dvs[i]]), mean(de_pat[,pref_dvs[i]])), lwd=p_lwd, col=lcol)
    lines(c(0,p_meanlength), c(mean(ch_sur[,pref_dvs[i]])-11+panel_offset, mean(ch_sur[,pref_dvs[i]])-11+panel_offset), lwd=p_lwd, col=lcol)
    lines(c(-p_meanlength,0), c(mean(ch_pat[,pref_dvs[i]])-11+panel_offset, mean(ch_pat[,pref_dvs[i]])-11+panel_offset), lwd=p_lwd, col=lcol)
  }
  
  
}

dev.off()