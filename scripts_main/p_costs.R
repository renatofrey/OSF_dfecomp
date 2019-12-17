### This script analyzes and plots the average-cost curves
### Renato Frey

load("../data/main/prepared.Rdata")
load("../objects/main/sim_choices.Rdata")

set.seed(123)

init_min <- 1
cols <- c("green", "orange", "red")

pdf(file=paste("../output/main/costs_main.pdf", sep=""), width=8, height=4)

layout(cbind(rep(1, 10), rep(2, 10)))
par(mar=c(0,0,0,0))

par(mar=c(4,5,2,2), mgp=c(2.5,1,0))

# two separate panels for solitary and competitive search
for (searchmode in 1:2) {
  
  if (searchmode == 1) parts <- subset(participants, mode == "solo")
  if (searchmode == 2) parts <- subset(participants, mode == "comp")
  
  # store best (lowest) point of each participant
  partbest <- NULL
  
  # loop through all particpants of current condition
  for (part in 1:(nrow(parts)+3)) {
    
    # assume the same discrete choice sensitivity (to be looked up in the simulation analysis), as all participants had values close to 1
    sens <- 1
      
    # get probabilities of choosing H given sens and the choice env.
    ps <- mean_H[[paste("sens_", sens, sep="")]]
    colnames(ps)[which(colnames(ps) == "p0")] <- "p00"
    colnames(ps)[which(colnames(ps) == "p0.4")] <- "p40"
    colnames(ps)[which(colnames(ps) == "p0.2")] <- "p20"
    
    # the lines below set the conditions of three virtual "participants" representing the aggregates of each environment
    if (part - nrow(parts) == 1) {
      cond <- "p00"
      tps <- mean(parts$tps, na.rm=T)
    }
    
    if (part - nrow(parts) == 2) {
      cond <- "p40"
      tps <- mean(subset(parts, cond == cond)$tps, na.rm=T)
    } 
    
    if (part - nrow(parts) == 3) {
      cond <- "p20"
      tps <- mean(subset(parts, cond == cond)$tps, na.rm=T)
    } 
    
    # look up actual "time per sample" (tps) if participant is a real participant
    if (part <= nrow(parts)) {
      tps <- parts$tps[part]
      if (is.na(tps)) next
      cond <- as.character(parts$cond[part])
    }
    
    # define range of sample sizes
    s <- seq(2, 20, by=2)
    
    # costs
    c <- init_min*60 + s * tps
    
    # probability of choosing higher-EV option as a function of sample size
    pH <- ps[[cond]]
    
    # relative payoff if better option is chosen (2 in the kind and 40 in the wicked env.)
    if (cond == "p00") oH <- pH * 2 else oH <- pH * 40
    
    # average costs for the relative expected outcomes
    ac <- c/oH
    
    # use log for avg. search costs?
    if (T) {
      ac <- log(ac)
    }
    
    xl <- c(0, 40)
    yl <- c(.75, 6)
    
    lw = .2
    xtitle = 'Expected reward'
    ytitle = expression('Avg. costs (log'[seconds]*') per expected reward')
    
    # start plotting
    if ((searchmode == 1 & part == 1) | (searchmode == 2 & part == 1)) {
      if (cond == "p00") plot(oH, ac, type="l", las=1, xlim=xl, ylim=yl, col=cols[1], lwd=lw, xlab=xtitle, ylab=ytitle, frame=F, cex.lab=1.2)
      if (cond == "p40") plot(oH, ac, type="l", las=1, xlim=xl, ylim=yl, col=cols[2], lwd=lw, xlab=xtitle, ylab=ytitle, frame=F, cex.lab=1.2)
      if (cond == "p20") plot(oH, ac, type="l", las=1, xlim=xl, ylim=yl, col=cols[3], lwd=lw, xlab=xtitle, ylab=ytitle, frame=F, cex.lab=1.2)
      
      if (searchmode == 1) title("Solitary search")
      if (searchmode == 2) title("Competitive search")
    } else {
      
      if (cond == "p00") pcol = cols[1]
      if (cond == "p40") pcol = cols[2]
      if (cond == "p20") pcol = cols[3]
      plwd = lw
      if (part > nrow(parts)) {
        pcol = "black"
        plwd = 1.2
      }
      points(oH, ac, type="l", col=pcol, lwd=plwd)
    }
    
    
    ind1 <- c(1, 2, 3, 4, 5, 6, 8, 10)
    ind2 <- c(1, 2, 3, 4, 5, 6, 8, 10)
    
    if (cond == "p00") pc <- cols[1]
    if (cond == "p40") pc <- cols[2]
    if (cond == "p20") pc <- cols[3]
    
    points(oH[ind1], ac[ind1], pch=19, col=pc, cex=0.1)
    
    
    samples <- parts[part,"samples"]
    ind <- which.min(abs(samples - s))
    curr_x <- oH[ind] + rnorm(mean=0, sd=0.3, 1)
    if (cond == "p00") curr_x <- curr_x + 1.5
    curr_y <- ac[ind]
    #points(curr_x, curr_y, pch=4, cex=1.2, col="black")
    #points(curr_x, curr_y, pch=23, cex=.8, bg=pc)
    if (part <= nrow(parts) & length(curr_x) != 0) partbest <- rbind(partbest, data.frame(x=curr_x, y=curr_y, pc=pc))
    
    # if this is the last participant, loop through all participants again to draw the diamonds
    if (part == nrow(parts)) {
      for (k in 1:nrow(partbest)) {
        points(partbest[k,1], partbest[k,2], pch=23, cex=.8, bg=as.character(partbest[k,3]))
      }  
    }

    # if this is one of the virtual "participants", plot the average lines
    if (part > nrow(parts)) {
      points(oH[ind1], ac[ind1], pch=21, bg=pc, cex=2.2)
      tcex <- .7
      text(oH[ind1], ac[ind1], as.character(s[ind1]), cex=tcex)
    }
    
  } 
  
  if (searchmode == 2) legend("topright", box.lty=0, c("Kind environment", "Moderately wicked env.", "Extremely wicked env."), lty=1, lwd=2, col=cols, horiz=F)

}  
  
dev.off()