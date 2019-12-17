### This script extracts the regression tables for Latex
### Renato Frey

library(HDInterval)
library(xtable)

# select predictors
sel_IVs <- c("(Intercept)", "condp40", "condp20", "gamble_ind", "modecomp", "condp40:modecomp", "condp20:modecomp", "modecomp:gamble_ind", "condp40:gamble_ind", "condp20:gamble_ind")

for (i in 1:4) {
  
  if (i == 1) load("../objects/main/model_samplesize.Rdata")
  if (i == 2) load("../objects/main/model_efficiency.Rdata")
  if (i == 3) load("../objects/main/model_Hexp.Rdata")
  if (i == 4) load("../objects/main/model_H.Rdata")
  
  if (is.element(i, 1:2)) r_prec <- 1 else r_prec <- 2
  
  if (DV_label == "Efficiency") DV_label <- "Search inefficiency"
  
  coefs_all <- summary(mod_rstan)[,c("50%", "2.5%", "97.5%")]
  sel_IVs_curr <- sel_IVs[is.element(sel_IVs, rownames(coefs_all))]
  #coefs_v1 <- coefs_all[sel_IVs_curr, ]
  
  # get coefficients directly from mcmc samples with HDI function
  mcmc <- as.data.frame(mod_rstan)[sel_IVs_curr]
  coefs <- t(rbind(median=apply(mcmc, 2, median), apply(mcmc, 2, hdi)))
  
  # back-transform models with log link-functions to probabilities
  if (is.element(i, 3:4)) coefs <- rbind("(Intercept)"=exp(coefs[1,]), exp(coefs[1,] + coefs[2:nrow(coefs),]) - exp(coefs[1,]))
  
  coefs <- round(coefs, r_prec)
  
  column <- apply(coefs, 1, function(x) {paste(x[1], " [", x[2], " -- ", x[3], "]", sep="")})
  
  if (i == 1) {
    tab <- data.frame(column)
    tab[,1] <- as.character(tab[,1])
    colnames(tab)[1] <- DV_label
  } else {
    match_i <- match(rownames(tab), names(column))
    tab[,DV_label] <- column[match_i]
  }

}

labels_old <- rownames(tab)

labels_new <- labels_old
labels_new <- gsub("\\(Intercept\\)", "Intercept", labels_new)
labels_new <- gsub("condp40:modecomp", "Competitive x Moderately wicked", labels_new)
labels_new <- gsub("condp20:modecomp", "Competitive x Extremely wicked", labels_new)
labels_new <- gsub("modecomp:gamble_ind", "Trial x Competitive", labels_new)
labels_new <- gsub("condp40:gamble_ind", "Trial x Moderately wicked", labels_new)
labels_new <- gsub("condp20:gamble_ind", "Trial x Extremely wicked", labels_new)
labels_new <- gsub("condp40", "Environment: Moderately wicked", labels_new)
labels_new <- gsub("condp20", "Environment: Extremely wicked", labels_new)
labels_new <- gsub("modecomp", "Search mode: Competitive", labels_new)
labels_new <- gsub("gamble_ind", "Trial", labels_new)

row.names(tab) <- labels_new
#tab <- tab[c(1,6,2,3,4,5),]
tab <- tab[c(1,2,3,5,6,7,4,8,9,10),]

is.credible <- function(string) {
  mean <- unlist(strsplit(string, " \\["))[1]
  confint <- regmatches(string, regexpr('\\[.+\\]', string))
  confint <- gsub("\\[", "", confint)
  confint <- gsub("\\]", "", confint)
  confint <- unlist(strsplit(confint, " -- "))

  if (sum(confint <  0) == 2 | sum(confint > 0) == 2) return(T) else return(F)
}

tab_cred <- apply(tab, c(1,2), is.credible)
tab_disp <- tab
tab_disp[!tab_cred] <- NA
#write.csv(tab_disp, file="../output/pilot/tab_regr.csv")

# print credible coefficients in bold?
tab[tab_cred] <- paste("\\textbf{", tab[tab_cred], "}", sep="")

capt <- "Bayesian regression analyses."

xtab <- xtable(tab,
               caption=capt,
               label="tab:main_regr",
               type="latex",
               align=paste(c("l", rep("l", ncol(tab))), collapse="")
               #align=c("lp{1in} ", rep("rp{1in} ", ncol(tab2)))
)

tmp <- print(xtab,
             caption.placement = "top",
             #table.placement = getOption("xtable.table.placement", "H"),
             floating.environment = "table*",
             sanitize.text.function = identity,
             file="") #file_name2

# remove table placement
#tmp <- gsub("[ht]", "[p]", tmp, fixed=T)

# make tiny
#tmp <- gsub("\\centering", "\\centering \n\\tiny", tmp, fixed=T)

tmp <- gsub("\\end{tabular}", paste("\\multicolumn{", ncol(tab)+1, "}{l}{\\footnotesize Note. Credible coefficients (with highest density intervals excluding 0) are printed in bold. Intercept depicts the reference level ``trial 1 in the solitary} \\\\  \\end{tabular}", sep=""), tmp, fixed=T)

tmp <- gsub("\\end{tabular}", paste("\\multicolumn{", ncol(tab)+1, "}{l}{\\footnotesize mode of the kind environment''. The three-way interactions for sample size were not credible and are not shown in the table.} \\\\  \\end{tabular}", sep=""), tmp, fixed=T)

# alternating row colors
#tmp <- gsub("\\tiny", "\\tiny \ \\rowcolors{2}{gray!25}{white}", tmp, fixed=T)

cat(tmp, file=paste("../output/main/tab_regr.tex", sep=""))