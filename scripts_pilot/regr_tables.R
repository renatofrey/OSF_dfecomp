### This script extracts the regression tables for Latex
### (c) Renato Frey

library(xtable)

sel_model <- "stan_glm"

# select predictors
sel_IVs <- c("(Intercept)", "condwick0.20", "condwick0.15", "condwick0.10", "gamble_ind", "modecomp", "condwick0.20:modecomp", "condwick0.15:modecomp", "condwick0.10:modecomp")

for (i in c(1,3,2)) {
  
  if (i == 1) load("../objects/pilot/model_samplesize.Rdata")
  if (i == 2) load("../objects/pilot/model_H.Rdata")
  if (i == 3) load("../objects/pilot/model_Hexp.Rdata")
  
  # fix for newer rstanarm versions
  mod_rstan$stan_function <- "stan_glmer"
    
  if (i == 1) r_prec <- 1 else r_prec <- 2
  
  if (sel_model == "stan_glm") coefs <- round(summary(mod_rstan)[,c("mean", "2.5%", "97.5%")], r_prec)
  
  sel_IVs_curr <- sel_IVs[is.element(sel_IVs, rownames(coefs))]
  coefs <- coefs[sel_IVs_curr, ]
  
  # back-transform models with log link-functions to probabilities
  if (i == 2 | i == 3) coefs <- round(rbind("(Intercept)"=exp(coefs[1,]), exp(coefs[1,] + coefs[2:nrow(coefs),]) - exp(coefs[1,])), r_prec)
  
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
labels_new <- gsub("condwick0.20:modecomp", "Mildly w. x Competitive", labels_new)
labels_new <- gsub("condwick0.15:modecomp", "Moderately w. x Competitive", labels_new)
labels_new <- gsub("condwick0.10:modecomp", "Extremely w. x Competitive", labels_new)
labels_new <- gsub("condwick0.20", "Environment: Mildly wicked", labels_new)
labels_new <- gsub("condwick0.15", "Environment: Moderately wicked", labels_new)
labels_new <- gsub("condwick0.10", "Environment: Extremely wicked", labels_new)
labels_new <- gsub("modecomp", "Search mode: Competitive", labels_new)
labels_new <- gsub("gamble_ind", "Trial index", labels_new)

row.names(tab) <- labels_new
#tab <- tab[c(1,6,2,3,4,5),]
tab <- tab[c(1,2,3,4,6,7,8,9,5),]

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


if (sel_model == "glmulti") capt <- "Multi-model inference results"
if (sel_model == "stan_glm") capt <- "Bayesian regression analyses (pilot studies)."
if (sel_model == "lm") capt <- "Ordinary-least-squares results"

xtab <- xtable(tab,
               caption=capt,
               label="tab:pilot_regr",
               type="latex",
               align=paste(c("l", rep("l", ncol(tab))), collapse="")
               #align=c("lp{1in} ", rep("rp{1in} ", ncol(tab2)))
)

tmp <- print(xtab,
             caption.placement = "top",
             #table.placement = getOption("xtable.table.placement", "H"),
             sanitize.text.function = identity,
             file="") #file_name2

# remove table placement
#tmp <- gsub("[ht]", "[p]", tmp, fixed=T)

# make tiny
#tmp <- gsub("\\centering", "\\centering \n\\tiny", tmp, fixed=T)

tmp <- gsub("\\end{tabular}", paste("\\multicolumn{", ncol(tab)+1, "}{l}{\\footnotesize Note. Credible coefficients (with highest density intervals excluding 0) are printed in bold. Intercept depicts the reference} \\\\  \\end{tabular}", sep=""), tmp, fixed=T)

tmp <- gsub("\\end{tabular}", paste("\\multicolumn{", ncol(tab)+1, "}{l}{\\footnotesize level ``trial 1 in the solitary mode of the kind environment''.} \\\\  \\end{tabular}", sep=""), tmp, fixed=T)

# alternating row colors
#tmp <- gsub("\\tiny", "\\tiny \ \\rowcolors{2}{gray!25}{white}", tmp, fixed=T)

cat(tmp, file=paste("../output/pilot/tab_regr.tex", sep=""))