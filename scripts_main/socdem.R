### This script extracts participants' basic socio-demographic data
### (c) Renato Frey

library(xtable)

load("../data/main/prepared.Rdata")


participants$yearsofedu <- as.numeric(as.character(participants$yearsofedu))

vars <- c("sex", "age", "job", "income", "yearsofedu")

dat <- cbind(participants[,vars])

tab <- rbind(c("N", "", paste("", as.character(nrow(dat)), sep="")))


for (var in c(vars)) {
  
  vals <- dat[,var]
  levels(vals)[which(levels(vals) == "")] <- "NA"
  
  if (var == "income") {
    if (!is.element("4k_5k", levels(vals))) levels(vals) <- c(levels(vals), "4k_5k")
  }
  
  if (is.numeric(vals)) {

    out <- paste("M=", round(mean(vals, na.rm=T), 1), " (SD=", round(sd(vals, na.rm=T), 1), ")", sep="")
    
    out <- cbind(out)
    curr_out <- cbind("", out)
  }
  
  if (is.factor(vals)) {
    out <- as.data.frame(table(vals))
    out_perc <- paste(round(prop.table(out[,2])*100, 1), "%", sep="")
    #out[,2] <- paste(out[,2], " / ", out_perc, sep="")
    out[,2] <- paste("", out[,2], " (", out_perc, ")", sep="")
  
    curr_out <- out
    
    na_ind <- which(curr_out$vals == "NA")
    if (length(na_ind > 0)) {
      curr_out <- curr_out[c(2:nrow(curr_out), na_ind),]
    }
    
  }
  
  
  curr_out <- cbind(var, curr_out)
  if (nrow(curr_out) > 1) {
    curr_out$var <- as.character(curr_out$var)
    curr_out$var[2:nrow(curr_out)] <- ""
  }
  
  add_matrix <- as.matrix(curr_out)
  
  colnames(add_matrix) <- NULL
  tab <- rbind(tab, add_matrix)
  
  print(tab)
  
}


labels_new <- tab[,1]
labels_new <- gsub("age", "Age", labels_new)
labels_new <- gsub("sex", "Sex", labels_new)
labels_new <- gsub("job", "Occupation", labels_new)
labels_new <- gsub("income", "Monthly net income", labels_new)
labels_new <- gsub("yearsofedu", "Years of education", labels_new)
tab[,1] <- labels_new

labels_new <- tab[,2]
ind_income <- which(grepl("_", labels_new))
tab[ind_income,] <- tab[match(sort(labels_new[ind_income]), labels_new),]
labels_new <- tab[,2]

labels_new <- gsub("female", "Female", labels_new)
labels_new <- gsub("\\bmale", "Male", labels_new)
labels_new <- gsub("\\bstudent", "Student", labels_new)
labels_new <- gsub("\\bemployed", "Employed", labels_new)
labels_new <- gsub("\\bunemployed", "Unemployed", labels_new)
labels_new <- gsub("\\bselfemployed", "Self-employed", labels_new)
labels_new <- gsub("\\bretired", "Retired", labels_new)
labels_new <- gsub("0_1k", "0 - 999 USD", labels_new)
labels_new <- gsub("1k_2k", "1,000 - 1,999 USD", labels_new)
labels_new <- gsub("2k_3k", "2,000 - 2,999 USD", labels_new)
labels_new <- gsub("3k_4k", "3,000 - 3,999 USD", labels_new)
labels_new <- gsub("4k_5k", "4,000 - 4,999 USD", labels_new)
labels_new <- gsub("5k_x", "> 5,000 USD", labels_new)
tab[,2] <- labels_new


#tab <- tab[c(1:5,17,6,8,10,9,7,11:16),]

xtab <- xtable(tab,
               type="latex",
               label=paste("tab:main_socdem"),
               caption=paste("Sociodemographic information."),
               align=c("llll"),
               #align=c("lp{1in} ", rep("rp{1in} ", ncol(tab2)))
)

output <- print(xtab,
                include.rownames=F,
                include.colnames=F,
                table.placement = getOption("xtable.table.placement", "tb"),
                caption.placement = "top",
                file="")


output <- sub("\\centering", "\\centering \n\\renewcommand{\\arraystretch}{1.3}", output, fixed=T)

output <- sub("\\hline\n  \\hline\n", "\\hline\n", output, fixed=T)


output <- gsub("\\begin{table}", "\\begin{table*}", output, fixed=T)
output <- gsub("\\end{table}", "\\end{table*}", output, fixed=T)


cat(output)

cat(output, file="../output/main/tab_socdem.tex")