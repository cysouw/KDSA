#' ---
#' title: "Test to use KDSA"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("apfel.R")

library(qlcData)
library(qlcVisualize)
library(qlcMatrix)

#' ### Load data

# Coordinates of villages
coor <- read.delim("../../data/coor.txt", row.names = 1)
# load basemap
load("../../data/KDSAvoronoi.Rdata")
# KDSA words
words <- read.delim("../../data/kdsa.txt", row.names = 1)
# example "Apfel"
apfel <- words$X8

#' ### Alignment

# tokenization
tok <- tokenize(apfel, profile = c("[aäeioöuüj]+h*","[bpf]+(ch)*","d","h","l+","n","r","v"), regex = T)

# alignments to be edited manually
out <- cbind(WORD = apfel, COGID = 1, ALIGN = tok$strings$tokenized)
write.table(out, file = "apfel_toBeAligned.txt", col.names = NA, quote = FALSE, sep = "\t")

# load manually corrected data
apfel <- read.table("apfel_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# sounds in "wurst"
ALIGN <- as.character(apfel$ALIGN)
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# recoding of different symbols
# write recoding template to be edited manually
write.recoding(ALIGN, file = "apfel_recoding_template.yml")
# recode data according to edited recoding template
RECODE <- recode(ALIGN, "apfel_recoding.yml")

#' ### Maps

# a
cols <- c("grey", "red", "darkred", "orange","blue")
vmap(v, col = cols[RECODE$A], border = NA)
legend("bottomright", legend = levels(RECODE$A), fill = cols, cex = .7)
title(main = "<a> of 'Apfel'")

# pf
cols <- c("darkred", "blue", "green", "red", "grey")
vmap(v, col = cols[RECODE$PF], border = NA)
legend("bottomright", legend = levels(RECODE$PF), fill = cols, cex = .7)
title(main = "<pf> of 'Apfel'")

# e
cols <- c("grey40", "blue", "orange", "grey", "green", "red", "yellow")
vmap(v, col = cols[RECODE$E], border = NA)
legend("bottomright", legend = levels(RECODE$E), fill = cols, cex = .7)
title(main = "<e> of 'Apfel'")

# r
cols <- c("grey", "red")
vmap(v, col = cols[RECODE$R], border = NA)
legend("bottomright", legend = levels(RECODE$R), fill = cols, cex = .7)
title(main = "<r> of 'ApfeRl'")

# l
cols <- c("red", "grey", "grey40")
vmap(v, col = cols[RECODE$L], border = NA)
legend("bottomright", legend = levels(RECODE$L), fill = cols, cex = .7)
title(main = "<l> of 'Apfel'")

# show Session Info
sessionInfo()


