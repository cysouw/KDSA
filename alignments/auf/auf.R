#' ---
#' title: "auf"
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
word <- words$X10

#' ### Alignment

# tokenization
tok <- tokenize(word, profile = c("[aäeioöuüj]+h*","[bpf]+","d","m","w","n","r","v","h","s","t","z"), regex = T)

# alignments to be edited manually
out <- cbind(WORD = word, COGID = 1, ALIGN = tok$strings$tokenized)
write.table(out, file = "auf_toBeAligned.txt", col.names = NA, quote = FALSE, sep = "\t")

# load manually corrected data
align <- read.table("auf_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# sounds in "auf"
ALIGN <- as.character(align$ALIGN)
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# recoding of different symbols
# write recoding template to be edited manually
write.recoding(ALIGN, file = "apfel_recoding_template.yml")
# recode data according to edited recoding template
ALIGN <- recode(ALIGN, "apfel_recoding.yml")

#' ### Maps

# a
cols <- c("darkred", "red", "purple", "blue", "green")
vmap(v, col = cols[ALIGN$A], border = NA)
legend("bottomright", legend = levels(ALIGN$A), fill = cols, cex = .7)
title(main = "<a> of 'Apfel'")

# pf
cols <- c("darkred", "darkblue", "green", "red", "purple", "blue", "darkgreen")
vmap(v, col = cols[ALIGN$PF], border = NA)
legend("bottomright", legend = levels(ALIGN$PF), fill = cols, cex = .7)
title(main = "<pf> of 'Apfel'")

# e
cols <- c("grey", "red", "darkred", "blue", "darkblue", "green", "yellow", "orange", "black")
vmap(v, col = cols[ALIGN$E], border = NA)
legend("bottomright", legend = levels(ALIGN$E), fill = cols, cex = .7)
title(main = "<e> of 'Apfel'")

# r
cols <- c("grey", "red")
vmap(v, col = cols[ALIGN$R], border = NA)
legend("bottomright", legend = levels(ALIGN$R), fill = cols, cex = .7)
title(main = "<r> of 'ApfeRl'")

# l
cols <- c("grey", "red")
vmap(v, col = cols[ALIGN$L2], border = NA)
legend("bottomright", legend = levels(ALIGN$L2), fill = cols, cex = .7)
title(main = "<l> of 'Apfel'")

# last vowel
cols <- c("grey", "red")
vmap(v, col = cols[ALIGN$E2], border = NA)
legend("bottomright", legend = levels(ALIGN$E2), fill = cols, cex = .7)
title(main = "additional vowel after <l> of 'Apfel'")

# show Session Info
sessionInfo()


