#' ---
#' title: "Test to use KDSA"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("brot.R")

library(qlcData)
library(qlcVisualize)
library(qlcMatrix)

#' ### Load data

# Coordinates of villages
coor <- read.delim("../../data/coor.txt", row.names = 1)
# KDSA words
words <- read.delim("../../data/kdsa.txt", row.names = 1)
# load basemap
load("../../data/KDSAvoronoi.Rdata")

#' ### Cognates and alignment

# example "Wurst"
brot <- words$X29
# all cognate
table(brot)

# tokenization
# write.profile(brot, file = "brot_graphemes.txt", editing = T)
tok <- tokenize(brot, profile = "brot_graphemes.txt", regex = T)

# alignments to be edited manually
out <- cbind(WORD = brot, COGID = 1, ALIGN = tok$strings$tokenized)
write.table(out, file = "brot_toBeAligned.txt", col.names = NA, quote = FALSE, sep = "\t")

# load manually corrected data
brot <- read.table("brot_aligned.txt", header = TRUE, sep = "\t")

#' ### Maps

# sounds in "wurst"
ALIGN <- as.character(brot$ALIGN)
ALIGN <- sapply(ALIGN,strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
colnames(ALIGN) <- c("B","R1","E","R2","O","K","T")
ALIGN <- as.data.frame(ALIGN)

# recoding of different symbols
# write recoding template to be edited manually
write.recoding(ALIGN, file = "wurst_recoding_template.yml")
# recode data according to edited recoding template
ALIGN <- recode(ALIGN, "wurst_recoding.yml")

# b
cols <- c("yellow", "grey", "red")
vmap(v, col = cols[ALIGN$B], border = NA)
legend("bottomright", legend = c(levels(ALIGN$B)), fill = c(cols), cex = .7)
title(main = "<b> of 'Brot'")

# t
cols <- rainbow(8)
vmap(v, col = cols[ALIGN$"T"], border = NA)
legend("bottomright", legend = c(levels(ALIGN$U),"(NC)"), fill = c(cols,"white"), cex = .7)
title(main = "<u> of 'Wurst'")



# show Session Info
sessionInfo()


