#' ---
#' title: "Test to use KDSA"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("auch.R")

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
auch <- words$X9
# all cognate
table(auch)

# tokenization
# write.profile(auch, file = "auch_graphemes.txt", editing = T)
tok <- tokenize(auch, profile = "auch_graphemes.txt", regex = T)

# alignments to be edited manually
out <- cbind(WORD = auch, COGID = 1, ALIGN = tok$strings$tokenized)
write.table(out, file = "auch_toBeAligned.txt", col.names = NA, quote = FALSE, sep = "\t")

# load manually corrected data
auch <- read.table("auch_aligned.txt", header = TRUE, sep = "\t")

#' ### Maps

# sounds
ALIGN <- as.character(auch$ALIGN)
ALIGN <- sapply(ALIGN,strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
#colnames(ALIGN) <- c("B","R1","E","R2","O","K","T")
ALIGN <- as.data.frame(ALIGN)

# recoding of different symbols
# write recoding template to be edited manually
# write.recoding(ALIGN, file = "auch_recoding_template.yml")
# recode data according to edited recoding template
ALIGN <- recode(ALIGN, "auch_recoding.yml")

# ch
cols <- c(rainbow(4), "grey")
vmap(v, col = cols[ALIGN$CH], border = cols[ALIGN$CH])
legend("bottomright", legend = c(levels(ALIGN$CH)), fill = c(cols), cex = .7)
title(main = "<ch> of 'auch'")

# show Session Info
sessionInfo()


