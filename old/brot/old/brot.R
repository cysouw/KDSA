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

# example "Brot"
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

# sounds in "brot"
ALIGN <- as.character(brot$ALIGN)
ALIGN[is.na(brot$WORD)] <- NA
ALIGN <- sapply(ALIGN,strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# recoding of different symbols
# write recoding template to be edited manually
write.recoding(ALIGN, file = "brot_recoding_template.yml")
# recode data according to edited recoding template
RECODE <- recode(ALIGN, "brot_recoding.yml")

# b
cols <- c("grey", "red")
vmap(v, col = cols[RECODE$B], border = NA)
legend("bottomright", legend = c(levels(RECODE$B)), fill = c(cols), cex = .7)
title(main = "<b> of 'Brot'")

# r
cols <- c("red", "grey")
vmap(v, col = cols[RECODE$R], border = NA)
legend("bottomright", legend = c(levels(RECODE$R)), fill = c(cols), cex = .7)
title(main = "<r> of 'Brot'")

# o
cols <- c("white", "green", "yellow", "blue", "red", "grey", "darkblue", "orange", "darkgreen")
vmap(v, col = cols[RECODE$O], border = NA)
legend("bottomright", legend = c(levels(RECODE$O)), fill = c(cols), cex = .7)
title(main = "<o> of 'Brot'")

# k
cols <- c("grey", "red")
vmap(v, col = cols[RECODE$K], border = NA)
legend("bottomright", legend = c(levels(RECODE$K)), fill = c(cols), cex = .7)
title(main = "<k> of 'Bro(k)t'")

# t
cols <- c("blue","grey", "red")
vmap(v, col = cols[RECODE$T], border = NA)
legend("bottomright", legend = c(levels(RECODE$T)), fill = c(cols), cex = .7)
title(main = "<t> of 'Brot'")

# show Session Info
sessionInfo()


