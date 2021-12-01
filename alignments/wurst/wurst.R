#' ---
#' title: "Test to use KDSA"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("wurst.R")

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
wurst <- words$X169

# clustering
s <- sim.strings(wurst)
cl <- cutree(hclust(as.dist(1-s)), h = .99)

# correction of cognacy
cl[cl==2] <- 1
cl[cl==3] <- 1

cl[wurst == "maasbaanj"] <- 2
cl[wurst == "tjOlbasse"] <- 3

cl[wurst == "isdebon"] <- 9
cl[wurst == "isderbian"] <- 9
cl[wurst == "issebine"] <- 9
cl[wurst == "istarbinjs"] <- 9
cl[wurst == "isterbian"] <- 9
cl[wurst == "istorbinjs"] <- 9

cl[wurst == "saucisse"] <- 6
cl[wurst == "saucissen"] <- 6
cl[wurst == "sees"] <- 6

cl[wurst == "roglenge"] <- 13

cl[wurst == "fleisch"] <- 14

# tokenization
tok <- tokenize(wurst, profile = "wurst_graphemes.txt")

# alignments to be edited manually
out <- cbind(WORD = wurst, COGID = cl, ALIGN = tok$strings$tokenized)
write.table(out, file = "wurst_toBeAligned.txt", col.names = NA, quote = FALSE, sep = "\t")

# load manually corrected data
wurst <- read.table("wurst_aligned.txt", header = TRUE, sep = "\t")

#' ### Maps

# plot cognacy
vmap(v, col = rainbow(max(wurst$COGID))[wurst$COGID], border = NA)
title(main = "Cognates of 'Wurst'")

# sounds in "wurst"
ALIGN <- as.character(wurst$ALIGN)
ALIGN[wurst$COGID != 1] <- NA
ALIGN <- sapply(ALIGN,strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# recoding of different symbols
# write recoding template to be edited manually
write.recoding(ALIGN, file = "wurst_recoding_template.yml")
# recode data according to edited recoding template
RECODE <- recode(ALIGN, "wurst_recoding.yml")

# w
cols <- c("red", "grey")
vmap(v, col = cols[RECODE$W], border = NA)
legend("bottomright", legend = c(levels(RECODE$W)), fill = c(cols), cex = .7)
title(main = "<w> of 'Wurst'")

# u
cols <- c("white","red", "yellow","green", "darkgreen", "orange", "blue", "grey","darkblue")
vmap(v, col = cols[RECODE$U], border = NA)
legend("bottomright", legend = c(levels(RECODE$U)), fill = c(cols), cex = .7)
title(main = "<u> of 'Wurst'")

# r
cols <- c("yellow", "red", "grey", "green")
vmap(v, col = cols[RECODE$R], border = NA)
legend("bottomright", legend = c(levels(RECODE$R)), fill = c(cols), cex = .7)
title(main = "<r> of 'Wurst'")

# s
cols <- c("green", "red", "blue", "grey")
vmap(v, col = cols[RECODE$S], border = NA)
legend("bottomright", legend = c(levels(RECODE$S)), fill = c(cols), cex = .7)
title(main = "<s> of 'Wurst'")

# t
cols <- c("red", "blue", "grey")
vmap(v, col = cols[RECODE$T], border = NA)
legend("bottomright", legend = c(levels(RECODE$T)), fill = c(cols), cex = .7)
title(main = "<t> of 'Wurst'")

# show Session Info
sessionInfo()


