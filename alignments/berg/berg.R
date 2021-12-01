#' ---
#' title: "Test to use KDSA"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("berg.R")

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
# example "Berg"
berg_source <- words$X19

#' ### Alignment

# tokenization
tok <- tokenize(berg_source, profile = c("[aäeoöuü]+h*","i","j","b","p","ch","d","n*m*g+k*","c*k","c","t","m","s","y","w","h","l+","n","r+","v"), regex = T)

# alignments to be edited manually
out <- cbind(WORD = berg_source, COGID = 1, ALIGN = tok$strings$tokenized)
write.table(out, file = "berg_toBeAligned.txt", col.names = NA, quote = FALSE, sep = "\t")

# load manually corrected data
berg <- read.table("berg_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# identify cases
labelnr <- 184
vmap(v,col=c("grey","red")[(berg_source==levels(berg_source)[labelnr])+1], border=NA)

# sounds in "berg"
ALIGN <- as.character(berg$ALIGN)
ALIGN[berg$COGID != 1] <- NA
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# recoding of different symbols
# write recoding template to be edited manually
write.recoding(ALIGN, file = "berg_recoding_template.yml")
# recode data according to edited recoding template
RECODE <- recode(ALIGN, "berg_recoding.yml")

#' ### Maps

# b
cols <- c("grey", "red", "blue")
vmap(v, col = cols[RECODE $Berig], border = NA)
legend("bottomright", legend = levels(RECODE $Berig), fill = cols, cex = .7)
title(main = "<b> of 'Berg'")

# e
cols <- c("red", "yellow", "green", "blue", "darkgreen", "grey","purple")
vmap(v, col = cols[RECODE $bErig], border = NA)
legend("bottomright", legend = levels(RECODE $bErig), fill = cols, cex = .7)
title(main = "<e> of 'Berg'")

# r
cols <- c("red", "blue", "grey")
vmap(v, col = cols[RECODE $beRig], border = NA)
legend("bottomright", legend = levels(RECODE $beRig), fill = cols, cex = .7)
title(main = "<r> of 'Berg'")

# i
cols <- c("grey", "blue")
vmap(v, col = cols[RECODE $berIg], border = NA)
legend("bottomright", legend = levels(RECODE $berIg), fill = cols, cex = .7)
title(main = "<i> of 'Ber(i)g'")

# g
cols <- cols <- c("red", "purple", "green", "black", "grey", "blue", "yellow", "darkgreen")
vmap(v, col = cols[RECODE $beriG], border = NA)
legend("bottomright", legend = levels(RECODE $beriG), fill = cols, cex = .7)
title(main = "<g> of 'Berg'")

# n
cols <- c("grey","red")
vmap(v, col = cols[RECODE $beriNg], border = NA)
legend("bottomright", legend = levels(RECODE $beriNg), fill = cols, cex = .7)
title(main = "<n> of 'Berg(n)'")

# show Session Info
sessionInfo()


