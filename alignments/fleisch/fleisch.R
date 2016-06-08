#' ---
#' title: "Test to use KDSA"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("fleisch.R")

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
# example
example <- words$X51

#' ### Cognates

levels(example)
kjod <- 144:150
meat <- 151:152

cl <- rep("fleisch", times = length(example))
sapply(kjod, function(x){cl[example == levels(example)[x]] <<- "kjod"})
sapply(meat, function(x){cl[example == levels(example)[x]] <<- "meat"})
cl <- as.factor(cl)

# plot cognacy
vmap(v, col = rainbow(nlevels(cl))[as.numeric(cl)], border = NA)
title(main = "Cognates of 'Fleisch'")

#' ### Alignment

# tokenization
tok <- tokenize(example, profile = c("[aäeioöuüy]+h?j?","f","pf","v","w","r","t","ck","k","g","[sz]+c?h?","ch","m", "kj", "d","n","l"), regex = T)

# alignments to be edited manually
out <- cbind(WORD = example, COGID = cl, ALIGN = tok$strings$tokenized)
write.table(out, file = "fleisch_toBeAligned.txt", col.names = NA, quote = FALSE, sep = "\t")

# load manually corrected data
data <- read.table("fleisch_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# sounds
ALIGN <- as.character(data$ALIGN)
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN[data$COGID != 1] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# recoding of different symbols
# write recoding template to be edited manually
write.recoding(ALIGN, file = "recoding_template.yml")
# recode data according to edited recoding template
ALIGN <- recode(ALIGN, "fleisch_recoding.yml")

#' ### Maps

cols <- c("red", "blue", "green", "purple")
vmap(v, col = cols[ALIGN[,1]], border = NA)
legend("bottomright", legend = levels(ALIGN[,1]), fill = cols, cex = .7)
title(main = "<f> of 'Fleisch'")

cols <- c("grey", "red")
vmap(v, col = cols[ALIGN[,2]], border = NA)
legend("bottomright", legend = levels(ALIGN[,2]), fill = cols, cex = .7)
title(main = "<l> of 'Fleisch'")

cols <- sample(rainbow(10))
vmap(v, col = cols[ALIGN[,3]], border = NA)
legend("bottomright", legend = levels(ALIGN[,3]), fill = cols, cex = .7)
title(main = "vowel of 'Fleisch'")

cols <- c("grey", "red", "blue", "green")
vmap(v, col = cols[ALIGN[,4]], border = NA)
legend("bottomright", legend = levels(ALIGN[,4]), fill = cols, cex = .7)
title(main = "extra consonant in 'Fleisch'")

cols <- c("grey", "red", "blue", "green")
vmap(v, col = cols[ALIGN[,5]], border = NA)
legend("bottomright", legend = levels(ALIGN[,5]), fill = cols, cex = .7)
title(main = "<sch> of 'Fleisch'")

cols <- c("grey", "red", "blue")
vmap(v, col = cols[ALIGN[,6]], border = NA)
legend("bottomright", legend = levels(ALIGN[,6]), fill = cols, cex = .7)
title(main = "<k> of 'Fleisch'")

# show Session Info
sessionInfo()


