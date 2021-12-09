
source("../../scripts/mapWenker.R")

# load manually corrected data
word <- read.table("auch_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# alignments
ALIGN <- as.character(word$ALIGN)
ALIGN[word$COGID != 1] <- NA
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# maps

AU <- mapWenker(ALIGN$V2, vowel = T, center = "au", title = "Auch")
CH <- mapWenker(ALIGN$V3, vowel = F, center = "ch", title = "Auch")

htmlwidgets::saveWidget(AU, file = "Auch_AU.html")
htmlwidgets::saveWidget(CH, file = "Auch_CH.html")
