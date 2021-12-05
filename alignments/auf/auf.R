
source("../../scripts/mapWenker.R")

# load manually corrected data
word <- read.table("auf_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# alignments
ALIGN <- as.character(word$ALIGN)
ALIGN[word$COGID != 1] <- NA
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# maps

AU <- mapWenker(ALIGN$V1, vowel = T, center = "au", title = "Auch")
AUF <- mapWenker(ALIGN$V2, vowel = F, center = "f", title = "Auch")

htmlwidgets::saveWidget(AU, file = "Auf_AU.html")
htmlwidgets::saveWidget(AUF, file = "Auf_F.html")
