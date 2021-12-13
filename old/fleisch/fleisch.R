
source("../../scripts/mapWenker.R")

# load manually corrected data
word <- read.table("fleisch_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# alignments
ALIGN <- as.character(word$ALIGN)
ALIGN[word$COGID != 1] <- NA
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# maps

FL <- mapWenker(ALIGN$V1, vowel = F, center = "f", title = "Fleisch")
EI <- mapWenker(ALIGN$V3, vowel = T, center = "ei", title = "Fleisch")
SCH <- mapWenker(ALIGN$V5, vowel = F, center = "sch", title = "Fleisch")

htmlwidgets::saveWidget(FL, file = "Fleisch_F.html")
htmlwidgets::saveWidget(EI, file = "Fleisch_EI.html")
htmlwidgets::saveWidget(SCH, file = "Fleisch_SCH.html")
