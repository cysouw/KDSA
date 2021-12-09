
source("../../scripts/mapWenker.R")

# load manually corrected data
word <- read.table("wurst_aligned.txt", header = TRUE, sep = "\t")

# alignments
ALIGN <- as.character(word$ALIGN)
ALIGN[word$COGID != 1] <- NA
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# maps

W <- mapWenker(ALIGN$V1, vowel = F, center = "w", title = "Wurst")
UR <- mapWenker(ALIGN$V2, vowel = T, center = "ur", title = "Wurst")
S <- mapWenker(ALIGN$V3, vowel = F, center = "s", title = "Wurst")
ST <- mapWenker(ALIGN$V4, vowel = F, center = "t", title = "Wurst")

htmlwidgets::saveWidget(W, file = "Wurst_W.html")
htmlwidgets::saveWidget(UR, file = "Wurst_UR.html")
htmlwidgets::saveWidget(S, file = "Wurst_S.html")
htmlwidgets::saveWidget(ST, file = "Wurst_T.html")