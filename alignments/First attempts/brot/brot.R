source("../../scripts/mapWenker.R")

# load manually corrected data
word <- read.table("brot_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# sounds in "berg"
ALIGN <- as.character(word$ALIGN)
ALIGN[word$COGID != 1] <- NA
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)


# maps

B <- mapWenker(ALIGN$V1, vowel = F, center = "b", title = "Brot")
R <- mapWenker(ALIGN$V4, vowel = F, center = "r", title = "Brot")
O <- mapWenker(ALIGN$V5, vowel = T, center = "o", title = "Brot")
KT <- mapWenker(ALIGN$V7, vowel = F, center = "t", title = "Brot")

htmlwidgets::saveWidget(B, file = "Brot_B.html")
htmlwidgets::saveWidget(R, file = "Brot_R.html")
htmlwidgets::saveWidget(O, file = "Brot_O.html")
htmlwidgets::saveWidget(KT, file = "Brot_T.html")
