
source("../../scripts/mapWenker.R")

# load manually corrected data
word <- read.table("apfel_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# alignments
ALIGN <- as.character(word$ALIGN)
ALIGN[word$COGID != 1] <- NA
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# maps

A <- mapWenker(ALIGN$V2, vowel = T, center = "ä", title = "Äpfelchen")
PF <- mapWenker(ALIGN$V4, vowel = F, center = "pf", title = "Äpfelchen")
E <- mapWenker(ALIGN$V5, vowel = T, center = "e", title = "Äpfelchen")
R <- mapWenker(ALIGN$V6, vowel = F, center = "-", title = "Äpfelchen")
L <- mapWenker(ALIGN$V7, vowel = F, center = "l", title = "Äpfelchen")


htmlwidgets::saveWidget(A, file = "Apfel_A.html")
htmlwidgets::saveWidget(PF, file = "Apfel_PF.html")
htmlwidgets::saveWidget(E, file = "Apfel_E.html")
htmlwidgets::saveWidget(R, file = "Apfel_R.html")
htmlwidgets::saveWidget(L, file = "Apfel_L.html")
