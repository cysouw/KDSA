

require(qlcData)
source("../../scripts/mapWenker.R")

# load manually corrected data
word <- read.table("berg_aligned.txt", row.names = 1, header = TRUE, sep = "\t")

# sounds in "berg"
ALIGN <- as.character(word$ALIGN)
ALIGN[word$COGID != 1] <- NA
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# recode data according to edited recoding template
RECODE <- qlcData::recode("berg_recoding.yml", ALIGN)

# maps

B <- mapWenker(ALIGN$V2, vowel = F, center = "b", title = "Berg")
ER <- mapWenker(ALIGN$V3, vowel = T, center = "er", title = "Berg")
G <- mapWenker(ALIGN$V6, vowel = F, center = "g", title = "Berg")
I <- mapWenker(ALIGN$V4, vowel = F, center = "-", title = "Berg")
N <- mapWenker(ALIGN$V5, vowel = F, center = "-", title = "Berg")

htmlwidgets::saveWidget(B, file = "Berg_B.html")
htmlwidgets::saveWidget(ER, file = "Berg_ER.html")
htmlwidgets::saveWidget(G, file = "Berg_G.html")
