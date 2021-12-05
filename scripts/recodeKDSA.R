#' ---
#' title: "Extract KDSA data"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# Coordinates of villages

LOC <- read.fwf("sources/KD_MILI.DAT"
				, width = c(7,14,4,6,4,27,12,6)
				, strip.white = TRUE
				)
				
COOR <- data.frame(LONG = LOC$V4+(LOC$V5/60), LAT = LOC$V2+(LOC$V3/60))
rownames(COOR) <- gsub(" ","",LOC$V1)

write.table( format(COOR, digits = 5, nsmall = 4)
			 , file = "sandbox/KDSAlocations.txt"
			 , sep = "\t"
			 , quote = F
			 , col.names = NA
			 )

# Data, lines correspond to coordinates

library(readr)

block <- c(3, 3, 3, 4, 4, 4, 15, 15, 10, 10, 10)
widths <- c(3, 3, rep(block, times = 174))
types <- paste0("ii", paste0(rep("iiiiiiccccc", times = 174), collapse = ""))

DATA <- read_fwf("sources/KD_vekt_1995-11-26.dat"
				, fwf_widths( widths )
				, col_types = types
				)

# select transcribed words
words <- seq(from = 9, by = 11, length.out = 174)

# prepare output
WORDS <- as.matrix(DATA[,words])
rownames(WORDS) <- rownames(COOR)
colnames(WORDS) <- 1:174

# changes in data

# missing data
WORDS[WORDS == "OHNE BELEG"] <- NA

# umlaut
WORDS <- gsub("A", "ä", WORDS)
WORDS <- gsub("O", "ö", WORDS)
WORDS <- gsub("U", "ü", WORDS)

# between brackets removed
WORDS <- gsub("\\(.+\\)", "", WORDS)

# digits "2" removed
WORDS <- gsub(" 2", "", WORDS)

# dashes removed
WORDS <- gsub("-", "", WORDS)

# apostrophes removed
WORDS <- gsub("'", "", WORDS)

# errors
WORDS[WORDS == "zum Essen"] <- "essen"
WORDS[WORDS == "lei."] <- "lei"
WORDS[WORDS == "troTe"] <- "tröte"

# check characters
# write orthography profile
library(qlcData)

w <- WORDS
dim(w) <- NULL
w <- na.omit(w)
write.profile(w)
write.profile(w, file = "sandbox/draftprofile.txt", editing = TRUE)

# after manual correction of profile, tokenize data
tok <- tokenize(w, profile = "data/profile.txt", regex = T, file = "sandbox/tokenized")
write.profile(tok$strings$tokenized, sep = " ", file = "sandbox/all_graphemes.txt")

# write output
write.table(WORDS
			, file = "sandbox/KDSAwords.txt"
			, sep="\t"
			, quote = FALSE
			, col.names = NA
			)

# collect only frequent words per columns
frequent <- apply(WORDS, 2, function(x){
				names(sort(table(x),decreasing = T)[1:5])
				})

write.table( t(frequent)
			, quote = FALSE
			, sep = "\t"
			, col.names = FALSE
			, file = "sandbox/frequentwords.txt"
			)

# show Session Info
sessionInfo()



