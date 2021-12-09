# some examples

# write base-files to sandbox for manual correction

source("scripts/tokenize.R")
writeFile(17)

# make map for individual sounds

source("scripts/mapWenker.R")
load("data/KDSAvoronoiSP.Rdata")

mapFromIndex <- function(nr) {
	index <- read.delim("alignments/index.txt")
	file <- paste0("alignments/", index$LEMMA[nr], ".txt")
	align <- getAlign(file, index$COGID[nr], index$COLUMN[nr])
	mapWenker(align
			, vowel = index$VOWEL[nr]
			, center = index$CENTER[nr]
			, title = index$TITLE[nr]
	)	
}

MAP <- mapFromIndex(14)

# show in browser

MAP

# save map as widget

htmlwidgets::saveWidget(MAP, file = "sandbox/title.html")