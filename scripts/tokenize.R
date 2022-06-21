# This script was used to prepare the alignment files from the transcribed snippets. These files were manually corrected into the files in the directory `alignments`. The current script is only retained here for historical purposed.

library(qlcData)

# read words
w <- read.delim("data/KDSA_words.txt", row.names = 1)
# read lemmas
l <- read.delim("data/KDSA_lemmas.txt", row.names = 1)
# read locations
n <- read.delim("data/KDSA_locations.txt",row.names = 1)

# tokenize
splitTokens <- function(lemma) {
	tokens <- tokenize(w[,lemma], profile = "data/KDSA_profile.txt", regex = T)$strings
	result <- cbind(
				locations = rownames(n)
				, strings = tokens$originals
				, cognateID = rep(1, times = dim(w)[1])
				, alignments = tokens$tokenized
				)
	result[,'alignments'][is.na(result[,'strings'])] <- "-"
	result[,'cognateID'][is.na(result[,'strings'])] <- NA
	result
}

# write files
writeFile <- function(lemma) {
	filename <- paste0(l$Context[lemma],"(",l$Sentence[lemma],").txt")
	write.table(splitTokens(lemma), file = file.path("sandbox",filename), quote = FALSE, row.names = FALSE, sep = "\t")
}
