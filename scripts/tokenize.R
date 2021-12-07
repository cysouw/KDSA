library(qlcData)

# read words
w <- read.delim("../data/KDSAwords.txt", row.names = 1)
# read lemmas
l <- read.delim("../data/KDSAlemmas.txt", row.names = 1)
# read locations
n <- read.delim("../data/KDSAlocations.txt",row.names = 1)

# tokenize
splitTokens <- function(lemma) {
	tokens <- tokenize(w[,lemma], profile = "../data/KDSAprofile.txt", regex = T)$strings
	colnames(tokens) <- c("strings","alignments")
	result <- cbind(locations = rownames(n), tokens, cognateID = 1)
	result$alignments[is.na(result$strings)] <- ""
	result$cognateID[is.na(result$strings)] <- NA
	result
}

# write files
writeFile <- function(lemma) {
	filename <- paste0(l$Context[lemma],"(",l$Sentence[lemma],").txt")
	write.table(splitTokens(lemma), file = file.path("..","sandbox",filename), quote = FALSE, row.names = FALSE, sep = "\t")
}
