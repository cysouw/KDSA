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
}


tok <- apply(w,2,tokens)
colnames(tok) <- l$Context
rownames(tok) <- 1:5892

write.table(tok, file = "sandbox/KDSAtokenized.txt", quote = F, sep = "\t")

t <- read.delim("data/KDSAtokenized.txt", row.names = 1)

ALIGN <- as.character(t[,2])
ALIGN <- t(sapply(ALIGN, function(x){strsplit(x," ")[[1]]}))
ALIGN[apply(ALIGN,1, function(x){sum(x=="-")==length(x)}),] <- NA
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

source("scripts/mapWenker.R")
load("data/KDSAvoronoiSP.Rdata")

A <- mapWenker(ALIGN$V4, vowel = T, center = "a", title = "Affe")