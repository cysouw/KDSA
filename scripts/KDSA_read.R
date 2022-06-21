# Some general help function to read and process data

getAlign <- function(lemma, cogid, column) {

	word <- read.table(lemma, header = TRUE, sep = "\t")

	ALIGN <- as.character(word$alignments)
	ALIGN[word$cognateID != cogid] <- NA
	ALIGN[is.na(word$cognateID)] <- NA
	ALIGN[is.na(word$strings)] <- NA
	ALIGN <- sapply(ALIGN, strsplit, split = " ")
	ALIGN <- do.call(rbind,ALIGN)

	ALIGN[,column]

}

allAlign <- function(dir, selection = NULL, cutoff = 5000) {
	
  index <- read.delim(file.path(dir, "index.txt"))

  if (!is.null(selection)) {
	  if (selection[1] == "V") {
	    index <- index[index$KIND == "V",]
	  } else if (selection[1] == "C") {
	    index <- index[index$KIND == "C",]
	  } else if (is.numeric(selection)) {
	  	index <- index[selection,]
	  } else if (is.character(selection)) {
	  	index <- index[index$CENTER %in% selection,]
	  }
  }
  
  readOne <- function(nr) {
    file <- file.path(dir, paste0(index$LEMMA[nr],".txt"))
    getAlign(file, index$COGID[nr], index$COLUMN[nr])
  }

  result <- sapply(1:nrow(index), readOne)
  sounds <- sapply(1:nrow(index), function(nr) {
    paste(index$TITLE[nr], index$CENTER[nr], sep = ":")
  })
  dupl <- which(duplicated(sounds))
  sounds[dupl] <- paste0(sounds[dupl], "2")
  colnames(result) <- sounds
  rownames(result) <- NULL

  freq <- apply(result, 2, function(x) {sum(is.na(x))})
  center = index$CENTER
  sel <- c(freq < cutoff) & c(center != "-")
  
  center <- center[sel]
  result <- result[, sel]

  return(list(align = result, center = center))
}
