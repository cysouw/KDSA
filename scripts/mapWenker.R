 # make map of alignment of Wenker data

require(sf)
require(sp)
require(leaflet)
require(qlcVisualize)
require(RColorBrewer)

# load this
# source("mapWenker.R")

# load basemap: gives object 'tiles'
# load("data/KDSAvoronoiSP.Rdata")

# Example usage
# MAP <- mapWenker(getAlign(file, column), vowel = F, center = "t", title = "Wurst")

# save map as widget
# htmlwidgets::saveWidget(MAP, file = "title.html")

mapFromIndex <- function(nr, savedir = NULL) {
	index <- read.delim("alignments/index.txt")
	file <- paste0("alignments/", index$LEMMA[nr], ".txt")
	align <- getAlign(file, index$COGID[nr], index$COLUMN[nr])
	mapWenker(align
			, vowel = index$VOWEL[nr]
			, center = index$CENTER[nr]
			, title = index$TITLE[nr]
			, savedir = savedir
	)	
}

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

mapWenker <- function(align, polygons = tiles, vowel = TRUE, center = NULL, title = "Wenker", savedir = NULL) {

	align <- as.character(align)
	align[align == "NA"] <- NA

	if (vowel) {

		# decompose vowel
		vowelDecom <- vowelAnalysis(align)
		cols <- qlcVisualize::heeringa(dist(vowelDecom), center = center)
		names(cols) <- rownames(vowelDecom)

		allCols <- cols[align]

		# Spellings for legend
		freq <- table(align)
		selLegend <- names(sort(freq, decreasing = TRUE)[1:min(length(cols),15)])
		# selLegend <- selLegend[order(cmdscale(dist(vowelDecom))[selLegend,1])]
		colLegend <- cols[selLegend]
		selLegend <- paste0(selLegend, " (", freq[selLegend], ")")
		textLegend <- paste0("<h3>", title, " ('", center, "')</h3><br>Frequent spellings")

	} else {

		# deal with consonants
		freq <- sort(table(align), decreasing = TRUE)
		consonants <- names(freq)
		consonants <- consonants[consonants != center]
		consonants <- c(center, consonants)
		freq <- freq[consonants]

		cols <- c("grey", brewer.pal(12, "Set3"), rep("#D3D3D3",times=20))
		cols <- cols[1:length(consonants)]
		names(cols) <- consonants
		
		allCols <- cols[align]

		colLegend <- cols
		selLegend <- paste0(consonants, " (", freq, ")")
		textLegend <- paste0("<h3>", title, " ('", center, "')</h3><br>Spellings")
	}

	# treat missing data
	allCols[is.na(allCols)] <- "#A9A9A9"
	align[is.na(align)] <- "No data"

	colLegend <- c(colLegend, "#A9A9A9")
	selLegend <- c(selLegend, paste0("no data (", sum(align == "No data"), ")"))

	# adding data to map, turn into sf-format
	data <- sp::SpatialPolygonsDataFrame(polygons,
				data.frame(spelling = align, color = allCols))
	map <- sf::st_as_sf(data)
	sf::st_crs(map) <- 4326

	# Leaflet widget
	widget <- leaflet::leaflet (map) %>%

	leaflet::addPolygons (
		stroke = F
		, fillColor = ~color
		, popup = ~spelling
		, fillOpacity = 0.9
		, smoothFactor = 0.1
	) %>%
	leaflet::addLegend (
		"bottomright"
		, title = textLegend
		, labels = selLegend
		, colors = colLegend
		, opacity = 0.9
	)

	if (is.null(savedir)) {
		return(widget)
	} else {
		dir.create(savedir, showWarnings = F)
		filename <- paste0(center, "_",title)
		filepath <- file.path(savedir, filename)
		htmlwidgets::saveWidget(widget
							, file = paste0(filepath,".html")
							, title = filename
							)
		unlink(paste0(filepath,"_files"), recursive = T)
	}
}

vowelAnalysis <- function(align) {

	l <- c("a","ä","e","i","j","y","o","ö","r","l","u","w","ü","-")
	h <- c( 1 , 2 , 3 , 4 , 5 , 5 , 3 , 3 ,2.5,2.5, 4 , 5 , 4 ,2.5)
	b <- c( 5 , 3 , 2 , 1 , 1 , 1 , 6 , 4 , 5 , 5 , 6 , 6 , 3 ,3.5)

	getNums <- function(letters, number) {

		letter <- substr(letters, number, number)
		if (letter == "h") {
			letter <- substr(letters,number-1,number-1)
		}
		c(h[l == letter], b[l == letter])
	}

	trans <- function(letters) {

		res <- matrix(nrow = 2, ncol = 5)

		if (nchar(letters) == 1) {
			res[,1:5] <- getNums(letters,1)
		} else if (nchar(letters) == 2) {
			res[,1] <- getNums(letters,1)
			res[,5] <- getNums(letters,2)
			res[,2:4] <- apply(res[,c(1,5)],1,mean)
		} else if (nchar(letters) == 3) {
			res[,1] <- getNums(letters,1)
			res[,3] <- getNums(letters,2)
			res[,5] <- getNums(letters,3)
			res[,2] <- apply(res[,c(1,3)],1,mean)
			res[,4] <- apply(res[,c(3,5)],1,mean)
		} else if (nchar(letters) == 4) {
			res[,1] <- getNums(letters,1)
			res[,2] <- getNums(letters,2)
			res[,4] <- getNums(letters,3)
			res[,5] <- getNums(letters,4)
			res[,3] <- apply(res[,c(2,4)],1,mean)
		} else if (nchar(letters) == 5) {
			res[,1] <- getNums(letters,1)
			res[,2] <- getNums(letters,2)
			res[,3] <- getNums(letters,3)
			res[,4] <- getNums(letters,4)
			res[,5] <- getNums(letters,5)
		}

		if (letters == "-") {
		  len <- c(0,0)
		} else {
		  len <- rep(nchar(letters),times=2)
		}

		c(res[1,], res[2,], len)
	}

	vowels <- sort(unique(align))
	return( t(sapply(vowels, trans)) )
}

allAlign <- function(dir, kind = "V", cutoff = 5000) {
  index <- read.delim(file.path(dir, "index.txt"))

  if (kind == "V") {
    index <- index[index$VOWEL,]
  } else {
    index <- index[!index$VOWEL,]
  }

  readOne <- function(nr) {
    file <- file.path(dir, paste0(index$LEMMA[nr],".txt"))
    getAlign(file, index$COGID[nr], index$COLUMN[nr])
  }

  result <- sapply(1:nrow(index), readOne)
  sounds <- sapply(1:nrow(index), function(nr) {
    paste(index$TITLE[nr], index$CENTER[nr], sep = ":")
  })
  colnames(result) <- sounds
  rownames(result) <- NULL

  freq <- apply(result, 2, function(x) {sum(is.na(x))})
  center = index$CENTER
  sel <- c(freq < cutoff) & c(center != "-")
  
  center <- center[sel]
  result <- result[, sel]

  return(list(align = result, center = center))
}
