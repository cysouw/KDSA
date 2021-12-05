#+ echo=FALSE

library(sp)
library(sf)
library(leaflet)

word <- read.table("fleisch_aligned.txt", header = TRUE, sep = "\t")

ALIGN <- as.character(word$ALIGN)
ALIGN[word$COGID != 1] <- NA
ALIGN <- sapply(ALIGN,strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

vowel <- ALIGN$V3
vowel[vowel == "-"] <- NA
vowel <- gsub("(.)h","\\1\\1",vowel)

l <- c("a","ä","e","i","j","y","o","ö","r","u","w","ü")
h <- c(1,2,3,4,4,4,2.5,3,2,4,4,4)
b <- c(5,3,2,1,1,1,5.5,4,5,6,6,3)

getNums <- function(letter) {
	c(h[l == letter], b[l == letter])	
}

trans <- function(letters) {
	
	res <- matrix(nrow = 2, ncol = 5)
	
	if (nchar(letters) == 1) {
		res[,1:5] <- getNums(letters)
	} else if (nchar(letters) == 2) {
		res[,1] <- getNums(substr(letters,1,1))
		res[,5] <- getNums(substr(letters,2,2))
		res[,2:4] <- apply(res[,c(1,5)],1,mean)
	} else if (nchar(letters) == 3) {
		res[,1] <- getNums(substr(letters,1,1))
		res[,3] <- getNums(substr(letters,2,2))
		res[,5] <- getNums(substr(letters,3,3))
		res[,2] <- apply(res[,c(1,3)],1,mean)
		res[,4] <- apply(res[,c(3,5)],1,mean)
	} else if (nchar(letters) == 4) {
		res[,1] <- getNums(substr(letters,1,1))
		res[,2] <- getNums(substr(letters,2,2))
		res[,4] <- getNums(substr(letters,3,3))
		res[,5] <- getNums(substr(letters,4,4))
		res[,3] <- apply(res[,c(2,4)],1,mean)
	} else if (nchar(letters) == 5) {
		res[,1] <- getNums(substr(letters,1,1))
		res[,2] <- getNums(substr(letters,2,2))
		res[,3] <- getNums(substr(letters,3,3))
		res[,4] <- getNums(substr(letters,4,4))
		res[,5] <- getNums(substr(letters,5,5))
	}
	
	c(res[1,],res[2,],nchar(letters))
}

vs <- sort(unique(vowel))
all <- t(sapply(vs, trans))
cols <- qlcVisualize::heeringa(dist(all), center = "ei")
names(cols) <- rownames(all)
allcols <- cols[vowel]

freq <- table(vowel)
top <- names(sort(freq, decreasing=T)[1:10])
top <- top[order(cmdscale(dist(all))[top,2])]

allcols[is.na(allcols)] <- "#FFFFFF"
vowel[is.na(vowel)] <- "No data"

# load basemap
load("../../data/KDSAvoronoiSP.Rdata")
#tmp <- as(v,"SpatialPolygons")

data <- SpatialPolygonsDataFrame(tiles, data.frame(vowel, color=allcols))
map <- st_as_sf(data)
st_crs(map) <- 4326

x <- leaflet (map) %>% 
addPolygons (
	stroke = F
	, fillColor = ~color
	, popup = ~vowel
	, fillOpacity = 0.9
	, smoothFactor = 0.5
) %>%
addLegend (
	"bottomright"
	, title = "<h3>Fleisch ('ei')</h3><br>Frequent spellings"
	, labels = top
	, colors = cols[top]
	, opacity = 0.9
)

x

# rmarkdown::render("voweltest.R")
# htmlwidgets::saveWidget(x, file = "fleischVowel.html")
