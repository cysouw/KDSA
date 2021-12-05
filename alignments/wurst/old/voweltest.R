#+ echo=FALSE

library(sp)
library(sf)
library(leaflet)

wurst <- read.table("wurst_aligned.txt", header = TRUE, sep = "\t")

ALIGN <- as.character(wurst$ALIGN)
ALIGN[wurst$COGID != 1] <- NA
ALIGN <- sapply(ALIGN,strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

vowel <- paste0(ALIGN$V2,ALIGN$V3)
vowel <- gsub("-","",vowel,fixed = T)
vowel[vowel=="NANA"] <- NA
vowel[vowel=="ih"] <- "ii"
vowel[vowel=="ihr"] <- "iir"
vowel[vowel=="uh"] <- "uu"
vowel[vowel=="oh"] <- "oo"
vowel[vowel=="öhr"] <- "öör"
vowel[vowel=="uhe"] <- "uue"
vowel[vowel=="uhr"] <- "uur"
vowel[vowel=="ühr"] <- "üür"
vowel[vowel=="üh"] <- "üü"
vowel[vowel=="ijr"] <- "iir"
vowel[vowel=="uj"] <- "ui"

vs <- sort(unique(vowel))

l <- c("a","ä","e","i","o","ö","r","u","ü")
h <- c(1,2,3,4,2.5,3,2,4,4)
b <- c(5,3,2,1,5.5,4,5,6,3)

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

all <- t(sapply(vs, trans))
cols <- qlcVisualize::heeringa(dist(all), center = "ur")
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
	, title = "<h3>Wurst ('ur')</h3><br>Frequent spellings:"
	, labels = top
	, colors = cols[top]
	, opacity = 0.9
)

x

# rmarkdown::render("voweltest.R")
# htmlwidgets::saveWidget(x, file = "wurstVowel.html")
