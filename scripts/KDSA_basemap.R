#' ---
#' title: "Preparing a map for KDSA"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("map.R")

# read coordinates
coor <- read.delim("../data/KDSA_locations.txt", row.names = 1)

# prepare map

library(sp)
library(qlcVisualize)

window <- hullToOwin(coor, shift = 0.1, alpha = 0.2)
v <- voronoi(coor, window)
vmap(v)

# try to separate parts

extra <- rbind( c( 7.80, 54.00),
				c(15.60, 49.20),
				c(15.50, 49.20),
				c(15.35, 49.25),
				c(15.20, 49.25),
				c(11.10, 46.15),
				c(11.20, 46.15),
				c(11.30, 46.15),
				c(16.50, 49.40),
				c(16.75, 49.40),
				c(17.00, 49.40))
colnames(extra) <- c("LONG_KDSA","LAT_KDSA")

window <- hullToOwin(rbind(coor[,1:2],extra), shift = 0.1, alpha = 0.2)
v <- voronoi(rbind(coor,extra), window)[1:dim(coor)[1]]

# save

save(v, file = "../sandbox/KDSA_voronoi.Rdata")

# turn into Spatial format
tiles <- as(v,"SpatialPolygons")
save(tiles, file = "../sandbox/KDSA_voronoiSP.Rdata")

# concaveman approach

groups <- rep(1, times = nrow(coor))

groups[c(5882, 5883, 5884, 5886, 5887, 5888, 5889, 5890, 5891, 5892)] <- 2
groups[c(5880, 5881, 5885)] <- 3
groups[c(4179, 4225, 4226, 4227, 4275, 4276, 4324, 4325, 4326, 4377, 4378, 4436, 4495, 4556, 4557, 4619, 4620)] <- 4
groups[c(4435, 4493, 4494, 4555, 4618)] <- 5
groups[c(3589, 3812, 3813, 3869, 3870, 3871, 3929, 3930)] <- 6
groups[c(4273, 4274)] <- 7
groups[409] <- 8

map <- weightedMap(coor[,1],coor[,2], grouping = groups, crs = 2397, expansion = 10000)

# show Session Info
sessionInfo()

