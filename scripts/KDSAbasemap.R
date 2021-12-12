#' ---
#' title: "Preparing a map for KDSA"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("map.R")

# read coordinates
coor <- read.delim("../data/KDSAlocations.txt", row.names = 1)

# prepare map

library(sp)
library(qlcVisualize)

window <- hullToOwin(coor, shift = 0.1, alpha = 0.2)
v <- voronoi(coor, window)
vmap(v)

# turn into Spatial format
tiles <- as(v,"SpatialPolygons")
save(tiles, file = "../sandbox/KDSAvoronoiSP.Rdata")

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
colnames(extra) <- c("LONG","LAT")

window <- hullToOwin(rbind(coor,extra), shift = 0.1, alpha = 0.2)
v <- voronoi(rbind(coor,extra), window)[1:dim(coor)[1]]

# save

save(v, file = "../sandbox/KDSAvoronoi.Rdata")

# turn into Spatial format
tiles <- as(v,"SpatialPolygons")
save(tiles, file = "../sandbox/KDSAvoronoiSP.Rdata")


# show Session Info
sessionInfo()

