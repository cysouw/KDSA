#' ---
#' title: "Preparing a map for KDSA"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("map.R")

# read coordinates
coor <- read.delim("data/coor.txt", row.names = 1)

# prepare map

library(qlcVisualize)

window <- hullToOwin(coor, shift = 0.1, alpha = 0.2)
v <- voronoi(coor, window)
vmap(v)

save(v, file = "sandbox/KDSAvoronoi.Rdata")

# show Session Info
sessionInfo()

