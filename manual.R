# some examples

# write base-files to sandbox for manual correction

source("scripts/tokenize.R")
writeFile(39)

# make map for individual sounds

source("scripts/mapWenker.R")
load("data/KDSAvoronoiSP.Rdata")

(MAP <- mapFromIndex(169))

# show in browser

MAP

# save map as widget

htmlwidgets::saveWidget(MAP, file = "sandbox/title.html")

# Sound comparison

source("scripts/mapWenker.R")

dir <- "alignments"
v <- allAlign(dir)
center <- v$center
v <- v$align

freq <- table(v)
numV <- vowelAnalysis(names(freq))
examples <- sample(names(freq),5000,prob=freq/sum(freq),replace = T)
data <- rbind(numV, numV[examples,])
d <- as.matrix(dist(data))
h <- heeringa(d)[1:nrow(numV)]
h <- h[order(freq, decreasing = T)]

#mds <- cmdscale(as.dist(d))
#plot(mds, pch = 20, col = heeringa(d),cex=2)
#text(mds[1:nrow(numV),],labels=names(freq), cex=.3)

v2 <- v
for (i in seq_along(center)) {
	v2[ v[,i] == center[i], i] <- NA
}

limage(v
       , order = "OLO_ward"
       , method = "hamming"
       , plot = F
       ) -> reorder

limage(v2[reorder$rows,reorder$cols]
       , col = h
       , col.na = "white"
       , labels.x = F
       , cex.axis = .5
       , legend = 20
       , cex.legend = 0.7
       , pch.na = NULL
       )




