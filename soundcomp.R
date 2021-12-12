library(qlcVisualize)
source("scripts/mapWenker.R")

dir <- "alignments"
v <- allAlign(dir)

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

sel <- 1:1000
system.time(
limage(v
       , order = "OLO_ward"
       , col = h[1:50]
       , labels.x = F
       , cex.axis = .5
       , legend = 20
       , cex.legend = 0.7
       , method = "hamming"
       , pch.na = NULL
       )
)



