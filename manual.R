# some examples

# write base-files to sandbox for manual correction

source("scripts/tokenize.R")
writeFile(39)

# make map for individual sounds

source("scripts/mapWenker.R")
load("data/KDSAvoronoiSP.Rdata")

(MAP <- mapFromIndex(581))

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
examples <- examples[examples!=""]
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

limage(v[reorder$rows,reorder$cols]
       , col = h # rainbow(25)
       , col.na = NA
       , labels.x = F
       , cex.axis = .3
       , legend = 15
       , cex.legend = 0.5
       , pch.na = NULL
       , asp = (nrow(v)/ncol(v))/2
       )

# surrounding

source("scripts/mapWenker.R")
source("scripts/transition.R")
library(qlcData)

# example alignments
align <- getAlign("alignments/BROT(30).txt",1,5)
align <- recode("sandbox/t_Brot.yml",data.frame(align))[,1]

align <- getAlign("alignments/ALT_e(4).txt",1,4)
align <- recode("sandbox/t_Alt.yml",data.frame(align))[,1]

align <- getAlign("alignments/AUS(16).txt",1,3)
align <- recode("sandbox/s_Aus.yml",data.frame(align))[,1]

align <- getAlign("alignments/ÄPFEL_chen(26).txt",1,3)
align <- recode("sandbox/pf_Apfel.yml",data.frame(align))[,1]

align <- getAlign("alignments/AFF_e(11).txt",1,5)
align <- recode("sandbox/f_Affe.yml",data.frame(align))[,1]

align <- getAlign("alignments/AB_end(24).txt",1,3)
align <- recode("sandbox/b_Abend.yml",data.frame(align))[,1]

# random
f=.5
align <- rep("d",times = 5892)
align[sample(5892,5892*f)] <- "t"

# compare partners
getPairs(align)
getRates(align,"b","p")
getAll(align)->q

# dynamics
t <- 10
forward <- as.vector( table(align) %*% expm(t*tmp$Q) )
backward <- as.vector( expm(t*tmp$Q) %*% table(align) )

# stable
m <- matrix(0,nrow(tmp$Q),ncol(tmp$Q))
m[,1] <- 1
stable <- solve(t(tmp$Q+m),m[1,])


