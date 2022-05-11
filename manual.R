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
library(qlcData)

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

# random
f=.3
align <- rep("d",times = 5892)
align[sample(5892,5892*f)] <- "t"

loc <- read.delim("data/KDSAlocations.txt")
d <- as.matrix(dist(loc[,2:3]))
getclose <- function(center,size=10) {
	align[order(d[center,], decreasing = F)[2:size]]	
}

close <- t(sapply(1:nrow(loc),getclose,size=10))
td <- apply(close,1,function(x){sum(x=="t",na.rm=T)})
(tmp <- table(td,align))

plot(log(tmp[,2]),log(tmp[,3]),type="b",xlim=c(0,8), ylim=c(0,8))
text(log(tmp[,2]),log(tmp[,3]),labels=rownames(tmp), col="red", pos=1)

e=0
plot(log(tmp[,1]+tmp[,4]+e),log(tmp[,2]+tmp[,5]+e),type="b",xlim=c(0,8), ylim=c(0,8))
text(log(tmp[,1]+tmp[,4]+e),log(tmp[,2]+tmp[,5]+e),labels=rownames(tmp), col="red", pos=1)

# maslova's approach

d2 <- round(d*5)

getPartner <- function(center, dist) {
	options <- which(d2[center,] == dist)
	if (length(options) == 0) {
		NA
	} else {
		as.vector(sample(options, 1))
	}
}

getSample <- function(size, dist, bias=F, character=NULL) {
	if(is.numeric(bias)) {
		seta <- sample(which(align==character), round(size*bias))
		setb <- sample(which(align!=character), round(size*(1-bias)))
		set <- c(seta,setb)
	} else {
	 	set <- sample(5892, size)
	}
	partners <- sapply(set, getPartner, dist = dist)
	return(list(set,partners))
}

getCounts <- function(bias = F, size, dist, character) {
	sample <- getSample(size, dist, bias, character)
	s1 <- align[sample[[1]]]
	s2 <- align[sample[[2]]]
	notNA <- !(is.na(s1)|is.na(s2))
	s1 <- s1[notNA]
	s2 <- s2[notNA]
	diffs <- sum(notNA) - sum(s1 == s2)
	chars <- sum(s1 == character) + sum(s2 == character)
	return(c(chars,diffs))
}
	
getStats <- function(dist, n, size, character) {
	
	tmp <- sapply(rep(c(.4,.5,.6),length.out=n), getCounts, size = size, dist = dist, character = character)

	cor <- as.numeric(cor.test(tmp[1,],tmp[2,])$estimate)
	sig <- cor.test(tmp[1,],tmp[2,])$p.value
		
	coefs <- summary(lm(tmp[2,]~tmp[1,]))$coefficients
	b0 <- rnorm(n,mean=coefs[1,1],sd=coefs[1,2])/(2*size)
	b1 <- rnorm(n,mean=coefs[2,1],sd=coefs[2,2])/(2*2*size)
	
	pAB <- ((1+b1)-sqrt((1+b1)^2-4*(b0+b1)))/2
	pBA <- ((1-b1)-sqrt((1-b1)^2-4*b0))/2

	pABmean <- mean(pAB, na.rm=T)
	pBAmean <- mean(pBA, na.rm=T)
	pABsd <- sd(pAB, na.rm=T)
	pBAsd <- sd(pBA, na.rm=T)
	
	rates <- function(x,y,d) { (-x/(x+y))*log(1-x-y)/d }
	
	qAB <- rates(pAB,pBA,dist)
	qBA <- rates(pBA,pAB,dist)
	
	qABmean <- mean(qAB, na.rm=T)
	qBAmean <- mean(qBA, na.rm=T)
	qABsd <- sd(qAB, na.rm=T)
	qBAsd <- sd(qBA, na.rm=T)
	
	return(c(cor=cor, sig=sig, pAB =pABmean, pBA = pBAmean, pAB.sd = pABsd, pBA.sd = pBAsd, qAB =qABmean, qBA = qBAmean, qAB.sd = qABsd, qBA.sd = qBAsd))
}
	
stats <- t(sapply(1:10,getStats,n=100,size=20,character="t"))
