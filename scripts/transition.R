# Methods for transition rates

# smaller quadrants

getQuadrants <- function(scaling, minsize) {
	roundLoc <- round(loc[,2:3]/scaling)
	quadrants <- table(roundLoc[,1],roundLoc[,2])
	lats <- as.numeric(colnames(quadrants))
	longs <- as.numeric(rownames(quadrants))
	samples <- which(table(roundLoc[,1], roundLoc[,2]) > minsize, arr.ind = T)
	samples[,1] <- longs[samples[,1]]
	samples[,2] <- lats[samples[,2]]
	return(list (roundLoc, samples))
}

tmp <- getQuadrants(2,10)
roundLoc <- tmp[[1]]
samples <- tmp[[2]]
rm(tmp)

# get sample of neighbouring pairs

getPartner <- function(center, dist = 1) {
	options <- which(d[center,] == dist)
	if (length(options) == 0) {
		NA
	} else if (length(options) == 1) {
		as.vector(options)
	} else {
		as.vector(sample(options, 1))
	}
}

getSample <- function(quad, dist = 1, loc = roundLoc) {
	set <- which(loc[,1] == quad[1] & loc[,2] == quad[2])
	partners <- sapply(set, getPartner, dist = dist)
	return(list(set, partners))
}

# get counts from alignment relative to a central character

getCounts <- function(quad, align, char, dist = 1, loc = roundLoc) {
	sample <- getSample(quad, dist, loc)
	s1 <- align[sample[[1]]]
	s2 <- align[sample[[2]]]
	notNA <- !(is.na(s1)|is.na(s2))
	s1 <- s1[notNA]
	s2 <- s2[notNA]
	len <- sum(notNA)
	diffs <- 1 - sum(s1 == s2)/len
	chars <- (sum(s1 == char) + sum(s2 == char))/(2*len)
	return(c(chars,diffs))		
}

# get transition statistics

getStats <- function(distance, alignment, character, s = samples) {

	tmp <- apply(s, 1, getCounts, align=alignment, char=character, dist=distance)
	
	cor <- as.numeric(cor.test(tmp[1,],tmp[2,])$estimate)
	sig <- cor.test(tmp[1,],tmp[2,])$p.value
		
	coefs <- summary(lm(tmp[2,]~tmp[1,]))$coefficients/2
	b0 <- rnorm(1000,mean=coefs[1,1],sd=coefs[1,2])
	b1 <- rnorm(1000,mean=coefs[2,1],sd=coefs[2,2])
	
	quadratic <- function(b0, b1) {
		suppressWarnings( (1-sqrt((1-b1)^2-4*b0))/2 )
	}
	
	q <- quadratic(b0,b1)
	pAB <- q+b1/2
	pBA <- q-b1/2
	
	pABmean <- mean(pAB, na.rm=T)
	pBAmean <- mean(pBA, na.rm=T)
	pABsd <- sd(pAB, na.rm=T)
	pBAsd <- sd(pBA, na.rm=T)
	
	rates <- function(x,y,d) { 
		(-x/(x+y))*log(1-x-y)/d 
	}
	
	qAB <- rates(pAB,pBA,distance)
	qBA <- rates(pBA,pAB,distance)
	
	qABmean <- mean(qAB, na.rm=T)
	qBAmean <- mean(qBA, na.rm=T)
	qABsd <- sd(qAB, na.rm=T)
	qBAsd <- sd(qBA, na.rm=T)
	
	return(c(cor=cor, sig=sig, pAB =pABmean, pBA = pBAmean, pAB.sd = pABsd, pBA.sd = pBAsd, qAB =qABmean, qBA = qBAmean, qAB.sd = qABsd, qBA.sd = qBAsd))
}

# examples






