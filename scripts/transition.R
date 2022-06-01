# Methods for transition rates

loc <- read.delim("data/KDSAlocations.txt")
d <- as.matrix(dist(loc[,2:3]))
d <- round(d*8)

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

getPairs <- function(align) {
	partners <- table(align, align[sapply(1:5892,getPartner, dist=1)])
	partners <- round(partners/sum(partners),4)*100
	partners + t(partners) - diag(diag(partners))
}

getRates <- function(align, from, to) {
	compare <- c(from, to)
	align[!(align %in% compare)] <- NA
	stats1 <- t(sapply(rep(1,5), getStats, alignment = align, character = compare[1]))
	stats2 <- t(sapply(rep(1,5), getStats, alignment = align, character = compare[2]))
	stats2 <- stats2[,c(1,2,4,3,6,5,8,7,10,9)]
	stats <- rbind(stats1,stats2)[,3:10]
	round(apply(stats,2,mean),4)
}

getAll <- function(align) {
	pairs <- getPairs(align)
	names(dimnames(pairs))[1] <- "% pairs"
	Q <- pairs
	names(dimnames(Q))[1] <- "Q"
	diag(Q) <- 0
	
	toofew <- diag(pairs) < 0.3
	Q[,toofew] <- 0
	Q[toofew,] <- 0
	
	sd <- Q
	names(dimnames(sd))[1] <- "sd"
	diag(sd) <- ""
	
	all <- c()
	chars <- unlist(dimnames(pairs)[1])
	for (i in 2:length(chars)) {
		for (j in 1:(i-1)) {
			if (Q[i,j] > 0.3) {
				stats <- getRates(align,chars[i],chars[j])
				if (sum(stats<0) == 0) {
					Q[i,j] <- stats["qAB"]
					Q[j,i] <- stats["qBA"]
					sd[i,j] <- stats["qAB.sd"]
					sd[j,i] <- stats["qBA.sd"]
					all <- rbind(all, stats)
					rownames(all)[nrow(all)] <- paste0(chars[i], " -> ", chars[j])
				} else {
					Q[i,j] <- NA
					Q[j,i] <- NA
					sd[i,j] <- "---"
					sd[j,i] <- "---"
				}
			} else if (pairs[i,j] > 0.5) {
				Q[i,j] <- NA
				Q[j,i] <- NA
				sd[i,j] <- "---"
				sd[j,i] <- "---"
			} else {
				Q[i,j] <- 0
				Q[j,i] <- 0
				sd[i,j] <- "---"
				sd[j,i] <- "---"
			}
		}	
	}
	diag(Q) <- -rowSums(Q, na.rm=T)	
	return(list(Q=Q,sd=sd,pairs=pairs,stats=all))
}

# Surrounding

getClose <- function(center, size) {
	order(d[center,], decreasing = F)[2:(size+1)]
}

getPressureSimple <- function(align, from, to, surround) {
	compare <- c(from, to)
	align[!(align %in% compare)] <- NA
	
	close <- t(sapply(1:length(align), getClose, size = surround))
	dFrom <- apply(close, 1, function(x) {sum(align[x] == from, na.rm = T)} )
	dTo <- apply(close, 1, function(x) {sum(align[x] == to, na.rm = T)} )

	pressure <- 0.5 + (dFrom - dTo)/(2*surround)
	pFrom <- as.vector(na.omit(pressure[align == from]))
	pTo <- as.vector(na.omit(pressure[align == to]))
	
	return (list(pFrom = pFrom, pTo = pTo, dFrom, dTo))
}

getContact <- function(align, from, to, qFrom, qTo, surround = 10) {
	
	pressure <- getPressureSimple(align, from, to, surround)
	
	extremes <- seq(.2,.5,length.out=10)
	estimates <- c()
	
	for (extreme in extremes) {
	
	cFrom0 <- sum( pressure[["pFrom"]] < extreme )
	cFrom1 <- sum( pressure[["pFrom"]] > 1 - extreme )
	cTo0 <- sum( pressure[["pTo"]] < extreme )
	cTo1 <- sum( pressure[["pTo"]] > 1 - extreme )
	
	f0 <- cTo0/cFrom0
	f1 <- cTo1/cFrom1
	
	pTo <- qFrom/f1 - qTo
	pFrom <- qTo*f0 - qFrom
	
	estimates <- rbind(estimates, c(pFrom, pTo))
	}
	
	onlyFinite <- is.finite(estimates[,1]) & is.finite(estimates[,2])
	estimates <- estimates[onlyFinite,]
	extremes <- extremes[onlyFinite]
	
	iFrom <- as.numeric(lm(estimates[,1]~extremes)$coefficients)
	iTo <- as.numeric(lm(estimates[,2]~extremes)$coefficients)
	
	if (iFrom[2]>0 | iTo[2]>0) {
		warning("Estimate does not show the expected direction.")
	}
		
	return(c(cFrom = iFrom[1], cTo = iTo[1] ))
	
}

# ======================

test <- function(align, from, to, qFrom, qTo, divisions, pos) {
	
	pressure <- getPressureSimple(align, from, to, 10)
	
	bins <- seq(0, 1, length.out = divisions+1)
	cFrom <- hist(pressure[["pFrom"]], breaks = bins, plot = F)$counts
	cTo <- hist(pressure[["pTo"]], breaks = bins, plot = F)$counts

	fn1n <- cTo[divisions+1-pos]/cFrom[divisions+1-pos]
	f1n <- cTo[pos]/cFrom[pos]
	
	pTo <- n*( qFrom*(n-2) - qTo*(f1n*(n-1) - fn1n) ) / (f1n*(n-1)^2 - fn1n)
	pFrom <- n*f1n*qTo - n*qFrom + (n-1)*f1n*pTo

	return(c(pFrom, pTo))
}

getRegr <- function(align, from, to, qFrom, qTo, surround = 10, breaks = 10) {
	
	pressure <- getPressureSimple(align, from, to, surround)
	
	bins <- seq(0, 1, length.out = breaks+1)	
	cFrom <- hist(pressure[["pFrom"]], breaks = bins, plot = F)$counts
	cTo <- hist(pressure[["pTo"]], breaks = bins, plot = F)$counts

	x <- seq(0.1, 0.9, length.out = breaks)
	f <- cTo/cFrom
	a <- f*qTo - qFrom
	r <- cbind(pFrom = (1-x), pTo = -f*(x) )
		
	lm(a ~ 0 + r)
}

getBeta <- function(pressure, epsilon) {
	
	mu <- ( mean(pressure) + epsilon ) / (1 + 2*epsilon)
	sigma <- sd(pressure) / (1 + 2*epsilon)
	alpha <- mu*((mu*(1-mu))/(sigma^2) - 1)
	beta <- (1-mu)*((mu*(1-mu))/(sigma^2) - 1)
	
	return(c(alpha = alpha, beta = beta))
}

getContactStats <- function(align, from, to, surround = 10, epsilon = 0.01) {

	pressure <- getPressureSimple(align, from, to, surround)
	abFrom <- getBeta(pressure[["pFrom"]], epsilon)
	abTo <- getBeta(pressure[["pTo"]], epsilon)
	
	alpha <- abFrom["alpha"] - abTo["alpha"]
	beta <- abFrom["beta"] - abTo["beta"]
	
	f0 <- as.numeric( (alpha)/(epsilon) - (beta)/(1+epsilon) )
	f1 <- as.numeric( (alpha)/(1+epsilon) - (beta)/(epsilon) )

	return(c(f0 = f0, f1 = f1, alpha, beta))
}


# library(fitdistrplus)
# library(EnvStats)

fitNT <- function(distr, method = "mge") {
	
	if (mean(distr) > 0.5) {
		fix <- list(max = 1)
	} else {
		fix <- list(min = 0)
	}
	
	suppressWarnings(
	fitdist(distr
			, "normTrunc"
			, method = method
			, fix.arg = fix
			, start = list( mean = mean(distr), sd = sd(distr) )
			)
	)
}





