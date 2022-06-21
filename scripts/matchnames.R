# Rough attempt to match the names of the villages from the printed atlas to the names of the original `Wenkerbögen`. This linking has been manually corrected and enhanced. The final result is documented in the file `data/KDSA_locations.txt`. The current script is only retained here for historical reasons.

ocr <- read.delim("../sources/scans\ from\ printed\ atlas/OCRnames.txt")
all <- read.delim("../sources/all\ wenker\ places/wenkerorte.txt")

ocrname <- gsub(" */.+$","",ocr[,3])
ocrname <- gsub(" *\\(.+$","",ocrname)

allname <- gsub(" */.+$","",all$name)
allname <- gsub(" *\\(.+$","",allname)

allplace <- gsub(" */.+$","",all$place)
allplace <- gsub(" *\\(.+$","",allplace)

m <- sapply(ocrname,function(x){all$bogennummern[allname==x | allplace ==x]})
l <- sapply(m,length)
m[l!=1] <- ""

write(unlist(m),ncolumns=1,file="../sandbox/match.txt")