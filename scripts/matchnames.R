ocr <- read.delim("../sources/OCRnames.txt")
all <- read.delim("../sources/wenkerorte.txt")

ocrname <- gsub(" */.+$","",ocr[,3])
ocrname <- gsub(" *\\(.+$","",ocrname)

allname <- gsub(" */.+$","",all$name)
allname <- gsub(" *\\(.+$","",allname)

allplace <- gsub(" */.+$","",all$place)
allplace <- gsub(" *\\(.+$","",allplace)

m <- sapply(ocrname,function(x){all$bogennummern[allname==x | allplace ==x]})
l <- sapply(m,length)
m[l!=1] <- ""

write(unlist(m),ncolumns=1,file="match.txt")