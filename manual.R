# some examples

# write base-files to sandbox for manual correction

source("scripts/tokenize.R")
writeFile(4)

# make map for individual sounds

source("scripts/mapWenker.R")
load("data/KDSAvoronoiSP.Rdata")

MAP <- mapWenker(getAlign("alignments/AB_end(24).txt", 2), vowel = T, center = "a" , title = "Abend")
MAP <- mapWenker(getAlign("alignments/AB_end(24).txt", 3), vowel = F, center = "b" , title = "Abend")

MAP <- mapWenker(getAlign("alignments/AFF_e(11).txt", 4), vowel = T, center = "a" , title = "Affe")
MAP <- mapWenker(getAlign("alignments/AFF_e(11).txt", 5), vowel = F, center = "ff" , title = "Affe")

MAP <- mapWenker(getAlign("alignments/ALL_e(38).txt", 2), vowel = T, center = "a" , title = "Alle")
MAP <- mapWenker(getAlign("alignments/ALL_e(38).txt", 3), vowel = F, center = "ll" , title = "Alle")

# show in browser

MAP

# save map as widget

htmlwidgets::saveWidget(MAP, file = "sandbox/title.html")