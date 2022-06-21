# some examples

# write alignment base-files to sandbox for manual correction

source("scripts/tokenize.R")
writeFile(39)

# make map for individual sounds from alignments
# as a shortcut, use line number from file `data/KDSA_alignment_index.txt`

source("scripts/KDSA_map.R")
source("scripts/KDSA_read.R")
load("data/KDSA_voronoiSP.Rdata")

(MAP <- mapFromIndex(24))

# show in browser

MAP

# save map as widget
# because of a bug, you will have to manually delete javascript files

htmlwidgets::saveWidget(MAP, file = "sandbox/title.html")


# There might be an error related to pandoc.
# In that case, you will explicitly have to tell R to find pandoc
# for example like this:

# old_path <- Sys.getenv("PATH")
# path_to_pandoc <- "/opt/homebrew/bin"
# Sys.setenv(PATH = paste(old_path, path_to_pandoc, sep = ":"))
