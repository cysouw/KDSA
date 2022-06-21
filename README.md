# KDSA Data

This repository contains the original digital data from the *Kleine Deutsche Sprachatlas* (KDSA) with a reorganisation of the data into more recent formats and some basic geographical mapping procedures. 

Please cite the data as:

- Veith, Werner H. 1984–1999. *Kleiner Deutscher Sprachatlas.* Dialektologisch bearbeitet von Werner H. Veith, computativ bearbeitet von Wolfgang Putschke und Lutz Hummel. 4 Bände. Tübingen: Max Niemeyer Verlag.

Please cite this repository as:

- Cysouw, Michael. 2022. *KDSA Data Repository.* Available online at <github.com/cysouw/KDSA>.

# Original data

The KDSA was originally conceived as a computerised mapping project in the 1970s. All the data were digitised and are still digitally available. These original data can be found in the `sources/original` directory with explanation about their structure, as prepared by Lutz Hummel. The data itself is readily readable as FWF ('fixed width format'). The script `KDSA_recode.R` converts the original data into tab-delimited tables, which are in the directory `data`. A few illustrative scans of the printed atlas are also available in the directory `sources/scans`

# Processing of the data

The original data was transliterated in the 1970s from the Wenker-survey performed in the 1890s. About 6000 of these 'Wenkerbögen' were selected for this atlas and targeted snippets from the originals were transliterated and stored electronically. Unfortunately, there was no documentation which of the 'Wenkerbögen' were selected. Based on the list of names in the printed atlas (scanned as `sources/scans/KDSA_locations.pdf`) we have been able to link the KDSA data to the original data (as documented in the file `data/KDSA_locations.txt`).

Also, the digital data did not include any indication exactly which snippets were transliterated. This is documented in the file `data/KDSA_lemmas.txt` based on the information in the printed atlas (scanned as `sources/scans/KDSA_index.pdf`). The actual snippets that were transliterated are in the file `data/KDSA_words.txt`. Note that these transliterations appear to be simplified (most diacritics from the original data have not been included) and often only parts of the complete words have transliterated.

# Alignment

Based on the raw transliterations, new alignments have been prepared, see the directory `alignments`. Each file in this directory analyses a single transliterated snippet, e.g. the snippet "ab" from the word "Abend" in the file `alignments/AB_end(24).txt`. These file repeat the original snippet and categorises them to different cognate sets (e.g. sometimes the snippet for "Abend" is actually related to the word "Nacht"). Each cognate set is then aligned uses spaces as separators and dashes for any empty column.

An index file for all alignment-columns is added as `data/KDSA_alignment_index.txt`.

# Mapping

A basic procedure to produce maps for each alignment-column is provided, see `manual.R` for usage. Note that this procedure has quite a long list of dependencies in R, so some additional installation might be necessary.
