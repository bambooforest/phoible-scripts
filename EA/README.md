Note that this code is deprecated. We have been changing:

https://github.com/phoible/dev/blob/master/raw-data/EA/EA_IPA_correspondences.tsv

directly on phoible, and so the data in the two pipelines is no longer aligned. Precede with caution.

# Conversion pipeline to convert the database of Euroasian phonologies to PHOIBLE format

## To get the data

Data sources:

http://eurasianphonology.info/
https://github.com/macleginn/eurasian-phonologies/blob/master/src/dbase/phono_dbase.json

Get the data:

`sh 1_get_raw_data.sh` 

- Downloads and converts the data from json to csv (latest version: MD5 (phono_dbase.csv) = d36d041ac81ea2a1d3f5b27f4ee4e323):


## Get the EA segment-PHOIBLE IPA correspondenes

To map EA segments to PHOIBLE IPA conventions (this is a recursive procedure that allows us to compile segment mismatches which we then convert with rules or by hand in `data/formatted/Handmade-EA_IPA_correspondences.tsv`). The output file is written as to the to `../data/formatted/EA_IPA_correspondences.tsv` directory and knitr creates an `2a_process_segments.html` file:

`Rscript -e 'rmarkdown::render("2a_process_segments.Rmd")'`


## Create PHOIBLE inventory format

This adds Inventory IDs and removes white space in segments.

`Rscript -e 'rmarkdown::render("2b_process_inventories.Rmd")'`


## Update PHOIBLE dev repo with the files output to ../data/formatted/:

- `EA_inventories.tsv`
- `EA_IPA_correspondences.tsv`


## TODO

- finish script `/processing/3_get_refs.py` to get the BibTeX references
