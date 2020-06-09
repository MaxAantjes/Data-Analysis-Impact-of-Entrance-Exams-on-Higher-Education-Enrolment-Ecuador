## This module generates a list of rural and urban postal codes.  


## Load packages and data.Download list of postal codes and
## ural/ urban divide in Ecuador. 

library(pacman)
pacman::p_load(pdftools)
pacman::p_load(stringr)

td = tempdir()
temp = tempfile(tmpdir=td, fileext=".pdf")
download.file("https://aplicaciones2.ecuadorencifras.gob.ec/SIN/descargas/cge2019.pdf",temp, mode = "wb")
pdf0 <- pdf_text(temp)
unlink(temp)

## Load useful function. 

change_name <- function(x, original, new) {
        
        for(i in 1:length(original)) {
                
                x <- gsub(original[i], new[i], x)
                
        }
        
        return(x)
}


## Create a list containing rural postal codes.

pdf1 <- tolower(paste(pdf0, collapse =""))

original <- c(" ", "rural", "cantón", "zonasenestudio", "zonam")
new <- c("", "splitsplitstartrural", "endruralsplitsplit",
         "endruralsplitsplit", "endruralsplitsplit")
pdf1 <- change_name(pdf1, original,new)

pdf1 <- str_split(pdf1, "splitsplit")
rural.list <- unlist(lapply(pdf1, grep, pattern = 
                              "startrural(.*)endrural"))

rural.text <- pdf1[[1]][rural.list]


## Create a list containing urban postal codes
urban.text <- pdf1[[1]][-rural.list]


## Extract rural and urban postal codes from list.
rural.codes <- as.integer(unlist(str_extract_all(rural.text, 
                               "[0-9][0-9][0-9][0-9][0-9][0-9]")))
urban.codes <- as.integer(unlist(str_extract_all(urban.text, 
                                "[0-9][0-9][0-9][0-9][0-9][0-9]")))

saveRDS(rural.codes, file = "rural_codes.csv")
saveRDS(urban.codes, file = "urban_codes.csv")
rm(list =ls())






