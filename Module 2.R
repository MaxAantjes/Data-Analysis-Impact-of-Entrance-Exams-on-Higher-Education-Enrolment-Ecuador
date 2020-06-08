## This module manipulates the data to distinguish between rural 
## and urban migration and origin. 


## Load packages and data.Download list of postal codes and
## ural/ urban divide in Ecuador. 

library(pacman)
pacman::p_load(pdftools)
pacman::p_load(stringr)

dat0 <- readRDS("clean_data")
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


## Whether codes are rural or urban can be checked with the 
## following function. This can be (and has been) used to test 
## the validity of the above method. 

rurORurb <- function(code, v1 = rural.codes, v2 = urban.codes) {
        
        if (is.na(code) == TRUE) {
                
                c <- code
                
        } else {
        
        a <- sum(grepl(code, v1))
        b <- sum(grepl(code, v2))
        
        if(a>0) {
                
                c <- "rural"}
        
        if(b>0) {
                
                c <- "urban"} 
        
        if(b==0 & a ==0) {
                
                c <- "code not found"
        }
        
        }
        
        return(c)
}

## Replace codes in original data set with factor variable
## (urban and rural). 

dat1 <- dat0

dat1$place.of.birth <- sapply(dat0$place.of.birth, rurORurb)

dat1$location.prior.address <- sapply(dat1$location.prior.address, 
                                   rurORurb)

dat1$current.address <- sapply(dat1$current.address, 
                                      rurORurb)

## Save data
saveRDS(dat1, "clean_data_urban_rural")




