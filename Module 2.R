## This module generates a dataframe classifying each postcode
## in terms of rural/urban and coast/highlands. 


## ----------------------------------------------------------------##
## GOAL: load packages. 
library(pacman)
pacman::p_load(pdftools,stringr)


## ----------------------------------------------------------------##
## GOAL: download raw data.  

td = tempdir()
temp = tempfile(tmpdir=td, fileext=".pdf")
download.file("https://aplicaciones2.ecuadorencifras.gob.ec/SIN/descargas/cge2019.pdf",temp, mode = "wb")
pdf0 <- pdf_text(temp)
unlink(temp)


## ----------------------------------------------------------------##
## GOAL: prepare loaded character string for extraction. 

pdf0 <- tolower(paste(pdf0, collapse =""))
pdf0 <- gsub(" ", "", pdf0)


## ----------------------------------------------------------------## 
## GOAL: Create data frame (df.codes) of all postal codes in Ecuador, with
## additional columns of separated codes (provincial level, canton level, 
## parroquia level)

## METHOD: Extract all codes from pdf file.

df.codes <- data.frame(str_extract_all(pdf0, "[0-9]{6}"))

## METHOD: Add seperator between codes and split the codes into new variables. 

temp1 <- gsub("([0-9]{2})([0-9]{2})([0-9]{2})", "\\1-\\2-\\3", 
                      df.codes[, 1])
temp1 <- str_split_fixed(temp1, "-", 3)

## METHOD: combine the two dataframes into one.
df.codes <- cbind(df.codes, temp1)
names(df.codes) <- c("postcode", "provincecode", "cantoncode", "parroquiacode")


## ----------------------------------------------------------------##
## GOAL: Create data frame (dat.area) of postal codes matched with urban or 
## rural area. 

## METHOD: Load useful function to change text according to pattern. 

change_name <- function(x, original, new) {
        
        for(i in 1:length(original)) {
                
                x <- gsub(original[i], new[i], x)
                
        }
        
        return(x)
}

## METHOD: Change text according to pattern in order to demark
## the beginning and end of rural code lists. 

original <- c("rural", "cantón", "zonasenestudio", "zonam")
new <- c("splitsplitstartrural", "endruralsplitsplit",
         "endruralsplitsplit", "endruralsplitsplit")
pdf1 <- change_name(pdf0, original,new)

## METHOD: Create a list of the list numbers of the elements
## containing chunks of text with rural postal codes.

pdf1 <- str_split(pdf1, "splitsplit")
rural.list <- unlist(lapply(pdf1, grep, pattern = 
                              "startrural(.*)endrural"))

## METHOD: Separate the list into a list with urban postcodes
## and a list with rural postcodes. 
rural.text <- pdf1[[1]][rural.list]
urban.text <- pdf1[[1]][-rural.list]

## METHOD: Extract the codes from each list and store them 
## into a dataframe. 1 indicates urban. 2 indicates rural. 
rural.codes <- data.frame(unique(unlist(str_extract_all(rural.text, 
                               "[0-9][0-9][0-9][0-9][0-9][0-9]"))))
urban.codes <- data.frame(unique(unlist(str_extract_all(urban.text, 
                                "[0-9][0-9][0-9][0-9][0-9][0-9]"))))

names(rural.codes) <- "postcodes"
rural.codes <- mutate(rural.codes, area = 1)
names(urban.codes) <- "postcodes"
urban.codes <- mutate(urban.codes, area = 0)
dat.area <- rbind(rural.codes, urban.codes)

remove(new, original, rural.list, pdf1, rural.text, urban.text, rural.codes, 
       urban.codes)


## ----------------------------------------------------------------##
## GOAL: create a dataframe which matches each postal code to the area (rural/
## urban.

## METHOD: Create a vector of the codes of coastal provinces based on 
## https://es.wikipedia.org/wiki/Regi%C3%B3n_Costa#Divisi%C3%B3n_pol%C3%ADtica

prov.codes <- unlist(str_extract_all(pdf0, "[0-9][0-9]provincia\\s*(.*)\\s*comprende"))
coast.names <- list("eloro", "esmeraldas", "guayas", "losríos", "manabí", "santaelena",
                  "santodomingodelostsáchilas")
coast.codes <- sapply(coast.names, grep, x = prov.codes, value = TRUE)
coast.codes <- unlist(str_extract_all(coast.codes, "[0-9][0-9]"))

## METHOD: Create region variable which indicates coastal/ highlands
## identification for each postcode. At this stage we will filter out
## codes starting with 90, as these are (according to the pdf file, p. 49)
## under observation and unclassified. The reason for this seems to stem
## from a variety of political deciosions. (https://en.wikipedia.org/wiki/Provinces_of_Ecuador).
## 1 indicates highlands. 2 indicates coast. 

dat2 <- dat1 %>%
        mutate(region = str_extract_all(dat1$postcodes, "^[0-9][0-9]")) %>%
        filter(!region == "90") %>%
        mutate(region = ifelse(region %in% coast.codes, 
                                         1, 0))
remove(dat1, prov.codes, coast.names, coast.codes)

## GOAL: Create a dataframe matching postal codes with canton name.

## METHOD: Extract postal codes with canton name from the pdf file, create
## a seperator and then split the string based on the separator to create a
## temporary dataframe. 

canton.codes <- unlist(
        str_extract_all(pdf0, 
                        "[0-9][0-9][0-9][0-9]cantón\\s*(.*)\\s*comprende"))

canton.codes <- gsub(pattern = "cantón", replacement = "-", canton.codes)
canton.codes <- gsub(pattern = "\r\ncomprende", replacement = "", canton.codes)
canton.codes <- data.frame(str_split_fixed(canton.codes, pattern = "-", 2))

## METHOD: Clean up dataframe names. 
names(canton.codes) <- c("code", "canton.name")


## ----------------------------------------------------------------##
## GOAL: return codes as numeric vector
dat2$postcodes <- as.integer(dat2$postcodes)


## ----------------------------------------------------------------##
## GOAL: Save data. 
saveRDS(dat2, file = "clean_data_area_region.rds")
rm(list = ls())