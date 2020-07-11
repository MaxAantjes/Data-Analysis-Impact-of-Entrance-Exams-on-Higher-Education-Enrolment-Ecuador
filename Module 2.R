## This module generates a dataframe classifying each postcode
## in terms of rural/urban and coast/highlands. 


## ----------------------------------------------------------------##
## GOAL: load packages. 
library(pacman)
pacman::p_load(pdftools,stringr,tidyverse)


## ----------------------------------------------------------------##
## GOAL: download raw data.  
url <- "https://aplicaciones2.ecuadorencifras.gob.ec/SIN/descargas/cge2019.pdf"
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".pdf")
download.file(url,temp, mode = "wb")
pdf0 <- pdf_text(temp)
unlink(temp)
remove(url)


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

provincecode <- unlist(str_extract_all(df.codes[,1], "^[0-9]{2}"))
cantoncode <- unlist(str_extract_all(df.codes[,1], "^[0-9]{4}"))

## METHOD: combine the two dataframes into one.
df.codes <- cbind(df.codes, provincecode, cantoncode)
names(df.codes) <- c("postcode", "provincecode", "cantoncode")
remove(temp1)


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

original <- c("rural", "(?:cantón|cantòn)", "zonasenestudio", "zonam")
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

names(rural.codes) <- "postcode"
rural.codes <- mutate(rural.codes, area = 1)
names(urban.codes) <- "postcode"
urban.codes <- mutate(urban.codes, area = 0)
df.area <- rbind(rural.codes, urban.codes)

remove(new, original, rural.list, pdf1, rural.text, urban.text, rural.codes, 
       urban.codes)


## ----------------------------------------------------------------##
## GOAL: create a dataframe which matches each postal code to the region
## (coast or highlands)

## METHOD: Create a vector of the codes of coastal provinces based on 
## https://es.wikipedia.org/wiki/Regi%C3%B3n_Costa#Divisi%C3%B3n_pol%C3%ADtica

coast.names <- c("eloro", "esmeraldas", "guayas", "losríos", "manabí", 
                 "santaelena", "santodomingodelostsáchilas")

## METHOD: Create a dataframe of strings with information on each province and
## province code. Then, split strings and check for matches with coast.names
## vector to create a three variable dataframe (provincename, provincecode and
## region)

df.region <- data.frame(
        unlist(str_extract_all(pdf0, 
                               "[0-9]{2}provincia\\s*(.*)\\s*comprende")))
names(df.region) <- "temp"

df.region <- df.region %>%
        mutate(provincename = gsub(
        "[0-9]{2}provincia(?:de|del)(\\s*(.*)\\s*)\r\ncomprende", "\\1",
               temp)) %>%
        mutate(provincecode = str_extract(temp, "^[0-9]{2}")) %>%
        mutate(region = ifelse(provincename %in% coast.names, 1, 0)) %>%
        select(-temp)

remove(coast.names)


## GOAL: Create a dataframe matching postal codes with canton name.

## METHOD: Extract postal codes with canton name from the pdf file, create
## a seperator and then split the string based on the separator to create a
## temporary dataframe. 

canton.codes <- unlist(
        str_extract_all(
                pdf0, "[0-9]{4}(?:cantón|cantòn|..cantón)\\s*(.*)\\s*(?:\r\n[0-9]\r\nc|\r\n[0-9]{2}\r\nc|c)omprende"))

canton.codes <- gsub(pattern = "(?:cantón|cantòn|[^0-9][^0-9]cantón)", 
                     replacement = "-", canton.codes)
canton.codes <- gsub(pattern = "(?:\r\ncomprende|\r\n[0-9]\r\ncomprende|\r\n[0-9]{2}\r\ncomprende)", replacement = "", canton.codes)
df.canton <- data.frame(str_split_fixed(canton.codes, pattern = "-", 2))

## METHOD: Clean up dataframe names. 
names(df.canton) <- c("cantoncode", "cantonname")
remove(canton.codes)


## ----------------------------------------------------------------##
## GOAL: merge all dataframes together

## METHOD: utilise merge function. 

dat0 <- df.codes %>%
        left_join(df.area, by = "postcode") %>%
        left_join(df.region, by = "provincecode") %>%
        left_join(df.canton, by = "cantoncode") %>%
        select(1, 2, 5, 3, 7, 4, 5, 6)

## METHOD: check for missing values.

unique(dat0$cantoncode[is.na(dat0$cantonname)])
unique(dat0$provincecode[is.na(dat0$provincename)])
unique(dat0$postcode[is.na(dat0$area)])


## ----------------------------------------------------------------##
## GOAL: return codes as numeric vector
dat2$postcodes <- as.integer(dat2$postcodes)


## ----------------------------------------------------------------##
## GOAL: Save data. 
saveRDS(dat2, file = "clean_data_area_region.rds")
rm(list = ls())



## METHOD: Create region variable which indicates coastal/ highlands
## identification for each postcode. At this stage we will filter out
## codes starting with 90, as these are (according to the pdf file, p. 49)
## under observation and unclassified. The reason for this seems to stem
## from a variety of political deciosions. (https://en.wikipedia.org/wiki/Provinces_of_Ecuador).
## 1 indicates highlands. 2 indicates coast. 