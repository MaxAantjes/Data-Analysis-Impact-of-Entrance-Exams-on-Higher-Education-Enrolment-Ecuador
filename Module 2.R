## This module generates a dataframe classifying each postcode
## in terms of rural/urban and coast/highlands. 


## ----------------------------------------------------------------##
## GOAL: load packages. 
library(pacman)
pacman::p_load(pdftools,stringr,tidyverse,openxlsx)


## ----------------------------------------------------------------##
## GOAL: download raw data. 

## METHOD: download government file on the rural/urban division of parroquias. 
url <- "https://aplicaciones2.ecuadorencifras.gob.ec/SIN/descargas/cge2019.pdf"
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".pdf")
download.file(url,temp, mode = "wb")
pdf0 <- pdf_text(temp)
unlink(temp)
remove(url)

## METHOD: download and extract excel files for population dataframe.
## The data required will have to be downloaded manually, as the file has 
## erroneously been saved as a 1993 Excel 5.0 file. The problems this generates
## become evident from the following github conversation:
## https://github.com/tidyverse/readxl/issues/618.Download 
## download the following file from the Ecuadorian government with
## all postcode matches during the 2010 census -
## https://www.ecuadorencifras.gob.ec/wp-content/plugins/download-monitor/download.php?id=334&force=1
## convert and save it in the working directory as a .xlsx file with the name
## "postcodes.xlsx"

check <- function(x){if(file.exists(x) == FALSE){warning(
        "STOP: download file manually")}}
check("postcodes.xlsx")


## ----------------------------------------------------------------##
## GOAL: prepare loaded character string for extraction. 

pdf0 <- tolower(paste(pdf0, collapse =""))
pdf0 <- gsub(" ", "", pdf0)


## ----------------------------------------------------------------## 
## GOAL: Create data frame (df.codes) of all postal codes in Ecuador, with
## additional columns of separated codes (provincial level, canton level, 
## parroquia level)

## METHOD: Write a function which replaces all special characters with normal
## characters to make merging different data sets (with different use of 
## special characters possible).

replace_special_char <- function(x) {
        
        original <- c("á", "é", "ó", "ú", "ñ", "í")
        new <- c("a", "e", "o", "u", "n", "i")
        
        for(i in 1:length(original)) {
                
                x <- gsub(original[i], new[i], x)
                
        }
        
        return(x)
}

## METHOD: load the province data.

temp.provinces <- read.xlsx("postcodes.xlsx", sheet = 1, rows = 11:36, 
                            colNames = FALSE)
temp.provinces <- data.frame(str_split(temp.provinces[,1], 
                                       "\\.", 2, simplify = TRUE))
temp.provinces <- temp.provinces %>%
        mutate(X1 = gsub(" ", "", X1)) %>%
        mutate(provincecode = ifelse(nchar(X1) < 2, paste0("0", X1), X1)) %>%
        mutate(provincename = X2) %>%
        mutate(X2 = tolower(gsub(" ", "", X2))) %>%
        mutate(provincename.nsp = replace_special_char(X2)) %>%
        select(provincecode, provincename, provincename.nsp)

## METHOD: load the canton data and prepare for merge through provincecode.
        
temp.cantons <- read.xlsx("postcodes.xlsx", sheet = 2, rows = 11:240, 
                            colNames = FALSE)
temp.cantons <- data.frame(str_split(temp.cantons[,1], 
                                       "\\.", 2, simplify = TRUE))
temp.cantons <- temp.cantons %>%
        mutate(X1 = gsub(" ", "", X1)) %>%
        mutate(cantoncode = ifelse(nchar(X1) < 4, paste0("0", X1), X1)) %>%
        mutate(provincecode = str_extract(cantoncode, "^[0-9]{2}")) %>%
        mutate(cantonname = X2) %>%
        mutate(X2 = tolower(gsub(" ", "", X2))) %>%
        mutate(cantonname.nsp = replace_special_char(X2)) %>%
        select(cantoncode, provincecode, cantonname, cantonname.nsp)

## METHOD: load the parroquia data and prepare for merge through cantoncode. 

temp.parroquia <- read.xlsx("postcodes.xlsx", sheet = 3, rows = 11:1031, 
                          colNames = FALSE)
temp.parroquia <- data.frame(str_split(temp.parroquia[,1], 
                                     "\\.", 2, simplify = TRUE))
temp.parroquia <- temp.parroquia %>%
        mutate(X1 = gsub(" ", "", X1)) %>%
        mutate(parroquiacode = ifelse(nchar(X1) < 6, paste0("0", X1), X1)) %>%
        mutate(cantoncode = str_extract(parroquiacode, "^[0-9]{4}")) %>%
        mutate(parroquianame = X2) %>%
        mutate(X2 = tolower(gsub(" ", "", X2))) %>%
        mutate(parroquianame.nsp = replace_special_char(X2)) %>%
        select(parroquiacode, cantoncode, parroquianame, parroquianame.nsp)

## METHOD: merge the dataframes
df.codes <- temp.parroquia %>%
        left_join(temp.cantons, by = "cantoncode") %>%
        left_join(temp.provinces, by = "provincecode") %>%
        select(1,2,5,3,6,8,4,7,9)


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

original <- c("rural", "cant(?:ó|ò)n", "zonam", 
              "zonasenestudio", "conlasparroquiasurbanas")
new <- c("endurban-split-startrural", "endurbanendrural-split-starturban", 
         "endurbanendrural-split-starturban", "endrural-split-", 
         "endurban-split-starturban")
pdf1 <- change_name(pdf0, original,new)

## METHOD: Create a list of the list numbers of the elements
## containing chunks of text with rural postal codes.

pdf1 <- str_split(pdf1, "-split-")
rural.list <- unlist(lapply(pdf1, grep, pattern = 
                                    "startrural(.*)endrural"))
urban.list <- unlist(lapply(pdf1, grep, pattern = 
                                    "starturban(.*)endurban"))

## METHOD: Separate the list into a list with urban postcodes
## and a list with rural postcodes. 
rural.text <- pdf1[[1]][rural.list]
urban.text <- pdf1[[1]][urban.list]

## METHOD: Extract the codes from each list and store them 
## into a dataframe with variable area (1 = rural, 0 = urban). 
rural.codes <- data.frame(unlist(str_extract_all(rural.text, 
                               "[0-9]{6}")))
urban.codes <- data.frame(unique(unlist(str_extract_all(urban.text, 
                                "[0-9]{6}"))))

names(rural.codes) <- "parroquiacode"
rural.codes <- mutate(rural.codes, area = 1)
names(urban.codes) <- "parroquiacode"
urban.codes <- mutate(urban.codes, area = 0)
df.area <- rbind(rural.codes, urban.codes)

remove(new, original, rural.list, pdf1, rural.text, urban.text, rural.codes, 
       urban.codes)


## ----------------------------------------------------------------##
## GOAL: create a dataframe which matches each postal code to the region
## (coast or highlands)

## METHOD: Create a vector of the codes of coastal provinces based on 
## https://es.wikipedia.org/wiki/Regi%C3%B3n_Costa#Divisi%C3%B3n_pol%C3%ADtica

coast.names <- c("eloro", "esmeraldas", "guayas", "losrios", "manabi", 
                 "santaelena", "santodomingo")

## METHOD: Create a dataframe which specifies whether a province belongs to
## the coast or highlands (1 = coast, 0 = highlands).
df.region <- data.frame()

df.region <- data.frame(unique(df.codes$provincename.nsp))
names(df.region) <- "provincename.nsp"
df.region <- df.region %>%
        mutate(region = ifelse(provincename.nsp %in% coast.names, 1, 0))
remove(coast.names)


## ----------------------------------------------------------------##
## GOAL: merge all dataframes together

## METHOD: utilise merge function. 

dat0 <- df.codes %>%
        left_join(df.area, by = "parroquiacode") %>%
        left_join(df.region, by = "provincename.nsp") 

## METHOD: check for missing values.

sum(is.na(dat0$cantonname))/nrow(dat0) 
sum(is.na(dat0$region))/nrow(dat0)
sum(is.na(dat0$area))/nrow(dat0)

## RESULTS: Only postcodes starting with 90 were unclassified. The reason for
## this is that they are under observation and not assigned any province
## (accordign to the pdf file, p.49). This is reflected  a variety of political
## decisions leading to "non-delimited" areas. (see: 
## https://en.wikipedia.org/wiki/Provinces_of_Ecuador).

## ----------------------------------------------------------------##
## GOAL: Save data. 
saveRDS(dat0, file = "postcode_classification.rds")
rm(list = ls())