## This module generates a dataframe classifying each postcode
## in terms of student/teacher ratio per academic year.


## ----------------------------------------------------------------##
## GOAL: load packages.
pacman::p_load(openxlsx, stringr, tidyverse)

df0 <- readRDS("postcode_classification.rds")


## ----------------------------------------------------------------##
## GOAL: create dataframe of the accepted places of higher education 
## institutions available per canton per year. 

## METHOD: download and extract excel files.
url <- "https://www.educacionsuperior.gob.ec/wp-content/uploads/downloads/2019/02/Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx.zip"
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".zip")
unzip(temp, list = TRUE)
download.file(url,temp)
unzip(temp, exdir = td, overwrite = TRUE)
dfuni <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 14, , rows = 16:62)
dftec <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 14, rows = 64:127)
unlink(temp)
remove(url)

## METHOD: clean data set of university places.
places.canton.uni <- dfuni %>%
        filter(!(Tipo.de.IES %in% "Total")) %>%
        select(-c(Tipo.de.IES, Provincia.del.Campus)) %>%

## METHOD: merge the two semester columns into annual columns
        mutate(accepted.offers.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.2018 = rowSums(.[14:15])) %>%
        select(-c(2:15))

## METHOD: clean data set of tecnological schools
names(dftec) <- names(dfuni)
places.canton.tec <-dftec %>%
        select(-c(Tipo.de.IES, Provincia.del.Campus)) %>%
        
        ## METHOD: merge the two semester columns into annual columns
        mutate(accepted.offers.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.2018 = rowSums(.[14:15])) %>%
        select(-c(2:15))
        
        
