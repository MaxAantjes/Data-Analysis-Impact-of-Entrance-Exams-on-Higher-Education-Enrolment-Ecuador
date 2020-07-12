## This module generates a dataframe classifying each postcode
## in terms of student/teacher ratio per academic year.


## ----------------------------------------------------------------##
## GOAL: load packages.
pacman::p_load(openxlsx, stringr, tidyverse)

df0 <- readRDS("postcode_classification.rds")


## ----------------------------------------------------------------##
## GOAL: create dataframe of the accepted places of university places available
## per canton per year. 

## METHOD: download and extract excel file
url <- "https://www.educacionsuperior.gob.ec/wp-content/uploads/downloads/2019/02/Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx.zip"
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".zip")
unzip(temp, list = TRUE)
download.file(url,temp)
unzip(temp, exdir = td, overwrite = TRUE)
dat0 <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 14, rows = 16:62)
unlink(temp)
remove(url)

## clean data set
places.canton <- dat0 %>%
        filter(!(Tipo.de.IES %in% "Total")) %>%
        select(-c(Tipo.de.IES, Provincia.del.Campus)) %>%

## merge the two semester columns into annual columns
        mutate(available.places.2012 = rowSums(.[2:3])) %>%
        mutate(available.places.2013 = rowSums(.[4:5])) %>%
        mutate(available.places.2014 = rowSums(.[6:7])) %>%
        mutate(available.places.2015 = rowSums(.[8:9])) %>%
        mutate(available.places.2016 = rowSums(.[10:11])) %>%
        mutate(available.places.2017 = rowSums(.[12:13])) %>%
        mutate(available.places.2018 = rowSums(.[14:15])) %>%
        select(-c(2:15))
