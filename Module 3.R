## This module generates a dataframe classifying each postcode
## in terms of student/teacher ratio per academic year.


## ----------------------------------------------------------------##
## GOAL: load packages.
pacman::p_load(openxlsx, stringr, tidyverse, zoo)

df0 <- readRDS("postcode_classification.rds")


## ----------------------------------------------------------------##
## GOAL: create dataframe of the accepted places of higher education 
## institutions located at certain cantones per year. 

## METHOD: download and extract excel files.
url <- "https://www.educacionsuperior.gob.ec/wp-content/uploads/downloads/2019/02/Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx.zip"
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".zip")
unzip(temp, list = TRUE)
download.file(url,temp)
unzip(temp, exdir = td, overwrite = TRUE)
dfuni <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 14, rows = 16:78)
dftec <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 14, rows = 80:143)
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
        mutate(accepted.offers.2018 = rowSums(.[14])) %>%
        mutate(cantonname = tolower(Cantón.del.Campus)) %>%
        mutate(cantonname = gsub(" ", "", cantonname)) %>%
        select(-c(1:14)) 
        

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
        mutate(accepted.offers.2018 = rowSums(.[14])) %>%
        mutate(cantonname = tolower(Cantón.del.Campus)) %>%
        mutate(cantonname = gsub(" ", "", cantonname)) %>%
        select(-c(1:14))

## METHOD: merge datasets
places.canton <- merge(places.canton.uni, places.canton.tec,
                       by = "cantonname", all.x = TRUE, all.y = TRUE,
                       suffixes = c(".uni", ".tec"))

## METHOD: replace the newly created NA values with 0, as their omission in the
## respective data sets indicates that there are no uni or technical school
## places available. 
places.canton[is.na(places.canton)] <- 0

## METHOD: Create a new variable with total higher education places available
## per year.
places.canton <- places.canton %>%
        mutate(accepted.offers.2012 = rowSums(.[,c(2,9)])) %>%
        mutate(accepted.offers.2013 = rowSums(.[,c(3,10)])) %>%
        mutate(accepted.offers.2014 = rowSums(.[,c(4,11)])) %>%
        mutate(accepted.offers.2015 = rowSums(.[,c(5,12)])) %>%
        mutate(accepted.offers.2016 = rowSums(.[,c(6,13)])) %>%
        mutate(accepted.offers.2017 = rowSums(.[,c(7,14)])) %>%
        mutate(accepted.offers.2018 = rowSums(.[,c(8,15)])) 
        
## METHOD: merge data set with the postcode data set.
df1 <- merge(df0, places.canton, by = "cantonname")


## Next data set required will have to be downloaded manually, as the file has 
## erroneously been saved as a 1993 Excel 5.0 file. The problems this generates
## become evident from the following github conversation:
## https://github.com/tidyverse/readxl/issues/618. Simply download the file 
## available here:
## https://www.ecuadorencifras.gob.ec/wp-content/plugins/download-monitor/download.php?id=324&force=1 
## then save it as a csv file in your working directory under the name
## population.csv. 

dat0 <- read.csv("population.csv", skip = 10)
dat1 <- dat0

## check for mistakes in canton column.
sum(!is.na(dat0$Nombre.del.Cantón[dat0$Nombre.del.Cantón == "Total"])) 

## Replace mistakes and check.
dat1 <- dat1 %>%
        mutate(cantonname = ifelse(Nombre.del.Cantón == "Total", NA, 
                                   Nombre.del.Cantón))
sum(!is.na(dat1$Nombre.del.Cantón[dat1$cantonname == "Total"])) 

## Fill missing values.
dat1[dat1 == ""] <- NA
dat1$cantonname[2:length(dat1$cantonname)] <- na.locf(dat1$cantonname)
dat1$Nombre.de.la.Parroquia[2:length(dat1$Nombre.de.la.Parroquia)] <-
        na.locf(dat1$Nombre.de.la.Parroquia)

## Create dataframe with young adult population (20-29 y/o) per cantón. 
dat2 <- dat1 %>%
        mutate(subset1 = gsub(" ", "", Nombre.de.la.Parroquia)) %>%
        mutate(subset2 = gsub(" ", "", ÁREA)) %>%
        filter(subset1 == "Total" & subset2 == "Total") %>%
        mutate(X.5 = gsub(",", "", X.5)) %>%
        mutate(X.6 = gsub(",", "", X.6)) %>%
        mutate(young.adult.population = as.integer(X.5) + as.integer(X.6)) %>%
        mutate(cantonname = tolower(gsub(" ", "", cantonname))) %>%
        select(cantonname, young.adult.population)
        
