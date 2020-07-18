## This module generates a dataframe classifying each postcode
## in terms of student/teacher ratio per academic year.


## ----------------------------------------------------------------##
## GOAL: load packages.
pacman::p_load(openxlsx, stringr, tidyverse, zoo)


## ----------------------------------------------------------------##
## GOAL: download data. 
df0 <- readRDS("postcode_classification.rds")

## METHOD: download and extract excel files for accepted places per higher 
## per recipient canton and canton of origin. 
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
dfuni2 <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 17, rows = 16:241)
dftec2 <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 17, rows = 243:466)
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
## GOAL: Load Functions. 

replace_special_char <- function(x) {
        
        original <- c("á", "é", "ó", "ú", "ñ", "í")
        new <- c("a", "e", "o", "u", "n", "i")
        
        for(i in 1:length(original)) {
                
                x <- gsub(original[i], new[i], x)
                
        }
        
        return(x)
}

## ----------------------------------------------------------------##
## GOAL: create a dataframe with the number of accepted places per canton of 
## recipient higher education institution. 

## METHOD: remove first line and column, as they contain no data. Additionally
## remove province data as we already have cantons matched with provinces. 
places.canton.uni <- dfuni %>%
        filter(!(Tipo.de.IES %in% "Total")) %>%
        select(-c(Tipo.de.IES, Provincia.del.Campus)) %>%

## METHOD: merge the two semester columns into annual columns.
        mutate(accepted.offers.inflow.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.inflow.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.inflow.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.inflow.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.inflow.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.inflow.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.inflow.2018 = rowSums(.[14])) %>%
        mutate(cantonname.nsp = tolower(Cantón.del.Campus)) %>%
        select(-Cantón.del.Campus) %>%
        mutate(cantonname.nsp = gsub(" ", "", cantonname.nsp)) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%
        
        ## METHOD: merge the doubly mentioned Quito (distritometropolitanodequito
        ## and quito obviously refer to the same area as there is no data overlap.
        mutate(cantonname.nsp = gsub("distritometropolitanodequito", 
                                     "quito", cantonname.nsp)) %>%
        group_by(cantonname.nsp) %>%
        summarise_all(list(sum)) %>%
        select(-c(2:14)) 
        

## METHOD: clean data set of tecnological schools.
names(dftec) <- names(dfuni)
places.canton.tec <-dftec %>%
        select(-c(Tipo.de.IES, Provincia.del.Campus)) %>%
        
        ## METHOD: merge the two semester columns into annual columns
        mutate(accepted.offers.inflow.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.inflow.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.inflow.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.inflow.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.inflow.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.inflow.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.inflow.2018 = rowSums(.[14])) %>%
        mutate(cantonname.nsp = tolower(Cantón.del.Campus)) %>%
        select(-Cantón.del.Campus) %>%
        mutate(cantonname.nsp = gsub(" ", "", cantonname.nsp)) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%

        ## METHOD: merge the doubly mentioned yantzaza (yantzaza and
        ## yantzaza(yantzaza) obviously refer to the same area as there is no 
        ## data overlap.
        mutate(cantonname.nsp = gsub("\\(.*\\)", "", cantonname.nsp)) %>%
        group_by(cantonname.nsp) %>%
        summarise_all(list(sum)) %>%
        select(-c(2:14))

## METHOD: merge datasets
places.canton <- merge(places.canton.uni, places.canton.tec,
                       by = "cantonname.nsp", all.x = TRUE, all.y = TRUE,
                       suffixes = c(".uni", ".tec"))

## METHOD: replace the newly created NA values with 0, as their omission in the
## respective data sets indicates that there are no uni or technical school
## places available. 
places.canton[is.na(places.canton)] <- 0

## METHOD: Create a new variable with total higher education places available
## per year.
df.places.inflow.canton <- places.canton %>%
        mutate(accepted.offers.inflow.total.2012 = rowSums(.[,c(2,9)])) %>%
        mutate(accepted.offers.inflow.total.2013 = rowSums(.[,c(3,10)])) %>%
        mutate(accepted.offers.inflow.total.2014 = rowSums(.[,c(4,11)])) %>%
        mutate(accepted.offers.inflow.total.2015 = rowSums(.[,c(5,12)])) %>%
        mutate(accepted.offers.inflow.total.2016 = rowSums(.[,c(6,13)])) %>%
        mutate(accepted.offers.inflow.total.2017 = rowSums(.[,c(7,14)])) %>%
        mutate(accepted.offers.inflow.total.2018 = rowSums(.[,c(8,15)])) 

remove(dftec, dfuni, places.canton, places.canton.tec, places.canton.uni)

## ----------------------------------------------------------------##
## GOAL: create a dataframe with the number of accepted places per canton
## of higher education institution.

## METHOD: remove first line and column, as they contain no data. Additionally
## remove province data as we already have cantons matched with provinces. 
places.canton.uni2 <- dfuni2 %>%
        filter(!(Tipo.de.IES %in% "Total")) %>%
        select(-c(Tipo.de.IES, Provincia.del.Campus)) %>%
        
        ## METHOD: merge the two semester columns into annual columns.
        mutate(accepted.offers.outflow.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.outflow.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.outflow.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.outflow.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.outflow.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.outflow.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.outflow.2018 = rowSums(.[14])) %>%
        mutate(cantonname.nsp = tolower(Cantón.del.Campus)) %>%
        select(-Cantón.del.Campus) %>%
        mutate(cantonname.nsp = gsub(" ", "", cantonname.nsp)) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%
        
        ## METHOD: rename (distritometropolitanodequito) as Quito to match with
        ## other data sets. 
        mutate(cantonname.nsp = gsub("distritometropolitanodequito", 
                                     "quito", cantonname.nsp)) %>%
        select(-c(2:14)) 


## METHOD: clean data set of tecnological schools.
names(dftec2) <- names(dfuni2)
places.canton.tec2 <-dftec2 %>%
        select(-c(Tipo.de.IES, Provincia.del.Campus)) %>%
        
        ## METHOD: merge the two semester columns into annual columns
        mutate(accepted.offers.outflow.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.outflow.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.outflow.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.outflow.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.outflow.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.outflow.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.outflow.2018 = rowSums(.[14])) %>%
        mutate(cantonname.nsp = tolower(Cantón.del.Campus)) %>%
        select(-Cantón.del.Campus) %>%
        mutate(cantonname.nsp = gsub(" ", "", cantonname.nsp)) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%
        
        ## METHOD: rename (distritometropolitanodequito) as Quito to match with
        ## other data sets. 
        mutate(cantonname.nsp = gsub("distritometropolitanodequito", 
                                     "quito", cantonname.nsp)) %>%
        select(-c(2:14))

## METHOD: merge datasets
places.canton.outflow <- merge(places.canton.uni2, places.canton.tec2,
                       by = "cantonname.nsp", all.x = TRUE, all.y = TRUE,
                       suffixes = c(".uni", ".tec"))

## METHOD: replace the newly created NA values with 0, as their omission in the
## respective data sets indicates that there are no uni or technical school
## places available. 
places.canton[is.na(places.canton)] <- 0

## METHOD: Create a new variable with total higher education places available
## per year.
df.places.outflow.canton <- places.canton %>%
        mutate(accepted.offers.outflow.total.2012 = rowSums(.[,c(2,9)])) %>%
        mutate(accepted.offers.outflow.total.2013 = rowSums(.[,c(3,10)])) %>%
        mutate(accepted.offers.outflow.total.2014 = rowSums(.[,c(4,11)])) %>%
        mutate(accepted.offers.outflow.total.2015 = rowSums(.[,c(5,12)])) %>%
        mutate(accepted.offers.outflow.total.2016 = rowSums(.[,c(6,13)])) %>%
        mutate(accepted.offers.outflow.total.2017 = rowSums(.[,c(7,14)])) %>%
        mutate(accepted.offers.outflow.total.2018 = rowSums(.[,c(8,15)]))

## ----------------------------------------------------------------##
## GOAL: create a dataframe with population per canton.

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

## The dataset is a pivotted table, so to access certain data subsetting rows is 
## necessary. In the Nombre.del.Cantón column the value "Total" indicates the 
## start of a section with the summed values per canton within a province. In 
## that section the cantonname is (paradoxically) found in the 
## Nombre.de.la.Parroquia variable. There are 3 values for each canton in the 
## ÁREA column ("Rural", "Urban" and "Total"). The row with the "Total" value 
## gives the total population dived for each age group. Accordingly, we can 
## extract the population for each canton by subsetting the "Total" values
## of the Nombre.de.la.Parroquia variable and the "Total" values of the Área 
## variables. 

## Another further three operations need to be performed. Firstly, the pivoted
## values are only filled in for the first row of the grouping. Accordingly,
## those values need to be filled (otherwise empty values will be subsetted). 

## Secondly, the entire table has been duplicated. One reason for this could
## be to have an alphabetically and non-alphabetically ordered version (the 
## second set is ordered alphabetically by canton). We can must thus subset
## only one of the two data sets. Testing for this duplication and for whether
## the values between the two duplicates match and in which rows the second 
## dataset can be found (row 250-472) can be found in the apendix. 

## Finally, the cantonnames should be cleared up for special characters to
## allow the data set to be merged. For this we can use the 
## replace_special_char function. 


## OPERATION 1: fill pivoted missing values.
dat1[dat1 == ""] <- NA
dat1$Nombre.del.Cantón[2:length(dat1$Nombre.del.Cantón)] <- 
        na.locf(dat1$Nombre.del.Cantón)
dat1$Nombre.de.la.Parroquia[2:length(dat1$Nombre.de.la.Parroquia)] <-
        na.locf(dat1$Nombre.de.la.Parroquia)

## OPERATION 1: Create dataframe with population per cantón.
dat2 <- dat1 %>%
        mutate(subset1 = tolower(gsub(" ", "", Nombre.de.la.Parroquia))) %>%
        mutate(subset2 = tolower(gsub(" ", "", ÁREA))) %>%
        filter(subset1 == "total" & subset2 == "total") %>%
        mutate(X.5 = gsub(",", "", X.5)) %>%
        mutate(X.6 = gsub(",", "", X.6)) %>%
        mutate(total.population.2010 = as.integer(gsub(",", "", X.22))) %>%
        mutate(young.adult.population.2010 = as.integer(X.5) + 
                       as.integer(X.6)) %>%
        rename(cantonname = Nombre.del.Cantón) %>%
        select(cantonname, young.adult.population.2010, total.population.2010)

## OPERATION 2: Rows 240 to 471 have been subsetted. 
dat3 <- dat2[250:471,]

## OPERATION 3: spcial characters need to be subsetted. 
df.population <- dat3 %>%
        mutate(cantonname.nsp = tolower(gsub(" ", "", cantonname))) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%
        select(cantonname, cantonname.nsp, young.adult.population.2010,
               total.population.2010)

remove(dat0, dat1, dat2, dat3)

## Test set: mocache = 3,076 + 2,640; cayambe = 7,801 + 7,413; 
## mangadelcura = 1,574	1,486.
unique(df.population$young.adult.population.2010[
        df.population$cantonname.nsp=="mocache"])
unique(df.population$young.adult.population.2010[
        df.population$cantonname.nsp=="cayambe"])
unique(df.population$young.adult.population.2010[
        df.population$cantonname.nsp=="mangadelcura"])





