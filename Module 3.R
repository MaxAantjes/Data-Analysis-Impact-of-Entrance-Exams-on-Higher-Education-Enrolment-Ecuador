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
        sheet = 14, rows = 80:143, colNames = FALSE)
dfuni2 <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 17, rows = 16:241)
dftec2 <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 17, rows = 243:466, colNames = FALSE)
unlink(temp)
remove(url)

## METHOD: download and extract excel files for population dataframe.
## The data required will have to be downloaded manually, as the file has 
## erroneously been saved as a 1993 Excel 5.0 file. The problems this generates
## become evident from the following github conversation:
## https://github.com/tidyverse/readxl/issues/618.Download 
## download the following file from the Ecuadorian government with
## all postcode matches during the 2010 census -
## https://www.ecuadorencifras.gob.ec/wp-content/plugins/download-monitor/download.php?id=324&force=1
## convert and save it in the working directory as a .xlsx file with the name
## "population.xlsx". It was available (20/07/2020) from this website:
## https://www.ecuadorencifras.gob.ec/informacion-censal-cantonal/

check <- function(x){if(file.exists(x) == FALSE){warning(
        "STOP: download file manually")}}
check("population.xlsx")

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
## GOAL: Create a match set for cantonnames per province.

## Method: it turned out in the subsequent process that there is a
## mistake in the df0 data set. Pablo Sexus is mistakenly called Pablo Vi. 
## According to this website 
## (https://worldpostalcode.com/ecuador/morona-santiago/pablo-sexto)
## the canton matching Pablo Sexto is 1411. Yet, in the data set it is called
## Pablo Vi. We will replace the name in the dataset accordingly.

df0$cantonname.nsp[df0$cantoncode == 1411] <- "pablosexto"

## METHOD: add a row for unregistered people or data ('sin registro'). This 
## allows any 'missing values' in terms of cantonnames to be mergable into the 
## code dataframe by assigning them 'sinregistro'. 

na.row <- data.frame(999, 999, 999, "sinregistro", "sinregistro", "sinregistro", 
                     "sinregistro", "sinregistro", "sinregistro", 999, 999)
names(na.row) <- names(df0)   
df0 <- rbind(df0, na.row)

## METHOD: add a column to the code dataframe which merges the cantonname.nsp
## and provincename.nsp. As there are cantons with similar names in different
## provinces this will avoid false matchings. 

mergeset <- df0 %>%
        mutate(match = paste0(provincename.nsp, cantonname.nsp)) %>%
        select(cantoncode, match) %>% 
        distinct(cantoncode, .keep_all = TRUE)
remove(na.row)


## ----------------------------------------------------------------##
## GOAL: create a dataframe with the number of accepted places per canton of 
## recipient higher education institution. 

## METHOD: remove first line and column, as they contain no data. Additionally
## remove province data as we already have cantons matched with provinces. 
places.canton.uni <- dfuni %>%
        filter(!(Tipo.de.IES %in% "Total")) %>%
        select(-Tipo.de.IES) %>%
        
        ## METHOD: replace cantonnames with codes as there are cantons in 
        ## different provinces with the same name. First fill missing values of 
        ## provinces (due to pivoted table).
        
        mutate(provincename = na.locf(Provincia.del.Campus)) %>%
        mutate(provincename.nsp = tolower(gsub(" ", "", provincename))) %>%
        mutate(provincename.nsp = replace_special_char(provincename.nsp)) %>%
        mutate(cantonname.nsp = tolower(gsub(" ", "", Cantón.del.Campus))) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%
        
        ## merge the doubly mentioned Quito (distritometropolitanodequito
        ## and quito obviously refer to the same area as there is no data 
        ## overlap. Replace Santo Domingo ... province with 'santodomingo' and
        ## la libertad with 'libertad' and change the province of "la concordia"
        ## to Esmeraldas (changed provinces in 2012: 
        ## https://en.wikipedia.org/wiki/La_Concordia_Canton)
        ## in line with the postcode data set.
        mutate(cantonname.nsp = gsub("distritometropolitanodequito", 
                                     "quito", cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("lalibertad", "libertad", 
                                     cantonname.nsp)) %>%
        mutate(provincename.nsp = gsub("santodomingodelostsachilas",
                                     "santodomingo", provincename.nsp)) %>%
        mutate(provincename.nsp = ifelse(cantonname.nsp == "laconcordia",
                                         "esmeraldas", provincename.nsp)) %>%
        
        ## create match variable indicating province and canton. 
        mutate(match = paste0(provincename.nsp, cantonname.nsp))
        
        ## merge the data sets.
        places.canton.uni <- places.canton.uni %>%
                left_join(mergeset, by = "match") %>%
                select(c(20, 3:15)) %>%

        ## METHOD: merge the two semester columns into annual columns.
        mutate(accepted.offers.inflow.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.inflow.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.inflow.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.inflow.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.inflow.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.inflow.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.inflow.2018 = rowSums(.[14])) %>%
        select(-c(2:14)) %>%
        
        
        ## Finally, we sum the values for the repeated rows (the duplicated
        ## values of cantons who as described above changed name during data 
        ## collection) %>%
        group_by(cantoncode) %>%
        summarise_all(list(sum)) 
        
        ## No NA values were created. 
        sum(is.na(places.canton.uni))

## METHOD: clean data set of tecnological schools in the same manner.
names(dftec) <- names(dfuni)
 
places.canton.tec <- dftec %>%
        select(-Tipo.de.IES) %>%
        mutate(provincename = na.locf(Provincia.del.Campus)) %>%
        mutate(provincename.nsp = tolower(gsub(" ", "", provincename))) %>%
        mutate(provincename.nsp = replace_special_char(provincename.nsp)) %>%
        mutate(cantonname.nsp = tolower(gsub(" ", "", Cantón.del.Campus))) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("distritometropolitanodequito", 
                                     "quito", cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("lalibertad", "libertad", 
                                     cantonname.nsp)) %>%
        mutate(provincename.nsp = gsub("santodomingodelostsachilas",
                                       "santodomingo", provincename.nsp)) %>%
        
        ## Further alterations need to be made for matching. banosdeaguasanta
        ## is referred to as banos in the code data set. This is its common
        ## name according to wikipedia:
        ## https://en.wikipedia.org/wiki/Ba%C3%B1os_de_Agua_Santa. Additionally,
        ## two names were changed during data collection, "pelileo" which will 
        ## be returned to "sanpedrodepelileo" and "shushufindicentral" which 
        ## will be returned to "shushufindi". Finally yantzaza had its name
        ## repeated between brackets for a part of the data collection process.
        ## This must be removed as well. 
        mutate(provincename.nsp = ifelse(cantonname.nsp == "laconcordia",
                                         "esmeraldas", provincename.nsp)) %>%
        mutate(cantonname.nsp = gsub("banosdeaguasanta", "banos", 
                                       cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("shushufindicentral", "shushufindi", 
                                     cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("^pelileo", "sanpedrodepelileo",
                                     cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("\\(yanzatza\\)", "", cantonname.nsp)) %>%
        
        mutate(match = paste0(provincename.nsp, cantonname.nsp)) %>%
        left_join(mergeset, by = "match") %>%
        select(c(20, 3:15)) %>%
        mutate(accepted.offers.inflow.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.inflow.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.inflow.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.inflow.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.inflow.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.inflow.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.inflow.2018 = rowSums(.[14])) %>%
        select(-c(2:14)) %>%
        group_by(cantoncode) %>%
        summarise_all(list(sum))

        ## No NA values were created. 
        sum(is.na(places.canton.uni))

## METHOD: merge datasets
places.canton <- merge(places.canton.uni, places.canton.tec,
                       by = "cantoncode", all.x = TRUE, all.y = TRUE,
                       suffixes = c(".uni", ".tec"))

        ## METHOD: replace the newly created NA values with 0, as their omission 
        ## in the respective data sets indicates that there are no uni or 
        ## technical school places available. 
        places.canton[is.na(places.canton)] <- 0

        ## METHOD: Create a new variable with total higher education places 
        ## available per year.
        df.inflow <- places.canton %>%
        mutate(accepted.offers.inflow.total.2012 = rowSums(.[,c(2,9)])) %>%
        mutate(accepted.offers.inflow.total.2013 = rowSums(.[,c(3,10)])) %>%
        mutate(accepted.offers.inflow.total.2014 = rowSums(.[,c(4,11)])) %>%
        mutate(accepted.offers.inflow.total.2015 = rowSums(.[,c(5,12)])) %>%
        mutate(accepted.offers.inflow.total.2016 = rowSums(.[,c(6,13)])) %>%
        mutate(accepted.offers.inflow.total.2017 = rowSums(.[,c(7,14)])) %>%
        mutate(accepted.offers.inflow.total.2018 = rowSums(.[,c(8,15)])) 

        remove(dftec, dfuni, places.canton, places.canton.tec, 
               places.canton.uni)

## ----------------------------------------------------------------##
## GOAL: create a dataframe with the number of accepted places per canton
## of higher education institution.

## METHOD: remove first line and column, as they contain no data. Additionally
## remove province data as we already have cantons matched with provinces. 

places.canton.uni2 <- dfuni2 %>%
        filter(!(Tipo.de.IES %in% "Total")) %>%
        select(-Tipo.de.IES) %>%
        mutate(provincename = na.locf(Provincia.del.Campus)) %>%
        mutate(provincename.nsp = tolower(gsub(" ", "", provincename))) %>%
        mutate(provincename.nsp = replace_special_char(provincename.nsp)) %>%
        mutate(cantonname.nsp = tolower(gsub(" ", "", Cantón.del.Campus))) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("distritometropolitanodequito",
                                     "quito", cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("lalibertad", "libertad", 
                                     cantonname.nsp)) %>%
        mutate(provincename.nsp = gsub("santodomingodelostsachilas",
                                       "santodomingo", provincename.nsp)) %>%
        mutate(provincename.nsp = ifelse(cantonname.nsp == "laconcordia",
                                        "esmeraldas", provincename.nsp)) %>%
        mutate(cantonname.nsp = gsub("banosdeaguasanta", "banos",
                                     cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("^pelileo", "sanpedrodepelileo",
                                     cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("^pillaro", "santiagodepillaro",
                                     cantonname.nsp)) %>%
        
        ## Remove the addition of jujan from alfredobaquerizomoreno(jujan) as it 
        ## is pointing to that canton (there is no entry without jujan).
        ## Remove tola from carlosjulioarosemena for the same reason. Change
        ## provinces placeolder zonaenestudio to zonasnodelimitadas in line with
        ## df0. 
        mutate(cantonname.nsp = gsub("\\(jujan\\)", "", cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("carlosjulioarosemenatola", 
                                     "carlosjulioarosemena", 
                                     cantonname.nsp)) %>%
        mutate(provincename.nsp = gsub("zonaenestudio", "zonasnodelimitadas",
                                       provincename.nsp)) %>%
        mutate(match = paste0(provincename.nsp, cantonname.nsp)) %>%
        left_join(mergeset, by = "match") %>%
        select(c(20, 3:15)) %>%
        mutate(accepted.offers.outflow.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.outflow.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.outflow.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.outflow.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.outflow.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.outflow.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.outflow.2018 = rowSums(.[14])) %>%
        select(-c(2:14)) %>%
        group_by(cantoncode) %>%
        summarise_all(list(sum)) 

        ## No NA values were created. 
        sum(is.na(places.canton.uni2))

## METHOD: clean data set of tecnological schools in the same manner.
names(dftec2) <- names(dfuni2)
places.canton.tec2 <- dftec2 %>%
        select(-Tipo.de.IES) %>%
        mutate(provincename = na.locf(Provincia.del.Campus)) %>%
        mutate(provincename.nsp = tolower(gsub(" ", "", provincename))) %>%
        mutate(provincename.nsp = replace_special_char(provincename.nsp)) %>%
        mutate(cantonname.nsp = tolower(gsub(" ", "", Cantón.del.Campus))) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("distritometropolitanodequito",
                                     "quito", cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("lalibertad", "libertad", 
                                     cantonname.nsp)) %>%
        mutate(provincename.nsp = gsub("santodomingodelostsachilas",
                                       "santodomingo", provincename.nsp)) %>%
        mutate(provincename.nsp = ifelse(cantonname.nsp == "laconcordia",
                                         "esmeraldas", provincename.nsp)) %>%
        mutate(cantonname.nsp = gsub("banosdeaguasanta", "banos",
                                     cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("^pelileo", "sanpedrodepelileo",
                                     cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("^pillaro", "santiagodepillaro",
                                     cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("\\(jujan\\)", "", cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("carlosjulioarosemenatola", 
                                     "carlosjulioarosemena", 
                                     cantonname.nsp)) %>%
        mutate(provincename.nsp = gsub("zonaenestudio", "zonasnodelimitadas",
                                       provincename.nsp)) %>%
        mutate(match = paste0(provincename.nsp, cantonname.nsp)) %>%
        left_join(mergeset, by = "match") %>%
        select(c(20, 3:15)) %>%
        mutate(accepted.offers.outflow.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.outflow.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.outflow.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.outflow.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.outflow.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.outflow.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.outflow.2018 = rowSums(.[14])) %>%
        select(-c(2:14)) %>%
        group_by(cantoncode) %>%
        summarise_all(list(sum))  

        ## No NA values were created. 
        sum(is.na(places.canton.tec2))

## METHOD: merge datasets
places.canton <- merge(places.canton.uni2, places.canton.tec2,
                       by = "cantoncode", all.x = TRUE, all.y = TRUE,
                       suffixes = c(".uni", ".tec"))

## METHOD: replace the newly created NA values with 0, as their omission 
## in the respective data sets indicates that there are no uni or 
## technical school places available. 
places.canton[is.na(places.canton)] <- 0

## METHOD: Create a new variable with total higher education places 
## available per year.
df.outflow <- places.canton %>%
        mutate(accepted.offers.outflow.total.2012 = rowSums(.[,c(2,9)])) %>%
        mutate(accepted.offers.outflow.total.2013 = rowSums(.[,c(3,10)])) %>%
        mutate(accepted.offers.outflow.total.2014 = rowSums(.[,c(4,11)])) %>%
        mutate(accepted.offers.outflow.total.2015 = rowSums(.[,c(5,12)])) %>%
        mutate(accepted.offers.outflow.total.2016 = rowSums(.[,c(6,13)])) %>%
        mutate(accepted.offers.outflow.total.2017 = rowSums(.[,c(7,14)])) %>%
        mutate(accepted.offers.outflow.total.2018 = rowSums(.[,c(8,15)])) 

remove(dftec2, dfuni2, places.canton, places.canton.tec2, 
       places.canton.uni2)
        

## ----------------------------------------------------------------##
## GOAL: create a dataframe with population per canton.

dat0 <- read.csv("population.csv", skip = 10)
dat1 <- dat0

## The dataset is a pivotted table, so to access certain data subsetting rows is 
## necessary. In the Nombre.de.la.Parroquia column the value "Total" indicates 
## the summed population for that particular canton (where the name of the 
## canton can be found in the Nombre.delCantón variable). For each of these 
## summations, there are 3 values in the ÁREA column ("Rural", "Urban" and 
## "Total"). The row with the "Total" value gives the total population for each 
## age group. Accordingly, we can extract the population for each canton by 
## subsetting the "Total" values of the Nombre.de.la.Parroquia variable and the 
## "Total" values of the Área variables. 

## Another further three operations need to be performed. Firstly, the pivoted
## values are only filled in for the first row of the grouping. Accordingly,
## those values need to be filled (otherwise empty values will be subsetted). 
## Next, 'Total' values in the Nombre del Cantón set need to be removed, as
## These sum the population of sets of cantons (on provincial level).

## Secondly, there are 'Total' values in the Nombre.del.Cantón which initiate
## a section with all parroquias in a province. These must be removed from the
## data, as they are simply a repetition of the data at individual canton level.

## Thirdly, the entire table has been duplicated. One reason for this could
## be to have an alphabetically and non-alphabetically ordered version (the 
## second set is ordered alphabetically by canton). We can must thus subset
## only one of the two data sets. Testing for this duplication and for whether
## the values between the two duplicates match and in which rows in the 
## subsetted data set, the first set of values can be found (row 0-250). The
## test for this can be found in the apendix.

## Finally, the cantonnames should be cleared up for special characters to
## allow the data set to be merged. For this we can use the 
## replace_special_char function. 

## OPERATION 1: fill pivoted missing values.
dat1[dat1 == ""] <- NA
dat1$Nombre.del.Cantón[2:length(dat1$Nombre.del.Cantón)] <- 
        na.locf(dat1$Nombre.del.Cantón)
dat1$Nombre.de.la.Parroquia[2:length(dat1$Nombre.de.la.Parroquia)] <-
        na.locf(dat1$Nombre.de.la.Parroquia)
dat1$Provincia[2:length(dat1$Provincia)] <- na.locf(dat1$Provincia)

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
        select(cantonname, young.adult.population.2010, total.population.2010,
               Provincia)

## OPERATION 2: Remove 'Total' values in the cantonname variable.
dat3 <- dat2 %>%
        filter(cantonname != 'Total')

## OPERATION 2: Rows 1 to 224 have been subsetted. 
dat4 <- dat3[0:224,]

## OPERATION 3: replace cantonnames with codes.  
df.population <- dat4 %>%
        mutate(cantonname.nsp = tolower(gsub(" ", "", cantonname))) %>%
        mutate(cantonname.nsp = replace_special_char(cantonname.nsp)) %>%
        mutate(provincename.nsp = tolower(gsub(" ", "", Provincia))) %>%
        mutate(provincename.nsp = replace_special_char(provincename.nsp)) %>%
        
        ## Make name changes for matching.
        mutate(cantonname.nsp = gsub("sanjosedechimbo", "chimbo",
                                     cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("pablovi", "pablosexto",
                                     cantonname.nsp)) %>%
        mutate(cantonname.nsp = gsub("delostsachilas", "",
                                       cantonname.nsp)) %>% 
        
        # Merge with mergeset. 
        mutate(match = paste0(provincename.nsp, cantonname.nsp)) %>%
        left_join(mergeset, by = "match") %>%
        select(cantoncode, young.adult.population.2010,
               total.population.2010)

remove(dat0, dat1, dat2, dat3, dat4)

## ----------------------------------------------------------------##
## GOAL: merge the dataframes.

names <- df0 %>%
        select(cantonname, cantonname.nsp, provincename, provincename.nsp,
               cantoncode)

df.merged <- df.population %>%
        left_join(df.outflow, by = "cantoncode") %>%
        left_join(df.inflow, by = "cantoncode") %>%
        left_join(names, by = "cantoncode")

df.merged[is.na(df.merged)] <- 0

## Test set: mocache = 3,076 + 2,640; cayambe = 7,801 + 7,413; 
## mangadelcura = 1,574	1,486.
unique(df.population$young.adult.population.2010[
        df.population$cantonname.nsp=="mocache"])
unique(df.population$young.adult.population.2010[
        df.population$cantonname.nsp=="cayambe"])
unique(df.population$young.adult.population.2010[
        df.population$cantonname.nsp=="mangadelcura"])

