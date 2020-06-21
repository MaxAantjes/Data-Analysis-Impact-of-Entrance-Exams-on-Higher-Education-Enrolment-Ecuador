## This module loads the raw data sets for different years and 
## periods. 


## ----------------------------------------------------------------##
## GOAL: load packages and data set. 
library(pacman)
pacman::p_load(dplyr, haven)


## ----------------------------------------------------------------##
## GOAL: download all data from September 2014 to September 2019 (excluding two data sets). 

## METHOD: create download and store file function.

load.files <- function(x) {
        
                td = tempdir()
                temp = tempfile(tmpdir=td, fileext=".zip")
                download.file(x[1],temp)
                unzip(temp, exdir=td, overwrite=TRUE)
                list <- tolower(list.files(td))
                
                if(isTRUE(grepl(".csv", x[2]))) {
                        
                dat <- read.csv(file.path(td, x[2]), sep =';')
                dat <- mutate(dat, date = x[3])
                names(dat)[[1]] <- "survey.location" 
                
                }
                
                else {
                        
                dat <- data.frame(read_sav(file.path(td, x[2])))
                dat <- mutate(dat, date = x[3])
                names(dat)[[1]] <- "survey.location"  
                
                }
                
                unlink(temp)
                
                return(dat)
                
}
   
## METHOD: create list of links to files, filepaths and dates per 
## dataframe. 

y2019sep <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Septiembre/BDD_DATOS_ABIERTOS_ENEMDU_%202019_09_CSV.zip",
              "BDD_DATOS_ABIERTOS_ENEMDU_ 2019_09_CSV/enemdu_personas_2019_09.csv",
              "2019-09-30")
y2019jun <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Junio/BDD_DATOS_ABIERTOS_ENEMDU_2019_06_CSV.zip",
              "BDD_DATOS_ABIERTOS_ENEMDU_ 2019_06_CSV/201906_EnemduBDD_15anios.csv",
              "2019-06-30")
y2019mar <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Marzo/BDD_DATOS_ABIERTOS_ENEMDU_%202019_03_CSV.zip", 
              "BDD_DATOS_ABIERTOS_ENEMDU_ 2019_03_CSV/201903_EnemduBDD_15anios.csv",
              "2019-03-31")
y2018dec <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2018/Diciembre-2018/BDD_DATOS_ABIERTOS_ENEMDU_%202018_12_CSV.zip",
              "BDD_DATOS_ABIERTOS_ENEMDU_ 2018_12_CSV/201812_enemdubdd_15anios.csv",
              "2018-12-31")
y2018sep <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2018/Septiembre-2018/BDD_DATOS_ABIERTOS_ENEMDU_2018_09_CSV.zip",
              "BDD_DATOS_ABIERTOS_ENEMDU_2018_09_CSV/201809_EnemduBDD_15anios.csv",
              "2018-09-30")
y2018jun <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2018/Junio-2018/BDD_DATOS_ABIERTOS_ENEMDU_2018_06_CSV.zip",
              "201806_EnemduBDD_15anios.csv",
              "2018-06-30")
y2018mar <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2018/Marzo-2018/BDD_DATOS_ABIERTOS_ENEMDU_2018_03_CSV.zip",
              "201803_EnemduBDD_15anios.csv",
              "2018-03-31")
y2017dec <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2017/Diciembre/BASES-DATOS-ENENDU_CSV.zip",
              "BASES_CSV/201712_EnemduBDD_15anios.csv",
              "2017-12-31")
y2017jun <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2017/Junio/062017_bddEnemdu_CSV.zip",
              "201706_EnemduBDD_CSV/201706_EnemduBDD_15anio.sav.csv",
              "2017-06-30")
y2017mar <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2017/Marzo/032017_bddEnemdu_CSV.zip",
              "032017_bddEnemdu_CSV/201703_EnemduBDD_CSV/201703_EnemduBDD_15anios.csv",
              "2017-03-31")
y2016dec <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2016/Diciembre-2016/122016_EnemduBDD_CVS.zip",
              "Bases CVS/122016_enemdubdd_completa.csv",
              "2016-12-31")
y2016sep <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2016/Septiembre-2016/BDD_ENEMDU_SEPTIEMBRE_2016_CSV.zip",
              "201609_EnemduBDD_per.csv",
              "2016-09-30")
y2016jun <- c("https://www.ecuadorencifras.gob.ec//documentos/web-inec/EMPLEO/2016/Junio-2016/BDD-ENEMDU-Junio2016.zip",
              "Publicadas/201606_EnemduBDD.SAV",
              "2016-06-30")
y2016mar <- c("https://www.ecuadorencifras.gob.ec//documentos/web-inec/EMPLEO/2016/Marzo-2016/BDD-ENEMDU-Marzo2016.zip",
              "201603_EnemduBDD.sav",
              "2016-03-31")
y2015dec <- c("https://www.ecuadorencifras.gob.ec//documentos/web-inec/EMPLEO/2015/Diciembre-2015/201512_EnemduBDD_publicar.zip",
              "201512_EnemduBDD_publicar/201512_EnemduBDD_15anios.sav",
              "2015-12-31")
y2015jun <- c("https://www.ecuadorencifras.gob.ec//documentos/web-inec/EMPLEO/2015/Junio-2015/201506_EnemduBDD.zip",
              "bdd_enemdu_15anios_06_2015/201506_EnemduBDD_15anios.sav",
              "2015-06-30")
y2015mar <- c("https://www.ecuadorencifras.gob.ec//documentos/web-inec/EMPLEO/2015/Marzo-2015/201503_EnemduBDD_15anios.zip",
              "201503_EnemduBDD_15anios.sav",
              "2015-03-31")
y2014dec <- c("https://www.ecuadorencifras.gob.ec//documentos/web-inec/EMPLEO/Empleo-Diciembre/Nuevo_Marco_Conceptual/201412_EnemduBDD_15anios.zip", 
              "201412_EnemduBDD_15anios.sav", 
              "2014-12-31")
y2014sep <- c("https://www.ecuadorencifras.gob.ec//documentos/web-inec/EMPLEO/Empleo-sep-2014/201409_EnenduBDD_15anios.zip",
              "201409_EnemduBDD_15anios.sav",
              "2014-09-30")


links <- list(y2019sep, y2019jun, y2019mar, y2018dec, y2018sep, y2018jun, y2018mar, 
              y2017dec, y2017jun, y2017mar, y2016dec, y2016sep, y2016jun, y2016mar,
              y2015dec, y2015jun, y2015mar, y2014dec, y2014sep)

## METHOD: run load.files function on list. 

all.data <- lapply(links, load.files)


## ----------------------------------------------------------------##
## GOAL: add two dataframes from September 2015 and September 2019
## which have zipfiles within zipfiles.

## METHOD: create a function which loads zipfiles within zipfiles

load.double.zip <- function(x) {
        
        td = tempdir()
        temp = tempfile(tmpdir=td, fileext=".zip")
        download.file(x[1],temp)
        unzip(temp, exdir=td, overwrite=TRUE)
        unzip(paste0(file.path(td), x[2]), exdir = td)
        
        dat <- read.csv(file.path(td, x[3]), sep =';')
        dat <- mutate(dat, date = x[4])
        names(dat)[[1]] <- "survey.location" 
        
        unlink(temp)
        
        return(dat)
        
}

## METHOD: create vectors with the information required for the 
## function. 

y2015sep <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2015/Septiembre-2015/201509_EnemduBDD_CSV.zip",
              "\\201509_EnemduBDD_CSV.zip",
              "\\201509_EnemduBDD_CSV\\201509_EnemduBDD_15anios.csv",
              "2015-09-30")

y2017sep <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2017/Septiembre/201709_EnemduBDD_CSV.zip",
              "\\bases csv\\201709_EnemduBDD_CSV.zip",
              "\\201709_EnemduBDD_CSV\\201709_EnemduBDD_15anios.csv",
              "2017-09-30")


## METHOD: run double.zip function on the vectors.

y2015sep <- load.double.zip(y2015sep)
y2017sep <- load.double.zip(y2017sep)


## METHOD: create function to add dataframes to the list. 

add.to.list <- function(element, list) {
        
        a <- length(list) + 1
        list[[a]] <- element
        return(list)
}

## METHOD: run function on dataframes. 

all.data.1 <- add.to.list(y2015sep, all.data)
all.data.1 <- add.to.list(y2017sep, all.data.1) 


## ----------------------------------------------------------------##
## GOAL: Load data frames from December 2007 to December 2014, which 
## are stored in a master zipfile document.

## METHOD: download master zipfile document in termporary directory.

url <- "https://educacion.gob.ec/wp-content/uploads/downloads/2017/06/BBDD_ENEMDU_Completas.zip" 
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".zip")
download.file(url,temp)
unzip(temp, exdir=td, overwrite=TRUE)

## METHOD: create function to extract files from subzipfiles.

unzip.in.td <- function(x) {
        
        unzip(paste0(file.path(td), x[1]), exdir = td)
        
        dat <- data.frame(read_sav(file.path(td, x[2])))
        dat <- mutate(dat, date = x[3])
        names(dat)[[1]] <- "survey.location" 
        
        return(dat)
        
}

## METHOD: create list to input in function.

y2007 <- c("\\BBDD_ENEMDU_Completas\\bdd_enemdu_15anios_12_2007.zip", 
           "\\bdd_enemdu_15anios_12_2007\\200712_EnemduBDD_15anios.sav",
           "2007-12-31")
y2008 <- c("\\BBDD_ENEMDU_Completas\\bdd_enemdu_15anios_12_2008.zip", 
           "\\bdd_enemdu_15anios_12_2008\\200812_EnemduBDD_15anios.sav",
           "2008-12-31")
y2009 <- c("\\BBDD_ENEMDU_Completas\\bdd_enemdu_15anios_12_2009.zip", 
           "\\bdd_enemdu_15anios_12_2009\\200912_EnemduBDD_15anios.sav",
           "2009-12-31")
y2010 <- c("\\BBDD_ENEMDU_Completas\\bdd_enemdu_15anios_12_2010.zip", 
           "\\bdd_enemdu_15anios_12_2010\\201012_EnemduBDD_15anios.sav",
           "2010-12-31")
y2011 <- c("\\BBDD_ENEMDU_Completas\\bdd_enemdu_15anios_12_2011.zip", 
           "\\bdd_enemdu_15anios_12_2011\\201112_EnemduBDD_15anios.sav",
           "2011-12-31")
y2012 <- c("\\BBDD_ENEMDU_Completas\\bdd_enemdu_15anios_12_2012.zip", 
           "\\bdd_enemdu_15anios_12_2012\\201212_EnemduBDD_15anios.sav",
           "2012-12-31")
y2013 <- c("\\BBDD_ENEMDU_Completas\\bdd_enemdu_15anios_12_2013.zip", 
           "\\bdd_enemdu_15anios_12_2013\\201312_EnemduBDD_15anios.sav",
           "2013-12-31")

## METHOD: Create a list of dataframes. 
list <- list(y2007, y2008, y2009, y2010, y2011, y2012, y2013)
all.data.2 <- lapply(list, unzip.in.td)
unlink(temp)


## ----------------------------------------------------------------##
## GOAL: Merge all data sets into one list:
dat0 <- c(all.data.1, all.data.2)
 
       
## ----------------------------------------------------------------##
## GOAL: save data. 
saveRDS(dat0, "raw_data_survey.rds")
rm(list = ls())

