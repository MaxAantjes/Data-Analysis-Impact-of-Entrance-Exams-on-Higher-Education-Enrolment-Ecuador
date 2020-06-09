## This module loads the raw data sets for different years and 
## periods. 

## Unfortunately, there is NO consistency in the data folders provided 
## by the Ecuadorian government. As such, filepaths for every individual
## download need to be specified. 

library(pacman)
pacman::p_load(dplyr)


## download and store file function.
load.files <- function(x) {
        
                td = tempdir()
                temp = tempfile(tmpdir=td, fileext=".zip")
                download.file(x[1],temp)
                unzip(temp, exdir=td, overwrite=TRUE)
                
                dat <- read.csv(file.path(td, x[2]), sep =';')
                dat <- mutate(dat, date = x[3])
                names(dat)[[1]] <- "survey.location"
                
                unlink(temp)
                
                return(dat)
}
   

## Create list of links to files, filepaths and dates per 
## dataframe. 

y2019sep <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Septiembre/BDD_DATOS_ABIERTOS_ENEMDU_%202019_09_CSV.zip",
              "BDD_DATOS_ABIERTOS_ENEMDU_ 2019_09_CSV/enemdu_personas_2019_09.csv",
              "201909")
y2019jun <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Junio/BDD_DATOS_ABIERTOS_ENEMDU_2019_06_CSV.zip",
              "BDD_DATOS_ABIERTOS_ENEMDU_ 2019_06_CSV/201906_EnemduBDD_15anios.csv",
              "201906")
y2019mar <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Marzo/BDD_DATOS_ABIERTOS_ENEMDU_%202019_03_CSV.zip", 
              "BDD_DATOS_ABIERTOS_ENEMDU_ 2019_03_CSV/201903_EnemduBDD_15anios.csv",
              "201903")
y2018dec <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2018/Diciembre-2018/BDD_DATOS_ABIERTOS_ENEMDU_%202018_12_CSV.zip",
              "BDD_DATOS_ABIERTOS_ENEMDU_ 2018_12_CSV/201812_enemdubdd_15anios.csv",
              "201812")
y2018sep <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2018/Septiembre-2018/BDD_DATOS_ABIERTOS_ENEMDU_2018_09_CSV.zip",
              "BDD_DATOS_ABIERTOS_ENEMDU_2018_09_CSV/201809_EnemduBDD_15anios.csv",
              "201809")
y2018jun <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2018/Junio-2018/BDD_DATOS_ABIERTOS_ENEMDU_2018_06_CSV.zip",
              "201806_EnemduBDD_15anios.csv",
              "201806")
y2018mar <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2018/Marzo-2018/BDD_DATOS_ABIERTOS_ENEMDU_2018_03_CSV.zip",
              "201803_EnemduBDD_15anios.csv",
              "201803")

links <- list(y2019sep, y2019jun, y2019mar, y2018dec, y2018sep,
              y2018jun, y2018mar)


## Create a list of dataframes. 

all.data <- lapply(links, load.files)


## save data. 

saveRDS(all.data, "raw_data")
rm(list = ls())
