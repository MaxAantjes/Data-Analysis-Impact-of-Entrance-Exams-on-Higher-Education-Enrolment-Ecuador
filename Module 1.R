## This module cleans and tidies the data. 

## Load packages and data set. 
library(tidyr)
library(dplyr)
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".zip")
download.file("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Septiembre/BDD_DATOS_ABIERTOS_ENEMDU_%202019_09_CSV.zip",temp)
unzip(temp, exdir=td, overwrite=TRUE)
dat0 <- read.csv(file.path(td, "BDD_DATOS_ABIERTOS_ENEMDU_ 2019_09_CSV/enemdu_personas_2019_09.csv"), sep =';')


## Select columns of interest (location of origin, migration, higher education 
## level and age).Select rows of interest(age from 20 to 25)

dat1 <- dat0 %>%
        select(ciudad, p03, p15aa, p15ab, p16a, p16b, p17a, p17b, p10a, p10b, 
               p12a, p12b) %>%
        filter(p03 > 20 & p03 < 25) 


## Tidy columnnames. Create factor variables in line with codebook. 

names(dat1) <- c("survey.location", "age", "born.in.survey.location", 
                 "place.of.birth", "ever.moved", 
                 "time.in.current.home", "prior.home.abroad", 
                 "location.prior.home", "higher.education.level", 
                 "no.years.completed", "obtained.degree", "field.of.degree")

dat1$higher.education.level <- factor(dat1$higher.education.level, 
                                      levels = 1:10, labels = 
                                         c("none", "none", "none", "none",
                                           "none", "none", "none", 
                                           "technical.school",
                                           "undergraduate", "postgraduate"))
dat1$born.in.survey.location <- factor(dat1$born.in.survey.location, labels =
                       c("yes", "no.as.national", "no.as.international"))
dat1$obtained.degree <- factor(dat1$obtained.degree, levels = c(1,2), labels =
                                       c("yes", "no"))
dat1$ever.moved <- factor(dat1$ever.moved, levels = c(1,2),
                                    labels = c("no", "yes"))
dat1$prior.home.abroad <- factor(dat1$prior.home.abroad, levels = 
                                         c(1,2), labels = c("no","yes"))


## Select rows where place.of.birth with survey.location is the same as 
## place.of.birth. Replace place.of.birth with survey.location. 

dat2 <- dat1 %>%
        filter(born.in.survey.location == "yes") %>%
        mutate(place.of.birth = survey.location) %>%
        select(-c(born.in.survey.location, survey.location))


## Now we merge the data sets again and exclude foreign born Ecuadorians.
dat3 <- dat1 %>%
        filter(born.in.survey.location == "no.as.national") %>%
        select(-c(born.in.survey.location, survey.location)) %>%
        rbind(dat2)

## Save data
saveRDS(dat3, "clean.data")
        

