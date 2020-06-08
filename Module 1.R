## This module cleans and tidies the data. 

## Load packages and data set. 
## run install.packages("pacman") if you do not have this package.

library(pacman)
pacman::p_load(dplyr)
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".zip")
download.file("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Septiembre/BDD_DATOS_ABIERTOS_ENEMDU_%202019_09_CSV.zip",temp)
unzip(temp, exdir=td, overwrite=TRUE)
dat0 <- read.csv(file.path(td, "BDD_DATOS_ABIERTOS_ENEMDU_ 2019_09_CSV/enemdu_personas_2019_09.csv"), sep =';')
unlink(temp)


## Select columns of interest (location of origin, migration, higher education 
## level and age).Select rows of interest(age from 20 to 25)

dat1 <- dat0 %>%
        select(ciudad, p03, p15, p15aa, p15ab, p16a, p16b, p17a, p17b, p10a, 
               p10b, p07, p12a, p12b) %>%
        filter(p03 > 17 & p03 < 25) 


## Tidy columnnames. Create factor variables in line with codebook. 

names(dat1) <- c("current.address", "age", "ethnicity", 
                 "born.at.current.address", "place.of.birth", "ever.moved", 
                 "time.at.current.address", "prior.address.abroad", 
                 "location.prior.address", "higher.education.level", 
                 "no.years.completed", "currently.matriculated",
                 "obtained.degree", "field.of.degree")

dat1$higher.education.level <- factor(dat1$higher.education.level, 
                                      levels = 1:10, labels = 
                                         c("none", "none", "none", "none",
                                           "none", "none", "none", 
                                           "technical.school",
                                           "undergraduate", "postgraduate"))
dat1$born.at.current.address <- factor(dat1$born.at.current.address, labels =
                       c("yes", "no.as.national", "no.as.international"))
dat1$obtained.degree <- factor(dat1$obtained.degree, levels = c(1,2), labels =
                                       c("yes", "no"))
dat1$ever.moved <- factor(dat1$ever.moved, levels = c(1,2),
                                    labels = c("no", "yes"))
dat1$prior.address.abroad <- factor(dat1$prior.address.abroad, levels = 
                                         c(1,2), labels = c("no","yes"))
dat1$ethnicity <- factor(dat1$ethnicity, levels = 1:8, labels = c("indigenous",
                                "afroecuadorian", "black", "mulatto",
                                "montubio", "mestizo", "white", "other"))
dat1$currently.matriculated <- factor(dat1$currently.matriculated, levels =
                                              c(1,2), labels = c("yes", "no"))


## Select rows where place.of.birth with survey.location is the same as 
## place.of.birth. Replace place.of.birth with survey.location. 

dat2 <- dat1 %>%
        filter(born.at.current.address == "yes") %>%
        mutate(place.of.birth = current.address) %>%
        select(-born.at.current.address)


## Now we merge the data sets again and exclude foreign born 
## Ecuadorians.

dat3 <- dat1 %>%
        filter(born.at.current.address == "no.as.national") %>%
        select(-born.at.current.address) %>%
        rbind(dat2)


## Save data
saveRDS(dat3, "clean_data")
        

