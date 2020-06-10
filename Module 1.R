## This module cleans and tidies the data. 


## Load packages and data set. 
## run install.packages("pacman") if you do not have this package.

library(pacman)
pacman::p_load(dplyr)

dat0 <- readRDS("raw_data")


## running list() on dat0 reveals that the data sets have different variables.
## For this reason we will reduce each data frame to a set of variables which are available
## for each element data frame of the list and which are relevant for our analysis. 
## We select the following columns of interest (related to location of origin, 
## migration, higher education level and age).Select rows of interest(age from 20 to 25)
## Whether these variables were available and similar for each year and period can be
## verified by checking the used "formulario" in every respective raw data folder linked 
## in the codebook. 


dat1 <- lapply(dat0, select, survey.location, p03, p15, p15aa, p15ab, p16a, p16b, p17a, p17b, p10a, 
       p10b, p07, p12a, p12b, date)
dat2 <- rbind(dat1[[1]], dat1[[2]], dat1[[3]], dat1[[4]], dat1[[5]],
              dat1[[6]], dat1[[7]])

## Tidy columnnames. Create factor variables in line with codebook. 

names(dat2) <- c("current.address", "age", "ethnicity", 
                 "born.at.current.address", "place.of.birth", "ever.moved", 
                 "time.at.current.address", "prior.address.abroad", 
                 "location.prior.address", "higher.education.level", 
                 "no.years.completed", "currently.matriculated",
                 "obtained.degree", "field.of.degree", "survey.date")

dat2$current.address <- factor(dat2$current.address, level = c(1,2),
                               labels = c("urban", "rural"))

dat2$higher.education.level <- factor(dat2$higher.education.level, 
                                      levels = 1:10, labels = 
                                         c("none", "none", "none", "none",
                                           "none", "none", "none", 
                                           "technical.school",
                                           "undergraduate", "postgraduate"))

dat2$born.at.current.address <- factor(dat2$born.at.current.address, level =
                                        c(1,2,3), labels = c("yes", 
                                        "no.as.national", "no.as.international"))

dat2$obtained.degree <- factor(dat2$obtained.degree, levels = c(1,2), labels =
                                       c("yes", "no"))

dat2$ever.moved <- factor(dat2$ever.moved, levels = c(1,2),
                                    labels = c("no", "yes"))

dat2$prior.address.abroad <- factor(dat2$prior.address.abroad, levels = 
                                         c(1,2), labels = c("no","yes"))
dat2$ethnicity <- factor(dat2$ethnicity, levels = 1:8, labels = c("indigenous",
                                "afroecuadorian", "black", "mulatto",
                                "montubio", "mestizo", "white", "other"))

dat2$currently.matriculated <- factor(dat2$currently.matriculated, levels =
                                              c(1,2), labels = c("yes", "no"))



## Save data
saveRDS(dat2, "modified_data_1.csv")
rm(list = ls())

        

