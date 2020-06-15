## This module cleans and tidies the data. 


## Load packages and data set. 

library(pacman)
pacman::p_load(dplyr, zoo)

dat0A <- readRDS("raw_data_1")
dat0B <- readRDS("raw_data_2")
dat0 <- c(dat0A, dat0B)

## GOAL: create dataframe of multiple dataframes by selecting
## the columns of interest they have in common.

## METHOD: eradicate lower/uppercase differences between dataframes.
## create and run function. 

namestolower <- function(x) {
        
        names(x) <- tolower(names(x))
        
        return(x)
                        
}

dat0 <- lapply(dat0, namestolower)


## METHOD: create column of NA values if question not asked in 
## survey.

addcolNA <- function(x, pattern) {
        
        a <- names(x)
        b <- length(x) + 1
        
        if(sum(grepl(pattern, a)) < 1) {
                
                x <- x %>%
                        mutate(new_col = NA) 
                
                names(x)[b] <- pattern
                
        }
        
        return(x)       
        
}

dat1 <- lapply(dat0, addcolNA, pattern = "p15aa")
dat1 <- lapply(dat1, addcolNA, pattern = "p15ab")

## METHOD: Select columns of interest and bind datasets. 

dat1 <- lapply(dat1, select, survey.location, p03, p15, p15aa, p15ab, 
               p16a, p16b, p17a, p17b, p10a, p10b, p07, p12a, p12b, 
               date, p09, p18)

dat2 <- do.call(rbind, dat1)


## GOAL: Tidy columnnames. Create factor variables in line with codebook. 

names(dat2) <- c("current.address", "age", "ethnicity", 
                 "born.at.current.address", "place.of.birth", "ever.moved", 
                 "time.at.current.address", "prior.address.abroad", 
                 "location.prior.address", "higher.education.level", 
                 "no.years.completed", "currently.matriculated",
                 "obtained.degree", "field.of.degree", "survey.date",
                 "reason.not.matriculated", "reason.for.migration")


## GOAL: Transform character variables into factor variables where relevant. 

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

dat2$ever.moved <- factor(dat2$ever.moved, levels = c(2,1),
                                    labels = c("yes", "no"))

dat2$prior.address.abroad <- factor(dat2$prior.address.abroad, levels = 
                                         c(1,2), labels = c("no","yes"))
dat2$ethnicity <- factor(dat2$ethnicity, levels = 1:8, labels = c("indigenous",
                                "afroecuadorian", "black", "mulatto",
                                "montubio", "mestizo", "white", "other"))

dat2$currently.matriculated <- factor(dat2$currently.matriculated, levels =
                                              c(1,2), labels = c("yes", "no"))

dat2$reason.not.matriculated <- factor(dat2$reason.not.matriculated, levels = 1:16,
                                       labels = c("age", "completed", "finances",
                                                  "poor performance", "for work", "leveling SENECYT",
                                                  "illness", "for house maintenance",
                                                  "family prohibition", "lack of educational facilities",
                                                  "lack of interest", "embarrassment", "no space",
                                                  "peer pressure", "to take care of children", 
                                                  "other"))
dat2$reason.for.migration <- factor(dat2$reason.for.migration, levels = 1:8,
                                    labels = c("work", "increase salary",
                                               "marriage", "academic",
                                               "health", "property purchase",
                                               "family reunion", "other"))
dat2 <- transform(dat2, survey.date = as.yearmon(survey.date))
                                    
## Save data
saveRDS(dat2, "modified_data_1.csv")
rm(list = ls())

        

