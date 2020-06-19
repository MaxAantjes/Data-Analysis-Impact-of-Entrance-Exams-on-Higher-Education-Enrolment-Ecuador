## This module cleans and tidies the data. 


## Load packages and data set. 

library(pacman)
pacman::p_load(dplyr, zoo, lubridate)

dat0 <- readRDS("raw_data.rds")
postcodes <- readRDS("postcodes.rds")


## ----------------------------------------------------------------##
## GOAL: create dataframe of multiple dataframes by selecting
## the columns of interest.

## METHOD: eradicate lower/uppercase differences between dataframes.
## create and run function. 

namestolower <- function(x) {
        
        names(x) <- tolower(names(x))
        
        return(x)
                        
}

dat0 <- lapply(dat0, namestolower)


## METHOD: create column of NA values if data not available in 
## particular survey.

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

dat1 <- lapply(dat0, addcolNA, pattern = "ciudad")

## METHOD: Select columns of interest and bind datasets. 

dat1 <- lapply(dat1, select, survey.location, ciudad, p03, p15,  
               p16a, p16b, p17a, p17b, p10a, p10b, p07, p12a,  
               date, p09, p18, p02)

dat2 <- do.call(rbind, dat1)
remove(dat0)

## ----------------------------------------------------------------##
## GOAL: Clarify column names. 

names(dat2) <- c("current.address.area", "current.address.postcode", "age", 
                 "ethnicity", "ever.moved", "time.at.current.address", 
                 "prior.address.abroad", "prior.address.postcode", 
                 "higher.education.level", "no.years.completed", 
                 "currently.matriculated","obtained.degree", "survey.date", 
                 "reason.not.matriculated", "reason.for.migration", "gender")


## GOAL: Specify variable class and levels for R. 

## METHOD: Specify factor variables according to surveys and codebook. 

dat2$current.address.area <- factor(dat2$current.address.area, level = c(1,2),
                               labels = c("urban", "rural"))

dat2$higher.education.level <- factor(dat2$higher.education.level, 
                                      levels = 1:10, labels = 
                                         c("none", "none", "none", "none",
                                           "none", "none", "none", 
                                           "technical.school",
                                           "undergraduate", "postgraduate"))

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


## Some surveys only had 14 levels for the following variable, meaning 'other'
## was classified as level 14. To merge the data appropriately, the newly 
## introduced levels are classified as 'other'. 

dat2$reason.not.matriculated <- factor(dat2$reason.not.matriculated, levels = 1:16,
                                       labels = c("age", "completed studies", "finances",
                                                  "poor academic performance", "for work", 
                                                  "leveling SENECYT",
                                                  "illness", "for house work",
                                                  "family prohibition", 
                                                  "lack of educational facilities",
                                                  "lack of interest", "embarrassment", 
                                                  "lack of places at educational facilities",
                                                  "other", "other", "other"))

dat2$reason.for.migration <- factor(dat2$reason.for.migration, levels = 1:8,
                                    labels = c("work", "increase salary",
                                               "marriage", "academic",
                                               "health", "property purchase",
                                               "family reunion", "other"))

dat2$gender <- factor(dat2$gender, levels = c(1,2), labels = c("male", "female"))

## METHOD: Transform survey date into a time variable. 

dat2 <- transform(dat2, survey.date = as.yearmon(survey.date))
remove(dat1)

## ----------------------------------------------------------------##
## GOAL: Clean up unnecessary NA values in data set (where the
## question was not asked as information was already given in
## prior questions). This allows us to see whether NA values
## are actually missing data. 

## METHOD: set time.at.current.address to age if ever.moved
## equals "no". Replace prior.address.postcode with current.address.postcode. 
## Replace reason.for.migration with "none". 

a <- dat2 %>%
        filter(ever.moved == "no") %>%
        mutate(time.at.current.address = age) %>%
        mutate(prior.address.postcode = current.address.postcode) %>%
        mutate(reason.for.migration = "none")

b <- dat2 %>%
        filter(ever.moved == "yes" | is.na(ever.moved))

dat3 <- rbind(a, b)

## METHOD: set reason.for.not.matriculated to "none if 
## currently.matriculated equals "yes".

a <- dat3 %>%
        filter(currently.matriculated == "yes") %>%
        mutate(reason.not.matriculated = "none")

b <- dat3 %>%
        filter(currently.matriculated == "no" | is.na(currently.matriculated))

dat3 <- rbind(a, b)

## METHOD: set prior.address.abraod to "no" if ever.moved
## equals "yes". 

dat3$prior.address.abroad[dat3$ever.moved == "no"] <- "no"

## METHOD: set prior.address.abraod to "no" if higher.education.level
## equals "none". 

dat3$obtained.degree[dat3$higher.education.level == "none"] <- "no"
remove(a)
remove(b)

## ----------------------------------------------------------------##
## GOAL: Create new indicating the area of prior address and the 
## region of current and prior address.

## METHOD: Create a function indicating the area of the postcode
## relying on the postcode dataframe created in module 1.
## This function will return NA values for individuals who live abroad.
## This makes sense, because we cannot determine whether these 
## individuals have migrated from urban or rural areas. 

area <- function(x, y = postcodes) {
        
        if (is.na(x) == TRUE) {
                
                c <- NA
                
        } else {
                
                c <-  y$area[y$codes == x]
                
                if(identical(c, character(0))) {
                        
                        c <- NA
                }
                
        }
        
        return(c)
}

## METHOD: run function on dataframe to create the 
## prior.address.area variable. 

dat3$prior.address.area <- lapply(dat3$prior.address.postcode, 
                                 area)
dat3$prior.address.area <- factor(dat3$prior.address.area, 
                                  levels = c(1,2),
                                  labels = c("urban", "rural"))

## METHOD: Create a similar function indicating the region of the postcode
## relying on the postcode dataframe created in module 1.
## This function will return NA values for individuals who live abroad.
## This makes sense, because we cannot determine whether these 
## individuals have migrated from urban or rural areas. 

region <- function(x, y = postcodes) {
        
        if (is.na(x) == TRUE) {
                
                c <- NA
                
        } else {
                
                c <-  y$region[y$codes == x]
                
                if(identical(c, character(0))) {
                        
                        c <- NA
                }
                
        }
        
        return(c)
}

## METHOD: run function on dataframe to create the 
## current.address.region variable. 

dat3$current.address.region <- lapply(dat3$current.address.postcode, 
                                  region)
dat3$current.address.region <- factor(dat3$current.address.region, 
                                  levels = 1:2,
                                  labels = c("highlands", "coast"))

## METHOD: run function on dataframe to create the 
## prior.address.region variable. 

dat3$prior.address.region <- lapply(dat3$prior.address.postcode, 
                                      region)
dat3$prior.address.region <- factor(dat3$prior.address.region, 
                                      levels = 1:2,
                                      labels = c("highlands", "coast"))

remove(dat2)

dat4 <- dat3 %>%
        mutate(estimated.final.highschool.year = survey.date - (age-18))

b <- interval(as.Date("2016-09-30"), as.Date("2017-09-30"))
c <- interval(as.Date("2017-09-30"), as.Date("2018-09-30"))
a <- as.Date("2013-12-30")
a %within% b

y <- c(c,b)

## Create sequence
seq(from = as.Date("1995-09-09"), to = as.Date("2017-09-09"), by = "years")

## Combine sequences in interval
coast.int.start <- seq(from = as.Date("2007-05-01"), to = as.Date("2019-05-01"), by = "years")
coast.int.end <- seq(from = as.Date("2008-04-30"), to = as.Date("2020-04-30"), by = "years")

## Function (create intervals)


## Function
intreplace <- function(x, y) {
        
        i <- 1
        
        while(!(x %within% y[i])) {
               
                i <- i+1
                
        if(is.na(y[i])) {
                
                break
        }
                }
    
        x <- y[i] 
        return(x)
}

d <- intreplace(a, y)


## ----------------------------------------------------------------##
## GOAL: Reorder variables in an intuitive way. 

dat3 <- dat3[, c(13, 16, 3, 4, 5, 6, 2, 1, 7, 4, 15, 8, 9, 10, 14, 11, 12)]


                                   
## Save data
saveRDS(dat2, "modified_data_1.csv")
rm(list = ls())

        

