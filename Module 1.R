## This module cleans and tidies the survey data. 


## ----------------------------------------------------------------##
## GOAL: load packages and data set. 
library(pacman)
pacman::p_load(dplyr, lubridate)

dat0 <- readRDS("raw_data_survey.rds")


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


## METHOD: test which data sets have all the variables of interest (some of 
## the data sets have missing columns). 

checkcols <- function(x, y) {
        
        incomplete.columns <- c()
        
        counter <- 0
        
        for(j in 1:length(x)) {
                
                for(i in 1:length(y)) {
                        
                        if(sum(
                                
                                grep(y[i], names(x[[j]])))
                           
                           == 0)
                                
                        {
                          
                                message(paste("Column", y[i],
                                            "missing in survey from", 
                                            x[j][[1]]$date[1], 
                                            "in list position",
                                      j))
                                
                                counter <- counter + 1
                                
                                incomplete.columns[counter] <- j
                                
                        } 
                        
                }
                
        
        }
        
        return(incomplete.columns)
        
}

colnames <- c("\\<ciudad\\>", "\\<p03\\>", "\\<p15\\>", "\\<p16a\\>", 
              "\\<p16b\\>", "\\<p17a\\>", "\\<p17b\\>", "\\<p10a\\>", 
              "\\<p10b\\>", "\\<p07\\>", "\\<p12a\\>", "\\<date\\>", 
              "\\<p09\\>", "\\<p02\\>")


position.inc.df <- checkcols(dat0, colnames)


## METHOD: Select columns of interest and bind datasets. 

dat1 <- lapply(dat0[-position.inc.df], select, survey.location, ciudad, p03, 
               p15, p16a, p16b, p17a, p17b, p10a, p10b, p07, p12a,  
               date, p09, p02)

dat1 <- do.call(rbind, dat1)


## METHOD: Clarify column names. 

names(dat1) <- c("current.address.area", "current.address.postcode", "age", 
                 "ethnicity", "ever.moved", "time.at.current.address", 
                 "prior.address.abroad", "prior.address.postcode", 
                 "education.level", "years.completed.highest.education.level", 
                 "currently.matriculated","obtained.degree", "survey.date", 
                 "reason.not.matriculated", "gender")

remove(dat0)


## ----------------------------------------------------------------##
## GOAL: Specify variable class and levels for R. 

## METHOD: Specify factor variables according to surveys and codebook. Levels
## with values c(1,2) are converted to c(0,1) to make regression computations
## posible and clear. 

dat3 <- dat2 %>%
        mutate(current.address.area = ifelse(
                current.address.area == 1, 0, 1))

dat2$current.address.area <- ifelse(dat2$current.address.area, level = 1, 0, 1)

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

dat2$gender <- factor(dat2$gender, levels = c(1,2), labels = c("male", "female"))

## METHOD: Transform survey date into a time variable. 

dat2 <- transform(dat2, survey.date = as.Date(survey.date))
remove(dat1)

## ----------------------------------------------------------------##
## GOAL: Add a variable calculating the minimum total years enrolled in education. 
## First we assign the base years required for the highest level of education.
## This is calculated through the Sistema Anterior, Sistema Actual Reforma
## Curricular table in the 2019 survey.

dat3 <- dat2
dat3$years.in.education[dat3$education.level == 1] <- 0 
dat3$years.in.education[dat3$education.level == 2] <- 0
dat3$years.in.education[dat3$education.level == 3] <- 0
dat3$years.in.education[dat3$education.level == 4] <- 1
dat3$years.in.education[dat3$education.level == 5] <- 0
dat3$years.in.education[dat3$education.level == 6] <- 6
dat3$years.in.education[dat3$education.level == 7] <- 10
dat3$years.in.education[dat3$education.level == 8] <- 13
dat3$years.in.education[dat3$education.level == 9] <- 13
dat3$years.in.education[dat3$education.level == 10] <- 13

dat3$years.in.education <- dat3$years.in.education + dat3$years.completed.highest.education.level
remove(dat2)


## ----------------------------------------------------------------##
## GOAL: Clean up unnecessary NA values in data set (where the
## question was not asked as information was already given in
## prior questions). This allows us to see whether NA values
## are actually missing data. 

## METHOD: set time.at.current.address to age if ever.moved
## equals "no". Replace prior.address.postcode with current.address.postcode. 
## Replace reason.for.migration with "none". 

a <- dat3 %>%
        filter(ever.moved == "no") %>%
        mutate(time.at.current.address = age) %>%
        mutate(prior.address.postcode = current.address.postcode) 

b <- dat3 %>%
        filter(ever.moved == "yes" | is.na(ever.moved))

dat4 <- rbind(a, b)

## METHOD: set reason.for.not.matriculated to "none if 
## currently.matriculated equals "yes".

a <- dat4 %>%
        filter(currently.matriculated == "yes") %>%
        mutate(reason.not.matriculated = "none")

b <- dat4 %>%
        filter(currently.matriculated == "no" | is.na(currently.matriculated))

dat4 <- rbind(a, b)

## METHOD: set prior.address.abraod to "no" if ever.moved
## equals "yes". 

dat4$prior.address.abroad[dat4$ever.moved == "no"] <- "no"

## METHOD: set prior.address.abraod to "no" if education.level
## equals "none". 

dat4$obtained.degree[as.integer(dat4$education.level) < 8] <- "no"
remove(dat3, a, b)


## ----------------------------------------------------------------##
## GOAL: order columns in an intuitive way.

dat5 <- dat4[, c(13, 3, 15, 16, 4, 2, 1, 5, 6, 7, 8, 9, 10, 16, 11, 14, 12)]
remove(dat4)

## ----------------------------------------------------------------##
## Save data
saveRDS(dat5, "clean_data_survey.rds")
rm(list = ls())