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
               date, p09, p02)

dat2 <- do.call(rbind, dat1)
remove(dat0)

## ----------------------------------------------------------------##
## GOAL: Clarify column names. 

names(dat2) <- c("current.address.area", "current.address.postcode", "age", 
                 "ethnicity", "ever.moved", "time.at.current.address", 
                 "prior.address.abroad", "prior.address.postcode", 
                 "higher.education.level", "no.years.completed", 
                 "currently.matriculated","obtained.degree", "survey.date", 
                 "reason.not.matriculated", "gender")


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
        mutate(prior.address.postcode = current.address.postcode) 

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


remove(dat2)


## ----------------------------------------------------------------##
## GOAL: Create variables indicating information about the respondent during
## the estimated period he/she was attending the final year of highschool, 
## including postcode and date. 

## METHOD: Create a variable with the date and year the respondent was 18. 
## Remove respondents who are not estimated to have been final year 
## highschoolers between 2007 and 2020 (as there is no data outside of this
## range of our variables of interest. 

dat4 <- dat3 %>%
        filter(age > 17) %>%
        mutate(estimated.final.highschool.year = survey.date - (age-18))

lowerlim <- as.yearmon("2007-01")
upperlim <- as.yearmon("2020-01")

dat4 <- dat4[dat4$estimated.final.highschool.year > lowerlim & 
                     dat4$estimated.final.highschool.year < upperlim, ]


## METHOD: Determine the location of respondents during their
## estimated final school year. Remove current.address and 
## prior.address variables as they are no longer of interest

dat4 <- dat4 %>%
        mutate(postcode.estimated.final.highschool.year =
                       ifelse(ever.moved == "no" | ever.moved == "yes"
                              & time.at.current.address > 
                                      (age - (difftime(survey.date, 
                                                               estimated.final.highschool.year,
                                                       unit = "weeks"))/52.25), 
                              current.address.postcode, prior.address.postcode)) %>%
        select(-c(current.address.postcode, current.address.area,
                  prior.address.postcode, prior.address.abroad, ever.moved))


## ----------------------------------------------------------------##
## GOAL: Create new variable indicating the area and the region of the postcodes.

## METHOD: Create a function indicating the area of the postcode
## relying on the postcode dataframe created in module 1.
## This function will return NA values for individuals who lived abroad.
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

## METHOD: run function on dataframe to create area variable indicating area. 

dat4$area.estimated.final.highschool.year <- 
        factor(lapply(dat4$postcode.estimated.final.highschool.year, area),
                                  levels = c(1,2),
                                  labels = c("urban", "rural"))

## METHOD: Create a similar function indicating the region of the postcode
## relying on the postcode dataframe created in module 1.
## This function will also return NA values for individuals who lived abroad.

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

## METHOD: run function on dataframe to create area variable indicating region. 

dat4$region.estimated.final.highschool.year <- 
        factor(lapply(dat4$postcode.estimated.final.highschool.year, area),
               levels = c(1,2),
               labels = c("highlands", "coast"))


## ----------------------------------------------------------------##
## GOAL: Transform the estimated date respondents were attending their
## final year in highschool into an interval variable indicating the
## academic year. 

## METHOD: Create sequences of the start and end of schoolyears in coastal
## and urban areas (they differ between regions in Ecuador). 

coast.int.start <- seq(from = as.Date("2007-05-01"), 
                       to = as.Date("2019-05-01"), by = "years")

coast.int.end <- seq(from = as.Date("2008-04-30"), 
                     to = as.Date("2020-04-30"), by = "years")

hland.int.start <- seq(from = as.Date("2007-09-01"),
                       to = as.Date("2019-09-01"), by = "years")

hland.int.end <- seq(from = as.Date("2008-08-31"), 
                     to = as.Date("2020-08-31"), by = "years")

## METHOD: Create a function which turns two vectors of start and end dates 
## into intervals. 

intcreate <- function(x, y) {
        
        a <- c(interval())
        
        for(i in 1:length(x)) {
                
                a[i] <- interval(as.Date(x[i]),as.Date(y[i]))
                
        }
        
        return(a)
        
}

## METHOD: Run vectors through function to create a list of intervals. 

coast.int <- intcreate(coast.int.start, coast.int.end)
hland.int <- intcreate(hland.int.start, hland.int.end)
remove(coast.int.start, coast.int.end, hland.int.end, hland.int.start)

## METHOD: Create function which replaces dates and years
## with the interval they belong in. 

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




## ----------------------------------------------------------------##
## GOAL: Reorder variables in an intuitive way. 

dat3 <- dat3[, c(13, 16, 3, 4, 5, 6, 2, 1, 7, 4, 15, 8, 9, 10, 14, 11, 12)]


                                   
## Save data
saveRDS(dat2, "modified_data_1.csv")
rm(list = ls())

        

