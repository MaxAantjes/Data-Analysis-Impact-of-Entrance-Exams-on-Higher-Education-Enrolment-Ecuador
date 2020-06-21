## This module cleans and tidies the data. 


## Load packages and data set. 

library(pacman)
pacman::p_load(dplyr, lubridate)

dat0 <- readRDS("raw_data.rds")
df1 <- readRDS("tidy_data_studteachratio.rds")
df2 <- readRDS("tidy_data_regionarea.rds")


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

dat2 <- transform(dat2, survey.date = as.Date(survey.date))
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

dat4 <- dat3 %>%
        filter(age > 17 & age < 22) %>%
        mutate(estimated.final.highschool.year = (survey.date - (age-18) * 365.2422))


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
                  prior.address.postcode, prior.address.abroad, ever.moved,
                  time.at.current.address))


## ----------------------------------------------------------------##
## GOAL: Create new variable indicating the area and the region of the postcodes.

## METHOD: Merge postcode and dat4. 

dat4 <- merge(dat4, postcodes, by.x = "postcode.estimated.final.highschool.year", by.y = "postcodes")


## ----------------------------------------------------------------##
## GOAL: Transform the estimated date respondents were attending their
## final year in highschool into an interval variable indicating the
## academic year. 

## METHOD: Create function which replaces as.Date variable with schoolyear at
## the coast. As schoolyears start in May, all dates before the 5th month indicate
## prior year-current year should be used. Otherwise current year - next 
## year should be used. 

schyearcoast <- function(x) {
        
        a <- year(x)
        
        if(month(x) < 5) {
                
                x <- paste0(as.character(a-1), "-", as.character(a)) } else {
                        
                        x <- paste0(as.character(a), "-", as.character(a+1))
                }
        
        return(x)
        
}

## METHOD: Create function which replaces as.Date variable with schoolyear at
## the highlands. As schoolyears start in May, all dates before the 9th month indicate
## prior year-current year should be used. Otherwise current year - next 
## year should be used. 


schyearhlands <- function(x) {
        
        a <- year(x)
        
        if(month(x) < 9) {
                
                x <- paste0(as.character(a-1), "-", as.character(a)) } else {
                        
                        x <- paste0(as.character(a), "-", as.character(a+1))
                }
        
        return(x)
        
}

## METHOD: Run functions on split data sets (coast and highlands). Remove
## any respondents with final higschoolyears earlier than year 2008-2009. Remove 
## any respondents with final later than 2017-2018 (as there are no respondents for 
## each year in the age group for later years).

dat4A <- dat4 %>%
        filter(region == 2) %>%
        filter(estimated.final.highschool.year > "2009-04-30" & 
                       estimated.final.highschool.year < "2017-05-01")

dat4A$estimated.final.highschool.year <- 
        lapply(dat4A$estimated.final.highschool.year, schyearcoast)


dat4B <- dat4 %>%
        filter(region == 1) %>%
        filter(estimated.final.highschool.year > as.Date("2009-08-31") & 
                       estimated.final.highschool.year < as.Date("2017-09-01"))

dat4B$estimated.final.highschool.year <- 
        lapply(dat4B$estimated.final.highschool.year, schyearhlands)


dat5 <- rbind(dat4A, dat4B)
dat5$estimated.final.highschool.year <- factor(
        unlist(dat5$estimated.final.highschool.year))

remove(dat4A, dat4B)

## ----------------------------------------------------------------##
## GOAL: Assign 

dat6 <- merge(dat5, dat10, by.x = c("postcode.estimated.final.highschool.year", "estimated.final.highschool.year"), by.y = c("postcode", "period"))




## ----------------------------------------------------------------##
## GOAL: Reorder variables in an intuitive way. 

dat3 <- dat3[, c(13, 16, 3, 4, 5, 6, 2, 1, 7, 4, 15, 8, 9, 10, 14, 11, 12)]


                                   
## Save data
saveRDS(dat2, "modified_data_1.csv")
rm(list = ls())

        

