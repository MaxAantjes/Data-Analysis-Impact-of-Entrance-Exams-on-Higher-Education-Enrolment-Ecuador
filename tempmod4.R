## This module modifies the data to estimate the particular features of respondents
## in terms of their final highschool year. It uses the tidy data sets from module 1,
## module 2 and module 3. 


## ----------------------------------------------------------------##
## GOAL: load packages and data set. 
library(pacman)
pacman::p_load(dplyr, lubridate)

dat0 <- readRDS("raw_data_survey.rds")
df1 <- readRDS("clean_data_area_region.rds")
df2 <- readRDS("tidy_data_student_teacher_ratio.rds")


## ----------------------------------------------------------------##
## GOAL: Create variables indicating information about the respondent during
## the estimated period he/she was attending the final year of highschool, 
## including postcode and date. 

## METHOD: Create a variable with the date and year the respondent was 18.

dat1 <- dat0 %>%
        filter(age > 17 & age < 22) %>%
        mutate(estimated.final.highschool.year = (survey.date - (age-18) * 365.2422))


## METHOD: Determine the location of respondents during their
## estimated final school year. Remove current.address and 
## prior.address variables as they are no longer of interest

dat1 <- dat1 %>%
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

dat2 <- merge(dat1, df1, by.x = "postcode.estimated.final.highschool.year", by.y = "postcodes")
remove(dat0)


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

dat2A <- dat2 %>%
        filter(region == 2) %>%
        filter(estimated.final.highschool.year > "2009-04-30" & 
                       estimated.final.highschool.year < "2017-05-01")

dat2A$estimated.final.highschool.year <- 
        lapply(dat2A$estimated.final.highschool.year, schyearcoast)


dat2B <- dat2 %>%
        filter(region == 1) %>%
        filter(estimated.final.highschool.year > as.Date("2009-08-31") & 
                       estimated.final.highschool.year < as.Date("2017-09-01"))

dat2B$estimated.final.highschool.year <- 
        lapply(dat2B$estimated.final.highschool.year, schyearhlands)


dat3 <- rbind(dat2A, dat2B)
dat3$estimated.final.highschool.year <- factor(
        unlist(dat5$estimated.final.highschool.year))

remove(dat2A, dat2B)


## ----------------------------------------------------------------##
## GOAL: Assign the estimated ratio between student and teacher for the estimated
## final school year of each respondent. 

dat4 <- merge(dat3, df2, by.x = c("postcode.estimated.final.highschool.year", "estimated.final.highschool.year"), by.y = c("postcode", "period"))


