## This module cleans and tidies the survey data. 


## ----------------------------------------------------------------##
## GOAL: load packages and data set. 
library(pacman)
pacman::p_load(dplyr, tidyr, lubridate)

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
        
        for(j in 1:length(x)) {
                
                counter <- 0
                
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
                                
                        } 
                        
                        if(counter > 0) {
                        
                        incomplete.columns[j] <- j
                        
                        }
                        
                }
                
        
        }
        
        return(incomplete.columns[!is.na(incomplete.columns)])
        
}

colnames <- c("\\<ciudad\\>", "\\<p03\\>", "\\<p15\\>", "\\<p16a\\>", 
              "\\<p16b\\>", "\\<p17a\\>", "\\<p17b\\>", "\\<p10a\\>", 
              "\\<p10b\\>", "\\<p07\\>", "\\<p12a\\>", "\\<date\\>", 
              "\\<p09\\>", "\\<p02\\>", "\\<pobreza\\>")


position.incomplete.df <- checkcols(dat0, colnames)


## METHOD: Select columns of interest and bind datasets. 

dat1 <- lapply(dat0[-position.incomplete.df], select, survey.location, ciudad,
               p03, p15, p16a, p16b, p17a, p17b, p10a, p10b, p07, p12a,  
               date, p09, p02, pobreza)

dat1 <- do.call(rbind, dat1)


## METHOD: Clarify column names. 

names(dat1) <- c("current.address.area", "current.address.postcode", "age", 
                 "ethnicity", "ever.moved", "time.at.current.address", 
                 "prior.address.abroad", "prior.address.postcode", 
                 "education.level", "years.completed.highest.education.level", 
                 "currently.matriculated","obtained.degree", "survey.date", 
                 "reason.not.matriculated", "gender", "in.poverty")

remove(dat0)


## ----------------------------------------------------------------##
## GOAL: Specify variable class and levels for R. 

## METHOD: Specify factor variables according to surveys and codebook. Levels
## with values c(1,2) are converted to c(0,1) to make regression computations
## posible and clear. Unless otherwise stated, 0 stands for: urban; no; female
## and 1 stands for: rural; yes; male. 

dat2 <- dat1 %>%
        
        mutate(current.address.area = ifelse(
                current.address.area == 1, 0, 1)) %>%
        
        mutate(obtained.degree = ifelse(obtained.degree == 1, 1, 0)) %>%
        
        mutate(ever.moved = ifelse(ever.moved == 1, 0, 1)) %>%
        
        mutate(prior.address.abroad = ifelse(
                prior.address.abroad == 1, 0, 1)) %>%
        
        mutate(currently.matriculated = ifelse(
                currently.matriculated == 1, 1, 0)) %>%
        
        mutate(vulnerable.ethnicity = ifelse(ethnicity %in% 1:5, 1, 0)) %>%
        
        mutate(gender = ifelse(gender == 1, 1, 0)) %>%
        
        mutate(survey.date = as.Date(survey.date, format = "%Y-%m-%d")) %>%

## Some surveys only had 14 levels for the following variable, meaning 'other'
## was classified as level 14. To merge the data appropriately, the newly 
## introduced levels are classified as 'other'.

        mutate(reason.not.matriculated = 
                       factor(
                               reason.not.matriculated, levels = 1:16,
                               labels = c("age", "completed studies", 
                                          "finances",
                                          "poor academic performance", 
                                          "for work", "leveling SENECYT",
                                          "illness", "for house work",
                                          "family prohibition", 
                                          "lack of educational facilities",
                                          "lack of interest", "embarrassment",
                                          "lack of places at educational
                                          facilities", "other", "other",
                                          "other"))) 

remove(dat1)
        

## ----------------------------------------------------------------##
## GOAL: Add a variable calculating the minimum total years enrolled in 
## education. 


## METHOD: Calculate every years in education for undergraduate graduates.

temp <- dat2 %>%
        filter(obtained.degree == 1 & education.level == 9 &
                       currently.matriculated == 0) 

mean(temp$years.completed.highest.education.level)

## METHOD: First we assign the base years required for the highest level of 
## education. This is calculated through the Sistema Anterior, Sistema 
## Actual Reforma Curricular table in the 2019 survey. For postgraduate
## students we assume 5 years of undergrad education, in line with the 
## mean calculated above. 

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
dat3$years.in.education[dat3$education.level == 10] <- 13+5

dat3$years.in.education <- dat3$years.in.education + 
        dat3$years.completed.highest.education.level

remove(dat2)


## ----------------------------------------------------------------##
## GOAL: Add a variable indicating if the respondent ever enrolled in 
## higher education.

dat4 <- dat3 %>%
        mutate(ever.enrolled.in.higher.education = 
                       ifelse(education.level > 7, 1, 0))

remove(dat3)


## ----------------------------------------------------------------##
## GOAL: Clean up unnecessary NA values in data set (where the
## question was not asked as information was already given in
## prior questions). This allows us to see whether NA values
## are actually missing data. 

## METHOD: set time.at.current.address to age if ever.moved
## equals "no". Replace prior.address.postcode with current.address.postcode. 
## Replace reason.for.migration with "none". 

temp1 <- dat4 %>%
        filter(ever.moved == 0) %>%
        mutate(time.at.current.address = age) %>%
        mutate(prior.address.postcode = current.address.postcode) 

temp2 <- dat4 %>%
        filter(ever.moved == 1 | is.na(ever.moved))

dat5 <- rbind(temp1, temp2)

## METHOD: set reason.for.not.matriculated to "none if 
## currently.matriculated equals "yes".

temp1 <- dat5 %>%
        filter(currently.matriculated == 1) %>%
        mutate(reason.not.matriculated = "none")

temp2 <- dat5 %>%
        filter(currently.matriculated == 0 | is.na(currently.matriculated))

dat5 <- rbind(temp1, temp2)

## METHOD: set prior.address.abroad to "no" if ever.moved
## equals "yes". 

dat5$prior.address.abroad[dat4$ever.moved == 0] <- 0

## METHOD: set obtained.degree to "no" if education.level
## is below higher education. 

dat5$obtained.degree[as.integer(dat4$education.level) < 8] <- 0

## METHOD: Remove remaining NA values. 
dat5 <- dat5 %>%
        drop_na()

remove(dat4, temp1, temp2)


## ----------------------------------------------------------------##
## GOAL: order columns in an intuitive way.

dat6 <- dat5[, c(13, 3, 15, 4, 16, 17, 18, 19, 2, 1, 5, 6, 7, 8, 9, 10, 11, 14, 
                 12)]

remove(dat5)


## ----------------------------------------------------------------##
## Save data
saveRDS(dat6, "clean_data_survey.rds")
rm(list = ls())