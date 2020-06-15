## This module merges columns and creates new variables. 

## Load packages and data set.

library(pacman)
pacman::p_load(dplyr, stringr)

dat0 <- readRDS("modified_data_1.csv")
list0 <- readRDS("codes.csv")
list0$codes <- as.integer(list0$codes)


## GOAL: know whether there is information on the place of birth
## of the individual.

## METHOD: select rows where born.at.current.address equals
## NA. Replace place.of.birth. with NA. 

bna <- dat0 %>%
        filter(is.na(born.at.current.address)) %>%
        mutate(place.of.birth = NA) %>%
        select(-born.at.current.address)

## GOAL: know whether the individuals who were born at the survey
## location were born in rural or urban areas. 

## METHOD: Select rows where born.at.current.address equals
## yes. Replace place.of.birth with survey.location (which is 
## already a factor variable with levels "urban" and "rural"). 

bsl <- dat0 %>%
        filter(born.at.current.address == "yes") %>%
        mutate(place.of.birth = current.address) %>%
        select(-born.at.current.address)


## GOAL: know whether the individuals who were not born at the
## survey location were born abroad. .

## METHOD: select rows where born.at.current.address equals
## no.as.international. Replace place.of.birth with "abroad". 

ba <- dat0 %>%
        filter(born.at.current.address == "no.as.international") %>%
        mutate(place.of.birth = "abroad") %>%
        select(-born.at.current.address)


## GOAL: know whether the individuals who were not born at the
## survey location were born in rural or urban areas. 

## METHOD: select rows where born.at.current.address equals
## no.as.national. The data takes the form of integer postcode 
## values. Match rural codes with "rural" and urban codes with 
## "urban" through function. 

be <- dat0 %>%
        filter(born.at.current.address == "no.as.national") %>%
        select(-born.at.current.address)


rurORurb <- function(x, y = list0) {
        
        if (is.na(x) == TRUE) {
                
                c <- NA
                
        } else {
                
               c <-  y$area[y$codes == x]
               
               if(identical(c, character(0))) {
                       
                       c <- "code not found"
               }
                        
        }
        
        return(c)
}


be$place.of.birth <- sapply(be$place.of.birth, rurORurb)


## GOAL: Merge the three subsets back into a single data.frame.  
        
dat1 <- rbind(be, ba, bsl, bna)
dat1$place.of.birth <- factor(dat1$place.of.birth, 
                              levels = c("urban", "rural", "abroad",
                                         "code not found"),
                              labels = c("urban", "rural", "abroad",
                                         "code not found"))


## GOAL: Replace codes in location.prior.address with factor
## level "abroad". 

## METHOD: select rows where prior.address.abroad equals "yes".
## set location.prior.address to "abroad". 

pab <- dat1 %>%
        filter(prior.address.abroad == "yes") %>%
        mutate(location.prior.address = "abroad") %>%
        select(-prior.address.abroad)


## GOAL: Replace codes in location.prior.address with factor
## levels "urban" and "rural". 

## METHOD: select rows where prior.address.abroad equals "no".
## Apply rurORurb function on location.prior.address. .

pal <- dat1 %>%
        filter(prior.address.abroad == "no" |
                       is.na(prior.address.abroad)) %>%
        select(-prior.address.abroad)

pal$location.prior.address <- sapply(pal$location.prior.address, 
                                      rurORurb)


## GOAL: Merge the two subsets back into a single data.frame. 

dat2 <- rbind(pab, pal)


## GOAL: Clean up unnecessary NA values in data set (where the
## question was not asked as information was already given in
## prior questions). This allows us to see whether NA values
## are actually missing data. 

## METHOD: set time.at.current.address to age if ever.moved
## equals "no". Set prior address to "none". 

nm <- dat2 %>%
        filter(ever.moved == "no") %>%
        mutate(time.at.current.address = age) %>%
        mutate(location.prior.address = "none")

ym <- dat2 %>%
        filter(ever.moved == "yes" | is.na(ever.moved))
        
dat3 <- rbind(nm, ym)
dat3$location.prior.address <- factor(dat3$location.prior.address,
                                      levels = c("urban", "rural", "abroad",
                                                 "none", "code not found"),
                                      labels = c("urban", "rural", "abroad",
                                                 "none", "code not found"))


## METHOD: set obtained.degree to "no" if higher.education.level
## equals "none". Set field.of.degree to "none" if obtained.degree
## equals "no". 

dat3$obtained.degree[dat3$higher.education.level == "none"] <- "no"
dat3$field.of.degree[dat3$obtained.degree == "no"] <- "none"
dat3$obtained.degree <- factor(dat3$obtained.degree)


## Order columns in intuitive way.
dat3 <- dat3[, c(13, 2, 3, 5, 6, 1, 7, 4, 15, 8, 9, 10, 14, 11, 12)]


## We now actually now whether NA values are missing. 
saveRDS(dat3, file = "modified_data_2.csv")
rm(list = ls())