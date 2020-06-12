## This module explores the data in 3 different ways. 


## Create last.migration variable. Note: rows with outside migration are lost. 
library(pacman)
pacman::p_load(dplyr, reshape2)
dat0 <- readRDS("modified_data_2.csv")


## GOAL: Select the group of first year undergraduates.  

first.years <- dat0 %>%
        filter(higher.education.level == "undergraduate" & 
                       no.years.completed == 1 & 
                       currently.matriculated == "yes")

## GOAL: select adequate 'total' set of the population to create
## proportions. 

## METHOD: look for age ranges with at least 10 first year students.

table1 <- dcast(first.years, age ~ no.years.completed)
table2 <- subset(table1, table1[[2]] > 10)
ages.of.interest <- as.vector(table2[[1]])

## METHOD: filter data set for relevant age range. Migrants
## or citizens that have returned from abroad are removed from the
## data set. 

total <- dat0 %>%
        filter(age %in% ages.of.interest) %>%
        filter(location.prior.address == "urban" | 
                       location.prior.address == "rural" |
                       location.prior.address == "none") %>%
        filter(place.of.birth == "urban" | place.of.birth == "rural")


## ----------------------------------------------------------------##

## GOAL: calculate % enrolment of individuals who identify as urban and
## rural in terms of CURRENT.ADDRESS per survey.date.  


## METHOD: create a table of the number of individuals enrolled as 
## undergraduates at university by the variables CURRENT.ADDRESS and SURVEY.DATE. 

CA.table <- dcast(first.years, survey.date ~ current.address)


## METHOD: count total number of individuals identified as urban
## by CURRENT.ADDRESS. 

dat.urban <- total %>%
        filter(current.address == "urban")
list0 <- split(dat.urban, dat.urban$survey.date)
number.urban <- data.frame(unlist(lapply(list0, nrow))) 

## METHOD: count total number of individuals identified as rural
## by CURRENT.ADDRESS.

dat.rural <- total %>%
        filter(current.address == "rural")
list1 <- split(dat.rural, dat.rural$survey.date)
number.rural <- data.frame(unlist(lapply(list1, nrow))) 

## METHOD: divide the number of enrolled rural and urban undergraduates per year 
## by the total rural and urban populations respectively. 

CA.table$numb.urban <- number.urban[, 1]
CA.table$numb.rural <- number.rural[, 1]
CA.table$prop.urban <- CA.table[, 2]/number.urban[, 1]
CA.table$prop.rural <- CA.table[, 3]/number.rural[, 1]


## ----------------------------------------------------------------##

## GOAL: calculate % enrolment of individuals who identify as urban and
## rural in terms of PLACE.OF.BIRTH per survey.date.  


## METHOD: create a table of the number of individuals enrolled as 
## undergraduates at university by the variables PLACE.OF.BIRTH and SURVEY.DATE. 

PB.table <- dcast(first.years, survey.date ~ place.of.birth)


## METHOD: count total number of individuals identified as urban
## by PLACE.OF.BIRTH. 

dat.urban1 <- total %>%
        filter(place.of.birth == "urban")
list0 <- split(dat.urban1, dat.urban1$survey.date)
number.urban1 <- data.frame(unlist(lapply(list0, nrow))) 

## METHOD: count total number of individuals identified as rural
## by PLACE.OF.BIRTH.

dat.rural1 <- total %>%
        filter(place.of.birth == "rural")
list1 <- split(dat.rural1, dat.rural1$survey.date)
number.rural1 <- data.frame(unlist(lapply(list1, nrow))) 

## METHOD: divide the number of enrolled rural and urban undergraduates per year 
## by the total rural and urban populations respectively. 

PB.table$numb.urban <- number.urban1[, 1]
PB.table$numb.rural <- number.rural1[, 1]
PB.table$prop.urban <- PB.table[, 2]/number.urban1[, 1]
PB.table$prop.rural <- PB.table[, 3]/number.rural1[, 1]


## ----------------------------------------------------------------##

## GOAL: calculate % enrolment of individuals who identify as urban and
## rural in terms of PRIOR.ADDRESS and CURRENT.ADDRESS per survey.date. 


## METHOD: create a variable which indicates whether someone has lived 
## at least less than 5 years in an urban area (including 0 years) or
## at least less than 5 years in a rural area.  

dat1A <- total %>%
        filter(current.address == "rural" & time.at.current.address > 4 | 
                       location.prior.address == "rural"& current.address == "urban" & 
                       time.at.current.address < 5) %>%
        mutate(time.urban.rural = "< 5 years urban")

dat1B <- total %>%
        filter(current.address == "urban" & location.prior.address == "urban" |
                       current.address == "urban" & time.at.current.address > 4 |
                       current.address == "rural" & time.at.current.address < 5) %>%
        mutate(time.urban.rural = "< 5 years rural")

isTRUE(nrow(total) == nrow(dat1A)+nrow(dat1B)) ## Check no rows were lost.

dat1 <- rbind(dat1A, dat1B)

## METHOD: create a table of the number of individuals enrolled as 
## undergraduates at university by the variables PLACE.OF.BIRTH and SURVEY.DATE. 

first.years.dat1 <- dat1 %>%
        filter(higher.education.level == "undergraduate" & 
                       no.years.completed == 1 & 
                       currently.matriculated == "yes")
        
PA.table <- dcast(first.years.dat1, survey.date ~ time.urban.rural)


## METHOD: count total number of individuals identified as urban
## by PLACE.OF.BIRTH. 

dat.urban2 <- dat1 %>%
        filter(time.urban.rural == "< 5 years rural")
list0 <- split(dat.urban2, dat.urban2$survey.date)
number.urban2 <- data.frame(unlist(lapply(list0, nrow))) 

## METHOD: count total number of individuals identified as rural
## by PLACE.OF.BIRTH.

dat.rural2 <- dat1 %>%
        filter(time.urban.rural == "< 5 years urban")
list1 <- split(dat.rural2, dat.rural2$survey.date)
number.rural2 <- data.frame(unlist(lapply(list1, nrow))) 

## METHOD: divide the number of enrolled rural and urban undergraduates per year 
## by the total rural and urban populations respectively. 

PA.table$numb.urban <- number.urban2[, 1]
PA.table$numb.rural <- number.rural2[, 1]
PA.table$prop.urban <- PA.table[, 2]/number.urban2[, 1]
PA.table$prop.rural <- PA.table[, 3]/number.rural2[, 1]


## Save data.
saveRDS(PA.table, "past.address.table")
saveRDS(PB.table, "place.of.birth.table")
saveRDS(CA.table, "current.address.table")
rm(list = ls())

