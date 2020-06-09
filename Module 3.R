## This module merges columns and creates new variables. 


## Load packages and data set.

library(pacman)
pacman::p_load(dplyr)

dat0 <- readRDS("modified_data_1.csv")
rural.codes <- as.vector(readRDS("rural_codes.csv"))
urban.codes <- as.vector(readRDS("urban_codes.csv"))


## Whether codes are rural or urban can be checked with the 
## following function. This can be (and has been) used to test 
## the validity of the above method. 

rurORurb <- function(code, v1 = rural.codes, v2 = urban.codes) {
        
        if (is.na(code) == TRUE) {
                
                c <- code
                
        } else {
                
                a <- sum(grepl(code, v1))
                b <- sum(grepl(code, v2))
                
                if(a>0) {
                        
                        c <- "rural"}
                
                if(b>0) {
                        
                        c <- "urban"} 
                
                if(b==0 & a ==0) {
                        
                        c <- "code not found"
                }
                
        }
        
        return(c)
}


## Select rows where place.of.birth with survey.location is the 
## same as place.of.birth. Replace place.of.birth with 
## survey.location. 

born.surv.loc <- dat0 %>%
        filter(born.at.current.address == "yes") %>%
        mutate(place.of.birth = current.address) %>%
        select(-born.at.current.address)


## Now we merge the data sets again and exclude foreign born 
## Ecuadorians.

born.elsewhere <- dat0 %>%
        filter(born.at.current.address == "no.as.national") %>%
        select(-born.at.current.address)

born.elsewhere$place.of.birth <- sapply(
        born.elsewhere$place.of.birth, rurORurb)
        
dat1 <- rbind(born.elsewhere, born.surv.loc)


## Replace codes in original data set with factor variable
## (urban and rural). 

dat1$location.prior.address <- sapply(dat1$location.prior.address, 
                                      rurORurb)

saveRDS(dat1, "temp")


## Create last.migration variable. Note: rows with outside 
## migration are lost. 


always.rural <- dat1 %>%
        filter(place.of.birth == "rural" & ever.moved == "no") %>%
        mutate(last.migration = "always.rural")

always.urban <- dat1 %>%
        filter(place.of.birth == "urban" & ever.moved == "no") %>%
        mutate(last.migration = "always.urban")

rural.rural <- dat1 %>%
        filter(current.address == "rural" & location.prior.address == "rural" & ever.moved == "yes") %>%
        mutate(last.migration = "rural.to.rural")

urban.urban <- dat1 %>%
        filter(current.address == "urban" & location.prior.address == "urban" & ever.moved == "yes") %>%
        mutate(last.migration = "urban.to.urban")

rural.urban <- dat1 %>%
        filter(current.address == "urban" & location.prior.address == "rural" & ever.moved == "yes") %>%
        mutate(last.migration = "rural.to.urban")

urban.rural <- dat1 %>%
        filter(current.address == "rural" & location.prior.address == "urban" & ever.moved == "yes") %>%
        mutate(last.migration = "urban.to.rural")

dat1 <- rbind(always.rural, always.urban, rural.rural, urban.urban, rural.urban, urban.rural) 
dat1 <- select(dat1, -c(current.address, ever.moved, prior.address.abroad, location.prior.address))
dat1$last.migration <- factor(dat1$last.migration)


## Save file 

saveRDS(dat1, file = "modified_data_1.csv")
