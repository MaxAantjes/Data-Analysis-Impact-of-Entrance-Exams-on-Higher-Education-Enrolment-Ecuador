## This module merges columns and creates new variables. 


## Load packages and data set.

library(pacman)

dat0 <- readRDS("clean_data_urban_rural")


## Create last.migration variable. Note: rows with outside migration are lost. 

always.rural <- dat0 %>%
        filter(place.of.birth == "rural" & ever.moved == "no") %>%
        mutate(last.migration = "always.rural")

always.urban <- dat0 %>%
        filter(place.of.birth == "urban" & ever.moved == "no") %>%
        mutate(last.migration = "always.urban")

rural.rural <- dat0 %>%
        filter(current.address == "rural" & location.prior.address == "rural" & ever.moved == "yes") %>%
        mutate(last.migration = "rural.to.rural")

urban.urban <- dat0 %>%
        filter(current.address == "urban" & location.prior.address == "urban" & ever.moved == "yes") %>%
        mutate(last.migration = "urban.to.urban")

rural.urban <- dat0 %>%
        filter(current.address == "urban" & location.prior.address == "rural" & ever.moved == "yes") %>%
        mutate(last.migration = "rural.to.urban")

urban.rural <- dat0 %>%
        filter(current.address == "rural" & location.prior.address == "urban" & ever.moved == "yes") %>%
        mutate(last.migration = "urban.to.rural")

dat1 <- rbind(always.rural, always.urban, rural.rural, urban.urban, rural.urban, urban.rural) 
dat1 <- select(dat1, -c(current.address, ever.moved, prior.address.abroad, location.prior.address))
dat1$last.migration <- factor(dat1$last.migration)
dat1 <- dat1[, c(1,2,3,10,4,5,6,7,8,9)]

## Save data.
saveRDS(dat1, "clean_data_urban_rural_migration")

        