## This module generates a dataframe classifying each postcode
## in terms of student/teacher ratio per academic year.


## ----------------------------------------------------------------##
## GOAL: load packages.
pacman::p_load(openxlsx, stringr, tidyverse, zoo)

df0 <- readRDS("postcode_classification.rds")


## ----------------------------------------------------------------##
## GOAL: create dataframe of the accepted places of higher education 
## institutions located at certain cantones per year. 

## METHOD: download and extract excel files.
url <- "https://www.educacionsuperior.gob.ec/wp-content/uploads/downloads/2019/02/Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx.zip"
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".zip")
unzip(temp, list = TRUE)
download.file(url,temp)
unzip(temp, exdir = td, overwrite = TRUE)
dfuni <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 14, rows = 16:78)
dftec <- read.xlsx(file.path(
        td,"Cupos_Aceptados_Indice_de_Tabulados_Diciembre_2018.xlsx"), 
        sheet = 14, rows = 80:143)
unlink(temp)
remove(url)

## METHOD: clean data set of university places.
places.canton.uni <- dfuni %>%
        filter(!(Tipo.de.IES %in% "Total")) %>%
        select(-c(Tipo.de.IES, Provincia.del.Campus)) %>%

## METHOD: merge the two semester columns into annual columns
        mutate(accepted.offers.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.2018 = rowSums(.[14])) %>%
        mutate(cantonname = tolower(Cantón.del.Campus)) %>%
        mutate(cantonname = gsub(" ", "", cantonname)) %>%
        select(-c(1:14)) 
        

## METHOD: clean data set of tecnological schools
names(dftec) <- names(dfuni)
places.canton.tec <-dftec %>%
        select(-c(Tipo.de.IES, Provincia.del.Campus)) %>%
        
        ## METHOD: merge the two semester columns into annual columns
        mutate(accepted.offers.2012 = rowSums(.[2:3])) %>%
        mutate(accepted.offers.2013 = rowSums(.[4:5])) %>%
        mutate(accepted.offers.2014 = rowSums(.[6:7])) %>%
        mutate(accepted.offers.2015 = rowSums(.[8:9])) %>%
        mutate(accepted.offers.2016 = rowSums(.[10:11])) %>%
        mutate(accepted.offers.2017 = rowSums(.[12:13])) %>%
        mutate(accepted.offers.2018 = rowSums(.[14])) %>%
        mutate(cantonname = tolower(Cantón.del.Campus)) %>%
        mutate(cantonname = gsub(" ", "", cantonname)) %>%
        select(-c(1:14))

## METHOD: merge datasets
places.canton <- merge(places.canton.uni, places.canton.tec,
                       by = "cantonname", all.x = TRUE, all.y = TRUE,
                       suffixes = c(".uni", ".tec"))

## METHOD: replace the newly created NA values with 0, as their omission in the
## respective data sets indicates that there are no uni or technical school
## places available. 
places.canton[is.na(places.canton)] <- 0

## METHOD: Create a new variable with total higher education places available
## per year.
places.canton <- places.canton %>%
        mutate(accepted.offers.2012 = rowSums(.[,c(2,9)])) %>%
        mutate(accepted.offers.2013 = rowSums(.[,c(3,10)])) %>%
        mutate(accepted.offers.2014 = rowSums(.[,c(4,11)])) %>%
        mutate(accepted.offers.2015 = rowSums(.[,c(5,12)])) %>%
        mutate(accepted.offers.2016 = rowSums(.[,c(6,13)])) %>%
        mutate(accepted.offers.2017 = rowSums(.[,c(7,14)])) %>%
        mutate(accepted.offers.2018 = rowSums(.[,c(8,15)])) 
        
## METHOD: merge data set with the postcode data set.
df1 <- merge(df0, places.canton, by = "cantonname")


## Next data set required will have to be downloaded manually, as the file has 
## erroneously been saved as a 1993 Excel 5.0 file. The problems this generates
## become evident from the following github conversation:
## https://github.com/tidyverse/readxl/issues/618. Simply download the file 
## available here:
## https://www.ecuadorencifras.gob.ec/wp-content/plugins/download-monitor/download.php?id=324&force=1 
## then save it as a csv file in your working directory under the name
## population.csv. 

dat0 <- read.csv("population.csv", skip = 10)
dat1 <- dat0

## check for the number of totals in theNombre.del.Cantón column, this should 
## equal the number of provinces, 24. In these particular sections, the values 
## in Nombre.de.la.Parroquia represent the different cantons. Finally, again 
## within the same section, the 'total'value in the ÁREA column indicates the row 
## gives the total population for  that particular canton. 
sum(!is.na(dat0$Nombre.del.Cantón[dat0$Nombre.del.Cantón == "Total"])) 



## Replace mistakes and check.
dat1 <- dat1 %>%
        mutate(cantonname = ifelse(Nombre.del.Cantón == "Total", "remove", 
                                   Nombre.del.Cantón))
sum(!is.na(dat1$Nombre.del.Cantón[dat1$cantonname == "Total"])) 

## Fill missing values.
dat1[dat1 == ""] <- NA
dat1$cantonname[2:length(dat1$cantonname)] <- na.locf(dat1$cantonname)
dat1$Nombre.de.la.Parroquia[2:length(dat1$Nombre.de.la.Parroquia)] <-
        na.locf(dat1$Nombre.de.la.Parroquia)

## Create dataframe with young adult population (20-29 y/o) per cantón. 
dat2 <- dat1 %>%
        mutate(subset1 = gsub(" ", "", Nombre.de.la.Parroquia)) %>%
        mutate(subset2 = gsub(" ", "", ÁREA)) %>%
        filter(cantonname != "remove") %>%
        filter(subset1 == "Total" & subset2 == "Total") %>%
        mutate(X.5 = gsub(",", "", X.5)) %>%
        mutate(X.6 = gsub(",", "", X.6)) %>%
        mutate(young.adult.population.2010 = as.integer(X.5) + 
                       as.integer(X.6)) %>%
        mutate(cantonname = tolower(gsub(" ", "", cantonname))) %>%
        select(cantonname, young.adult.population.2010)

## As it turns out the data is duplicated in the data set. After checking the
## original excel document this turns out to be the case there too. We will now
## check if the values of the duplicated data are equal for every canton.
lapply(unique(dat2$cantonname), subset, x$z=dat2)

subsetvector <- function(x,y,z){
        a <- list()
        for(i in 1:length(x)) {
               a[[i]] <- subset(y, z == x[i])$young.adult.population.2010
               a[[i]][3] <- x[i]} 
        return(a)}

test <- subsetvector(unique(dat2$cantonname), dat2, dat2$cantonname) 

identicaltest <- function(x) {
        a <- c()
        for(i in 1:length(x)) {
                a[i] <- isFALSE(identical(x[[i]][1], x[[i]][2]))
                if(a[i] == TRUE) {print(x[[i]][3])}
        }
        return(sum(a))
}

identicaltest(test)

## It turns out there are two values which do not match: Bolivar and Olmedo.
## It turns out the first two occurances of each sums to the actual total. 
## This is why there are 446 instead of 444 values in the dataset (there are
## 222 cantones).
nm1 <- dat2$young.adult.population.2010[dat2$cantonname == "bolivar"]
nm2 <- dat2$young.adult.population.2010[dat2$cantonname == "olmedo"]
print(list(nm1, nm2))
identical(sum(nm1[1], nm1[2]), nm1[3])
identical(sum(nm2[1], nm2[2]), nm2[3])

## We can subset the second set of values from the dataset, i.e. from row
## 224 onwards to generate a nonduplicated set. The set was probably duplicated
## in the first place to create an additional alphabetically ordered dataframe
## (by viewing the dataframe you can see it's in alphabetical order, whilst
## the start of the other list isn't).
population.canton <- dat2[225:nrow(dat2),]
identical(nrow(population.canton), length(unique(dat3$cantonname)))

## Evidently every row now represents ONE canton and each canton is present. We 
## can now add this information to the original dataset. As expected, no NA 
## values were created. 
df2 <- merge(df1, population.canton, by = "cantonname")
length(df2$cantonname[is.na(df2$young.adult.population.2010)])

## Test set: Mocache = 3,076 + 2,640; cayambe = 7,801 + 7,413; 
## mangadelcura = 1,574	1,486
unique(df2$young.adult.population.2010[df2$cantonname=="mocache"])
unique(df2$young.adult.population.2010[df2$cantonname=="cayambe"])
unique(df2$young.adult.population.2010[df2$cantonname=="mangadelcura"])

