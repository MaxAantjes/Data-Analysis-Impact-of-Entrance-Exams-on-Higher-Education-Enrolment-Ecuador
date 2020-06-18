Module\_0\_Data\_Gathering
================
Max Aantjes
17/06/2020

  - [Introduction](#introduction)
  - [Links to Raw Data](#links-to-raw-data)
      - [Sep 2014 - Sep 2019](#sep-2014---sep-2019)
      - [Dec 2005 - Dec 2013](#dec-2005---dec-2013)
  - [Code for Data Extraction](#code-for-data-extraction)
      - [Packages required](#packages-required)
      - [Function 1: load.files](#function-1-load.files)
      - [Function 2: load.double.zip](#function-2-load.double.zip)
      - [Function 3: unzip.in.td](#function-3-unzip.in.td)
      - [Function 4: add.to.list](#function-4-add.to.list)
      - [Saving the data](#saving-the-data)
  - [Interpreting the Raw Data](#interpreting-the-raw-data)
  - [Links to other modules](#links-to-other-modules)

## Introduction

**Consistent census data from Ecuador is difficult to find and access
due to the continuous changes to the websites and software used by the
Ecuadorian government. Nevertheless, there is a suprising wealth of
census data available from the ENEMDU surveys (Encuesta National de
Empleo, Desempleo y Subempleo) conducted by
[INEC](https://www.ecuadorencifras.gob.ec/). Data of more than 2 million
survey respondents is available over a period of 12 years.**

This guide was written to help researchers access this data using the
source code of my own analysis of the access to higher education in
Ecuador. This data extraction process requires
[R](https://cran.r-project.org/bin/windows/base/) (you might want to use
[Redata
software](https://www.cepal.org/en/topics/redatam/download-redatam) as
well for interpreting the data). Please note that survey responses have
been collected and published quarterly from September 2014. Before this
date, collection and publication of survey responses was annual.

<div style="margin-bottom:50px;">

</div>

## Links to Raw Data

**I strongly discourage downloading each of the survey files manually,
as unzipping the files, storing them and then loading them into R will
be a slow and memory intensive process (I’ve been there).**

Instead I recommend using the loop functions in the R code below.
Nevertheless, for those keen to check the sources of the data
themselves, here are all the links to the government repositories used.

<div style="margin-bottom:50px;">

</div>

#### Sep 2014 - Sep 2019

##### *File extension: .sav/.csv*

  - link to [government data repository for the
    year 2019](https://www.ecuadorencifras.gob.ec/enemdu-2019/);

  - link to [government data repository for the
    year 2018](https://www.ecuadorencifras.gob.ec/enemdu-2018/);

  - link to [government data repository for the
    year 2017](https://www.ecuadorencifras.gob.ec/enemdu-2017/);

  - link to [government data repository for the
    year 2016](https://www.ecuadorencifras.gob.ec/enemdu-2016/);

  - link to [government data repository for the
    year 2015](https://www.ecuadorencifras.gob.ec/enemdu-2015/);

  - link to [government data repository for the
    year 2014](https://www.ecuadorencifras.gob.ec/enemdu-2014/).

<div style="margin-bottom:50px;">

</div>

#### Dec 2005 - Dec 2013

##### *File extension: .sav/.rbs*

  - link to compilation of [government data repositories
    before 2014](https://educacion.gob.ec/enemdu/).

**Please note:** Zipped .sav files from the years 2007 - 2014 are
available from the [“Bases de datos ENEMDU completas
(2331)”](https://educacion.gob.ec/wp-content/uploads/downloads/2017/06/BBDD_ENEMDU_Completas.zip)
masterfile link (which I use). However, there seem to be missing columns
and the 2005/2006 data is missing. Another version of the same data is
downloadable in .rbs format through the individual links on the page
linked above. Nevertheless, I have not been able to extract this data
into R. These files can be read by the aforementioned REDATAM programme
and, according to the software’s [suplementary
documentation](https://www.cepal.org/es/enfoques/open-census-framework-extraer-procesar-datos-censales-redatam),
should be extractable through a combination of python and SQL.

<div style="margin-bottom:50px;">

</div>

## Code for Data Extraction

**The full code for automatic extraction is
[here](https://github.com/MaxAantjes/Data-Analysis-Impact-of-Entrance-Exams-on-Higher-Education-Enrolment-Ecuador/blob/master/Module%200.R)).
If you are not interested in understanding how it works (or crunched on
time), you can simply run it to load the data.**

For those who are keen to understand the code or wish to adapt it to
their needs, below is an explanations how it works.To extract the data
we will use four different functions. Why do we need four? Well, it
seems INEC has changed the file formats and zip methods of its publicly
available data various times over the past 12 years.

<div style="margin-bottom:50px;">

</div>

#### Packages required

Two packages, dplyr and haven, are required for the code to work. They
will be installed and loaded by running the following code (assuming you
already have the pacman package):

``` r
library(pacman)
pacman::p_load(dplyr, haven)
```

<div style="margin-bottom:50px;">

</div>

#### Function 1: load.files

First, will extract the data sets from September 2014 to September 2019
and store it as a list of data sets in R. These files are relatively
straightforward to access, as they are located in subdirectories within
a zipped folder. However, loading the files into R may prove
challenging, because the data of some survey dates is stored in .sav
files, whilst that of others is stored in.csv files. Another thing to
take in mind is that we are going to merge all the data sets of
different survey days together and, therefore, we must create an
additional variable which identifies the date respondents surveyed.
Furthermore, after succeeding in loading the data sets into R, we want
to get rid of the downloaded files and folders, as they take up a
significant chunk of storage space.

Considering the above, we need a function which does the following:

  - *(1)* creates a **temporary** directory
  - *(2)* downloads the zipped folder in the temporary folder;
  - *(3)* unzips the zipped folder;
  - *(4)* searches the actual data set in the zipped folder;
  - *(5)* checks if the data set is in .sav or .csv format;
  - *(6)* loads the data set through the apropriate method (read.csv or
    read\_sav);
  - *(7)* adds an additional column to the data set with the survey
    date.
  - *(8)* deletes the temporary folder.
  - *(9)* returns the data set.

To do this, in turn, the created function need three pieces of
information for each data set:

  - *(1)* the url to download the folder;
  - *(2)* the filepath to the file of interest in the folder;
  - *(3)* the survey date.

For example, we can store the required information as a vector as
follows:

``` r
y2019sep <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Septiembre/BDD_DATOS_ABIERTOS_ENEMDU_%202019_09_CSV.zip",
              "BDD_DATOS_ABIERTOS_ENEMDU_ 2019_09_CSV/enemdu_personas_2019_09.csv",
              "2019-09")
```

The following function can now extract the data we want (our vector is
the variable *x*):

``` r
load.files <- function(x) {
        
        ## (1) Creates Temporary Directory
                td = tempdir()
                temp = tempfile(tmpdir=td, fileext=".zip")
                
                ## (2) Downloads folder
                download.file(x[1],temp)
                
                ## (3) Unzips folder
                unzip(temp, exdir=td, overwrite=TRUE)
                list <- tolower(list.files(td))
                
                ## (5) Checks file extension is .csv.
                if(isTRUE(grepl(".csv", x[2]))) {
                        
                        ## (4, 6) Reads file found following filepath as .csv.  
                dat <- read.csv(file.path(td, x[2]), sep =';')
                
                ## (7) Adds survey date as column. 
                dat <- mutate(dat, date = x[3])
                
                ## Renamed this variable for my own analysis. 
                names(dat)[[1]] <- "survey.location" 
                
                }
                
                ## Performs a similar process for .sav file if it did not find an .csv file. 
                else {
                        
                dat <- data.frame(read_sav(file.path(td, x[2])))
                dat <- mutate(dat, date = x[3])
                names(dat)[[1]] <- "survey.location"  
                
                }
                
                ## (8) Deletes temporary folder. 
                unlink(temp)
                
                ## (9) Returns the loaded data set.
                return(dat)
                
}
```

We can now create a list containing the vectors with these three pieces
of information for each survey date and then run the load.files function
on each element using lapply.

``` r
links <- list(y2019sep, y2019jun, y2019mar, y2018dec, y2018sep, y2018jun, y2018mar, 
              y2017dec, y2017jun, y2017mar, y2016dec, y2016sep, y2016jun, y2016mar,
              y2015dec, y2015jun, y2015mar, y2014dec, y2014sep)

all.data <- lapply(links, load.files)
```

R then stores each of the imported data sets in a new list which we can
later merge together. Congratulations, you now have the data of more
than a million respondents at your disposal\!

<div style="margin-bottom:50px;">

</div>

#### Function 2: load.double.zip

Are we done yet? Well, not really. You might’ve noticed that two survey
dates between September 2014 and September 2019 are missing: September
2015 and September 2017. The above function does not work for this
survey data, because the data is stored in a zip folder **within** a zip
folder. To load these files we thus need a special function which unzips
folders at multiple layers.

This is not that difficult to achieve. We only need to: *(1)* add
information to the vector about the filepath which leads to the second
zipped folder; and *(2)* rewrite the load.files function to unzip this
additional folder. We can to that as follows (as these survey dates only
have data in .csv format we need not worry about checking extensions):

``` r
y2015sep <- c("https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2015/Septiembre-2015/201509_EnemduBDD_CSV.zip",
              
              ## File path to second zipfile 
              "\\201509_EnemduBDD_CSV.zip", 
              
              "\\201509_EnemduBDD_CSV\\201509_EnemduBDD_15anios.csv",
              "2015-09")

load.double.zip <- function(x) {
        
        td = tempdir()
        temp = tempfile(tmpdir=td, fileext=".zip")
        download.file(x[1],temp)
        unzip(temp, exdir=td, overwrite=TRUE)
        
        ## Unzip second zipped folder. 
        unzip(paste0(file.path(td), x[2]), exdir = td)
        
        dat <- read.csv(file.path(td, x[3]), sep =';')
        dat <- mutate(dat, date = x[4])
        names(dat)[[1]] <- "survey.location" 
        
        unlink(temp)
        
        return(dat)
        
}
```

Great\! Now we have all the data sets from September 2014 and September
2019 at our disposal.

<div style="margin-bottom:50px;">

</div>

#### Function 3: unzip.in.td

But what about data before September 2014? Luckily INEC has provided a
collection of data sets, including those from 2007 to 2013 (2005 and
2006 are for some reason missing) in one folder. The annual data sets
are here stored as .sav files in zip folders which reside within a
‘master’ zip folder. As there is only one ‘master’ zip folder we have
to download, we do not need to use a function to download the data this
time:

``` r
url <- "https://educacion.gob.ec/wp-content/uploads/downloads/2017/06/BBDD_ENEMDU_Completas.zip" 
td = tempdir()
temp = tempfile(tmpdir=td, fileext=".zip")
download.file(url,temp)
unzip(temp, exdir=td, overwrite=TRUE)
```

All the files we need are now in our temporary directory (td). You can
check this by typing

``` r
list.files(td)
```

To extract the files for each survey date we now need to tell R three
things:

  - *(1)* Where the zipped folder we want to unzip is;
  - *(2)* where the .sav file is located in the folder;
  - *(3)* which survey date it should associate with that .sav file.

Again, we can store this information in a vector for each survey date as
follows:

``` r
y2007 <- c("\\BBDD_ENEMDU_Completas\\bdd_enemdu_15anios_12_2007.zip", 
           "\\bdd_enemdu_15anios_12_2007\\200712_EnemduBDD_15anios.sav",
           "2007-12")
```

We can now simply adapt a segment of our load.double.zip function to
extract the data for each survey date:

``` r
unzip.in.td <- function(x) {
        
        ## Unzip and store 
        unzip(paste0(file.path(td), x[1]), exdir = td)
        
        dat <- data.frame(read_sav(file.path(td, x[2])))
        dat <- mutate(dat, date = x[3])
        names(dat)[[1]] <- "survey.location" 
        
        return(dat)
        
}
```

Running this with lapply for every date, will again return a list of
data frames:

``` r
list <- list(y2007, y2008, y2009, y2010, y2011, y2012, y2013)
all.data.2 <- lapply(list, unzip.in.td)
```

Fantastic\! We now also have all ENEMDU data available from 2007 to 2013
loaded into R. Remember to unlink the temporary folder after you’re
done:

``` r
unlink(temp)
```

<div style="margin-bottom:50px;">

</div>

#### Function 4: add.to.list

The final thing you might want to do is add all the data together in one
list of dataframes for storage. Adding the two lists (all.data and
all.data.2) together can simply be done through a vector command:

``` r
dat0 <- c(all.data.1, all.data.2)
```

To also add the dataframes from September 2015 and September 2017 to the
list, you can use the following function (with ‘element’ refering to the
dataframe you want to add and ‘list’ referring to the list you want to
add the dataframe to):

``` r
add.to.list <- function(element, list) {
        
        ## Calculate the length of list you want to add dataframe to and add 1.
        a <- length(list) + 1
        
        ## Create additional element in list and assign it the dataframe.
        list[[a]] <- element
        
        return(list)
}
```

You can run it as follows:

``` r
dat0 <- add.to.list(y2015sep, dat0)
dat0 <- add.to.list(y2017sep, dat0) 
```

Almost done\! We now have a list of dataframes containing all survey
data from 2007 to 2019.

<div style="margin-bottom:50px;">

</div>

#### Saving the data

You don’t want to download all those files again\! Just save the data as
an RDS file and you can load it into R whenever you want:

``` r
saveRDS(dat0, "raw_data")
```

<div style="margin-bottom:50px;">

</div>

## Interpreting the Raw Data

In the government repositories linked above codebooks (referred to as
“Diccionario de Variables”) are available for almost every survey date
from 2014 onwards. Unfortunately, these codebooks fail to describe the
levels that different factor variables take. Most of these levels can be
derived from the survey itself (referred to as “formulario”).

The form’s questions have changed minimally over time, but I have
noticed additions to the levels of certain factor variables
(e.g. question p09). Finding out what exactly has changed is not only a
timeconsuming process, but sometimes almost impossible, as these
codebooks and forms are (as far as I know) not available for the data
from survey dates before 2014.

Fortunately, it is possible to find more information on the variables
and their levels by reading the .rbs files for each survey date from
this [link](https://educacion.gob.ec/enemdu/). These files can be opened
by the [REDATA
software](https://www.cepal.org/en/topics/redatam/download-redatam).

Here is a short guide on how to do this:

*1. Open RED7 Admin and click R+SP dictionary*;

*2. Navigate to the downloaded folder and Select the file ending in
dpa*;

*3. You should now see a hierarchical list of variables.*;

*3. Navigate to the ‘PERSONA’ level*.

By double clicking a variable in the list in the left bottom corner, you
can now see the ‘labels’ in the screen on the right for each ‘code’ or
level.

<div style="margin-bottom:50px;">

</div>

## Links to other modules

This guide described the process of downloading Ecuadorian census data.
It is part of a larger set of modules to explore the access to higher
education in Ecuador. Links to the further modules are available below:
