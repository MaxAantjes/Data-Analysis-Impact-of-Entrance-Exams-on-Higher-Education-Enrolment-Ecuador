---
title: "Code Book"
author: "Max Aantjes"
date: "07/06/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Raw Data

All raw data was collected by INEC (Instituto Nacional de Estadística y Census) of Ecuador over various years. Raw data is available for 3 to 4 periods each year from September 2014 to September 2019). The data is automatically downloaded and extracted from temporary folders when running the R code files. However, the **form** used to collect the data (*"formulario"*) and the raw data of the **answers** (*"Bases de Datos"*) are publicly available also through these online government repositories:   

* link to [government data repository for the year 2019](https://www.ecuadorencifras.gob.ec/enemdu-2019/).  

* link to [government data repository for the year 2018](https://www.ecuadorencifras.gob.ec/enemdu-2018/).  

* link to [government data repository for the year 2017](https://www.ecuadorencifras.gob.ec/enemdu-2017/)  

* link to [government data repository for the year 2016](https://www.ecuadorencifras.gob.ec/enemdu-2016/)

* link to [government data repository for the year 2015](https://www.ecuadorencifras.gob.ec/enemdu-2015/)  

* link to [government data repository for the year 2014](https://www.ecuadorencifras.gob.ec/enemdu-2014/)  

Additionally, division of postcodes in rural and urban areas was required for the analysis. As a tidy data of such a division is not publically available at the time of writing, a pdf file from the Ecuadorian government stemming from 2019 was used:

* link to [list of postcodes divided per region and rural and urban areas](https://aplicaciones2.ecuadorencifras.gob.ec/SIN/descargas/cge2019.pdf)

As there is unfortunately no csv file publically available with this division 
