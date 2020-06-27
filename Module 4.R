## This module explores the data in 3 different ways. 


## Create last.migration variable. Note: rows with outside migration are lost. 
library(pacman)
pacman::p_load(dplyr, reshape2, ggplot2, zoo, tidyr, RColorBrewer, colorRampPalett)
dat0 <- readRDS("clean_data_survey.rds")


## GOAL: Graph the years completed by Ecuadorian youth by
## ethnicity, area, gender and year. 

dat1 <- dat0 %>%
        filter(age > 21 & age < 25) 
dat1$survey.date <- lubridate::year(dat1$survey.date) 

pop <- dat1 %>%
        group_by(current.address.area, survey.date, ethnicity) %>%
        summarise(n())
names(pop)[4] <- "count"
pop$survey.date <- factor(pop$survey.date)

hispop <- ggplot(pop, aes(y = count, x = survey.date, fill = ethnicity)) + geom_bar(position= "fill", stat = "identity", ) + facet_grid(.~current.address.area) + scale_y_continuous(labels=scales::percent) + scale_fill_manual(values = rev(colorRampPalette(brewer.pal(n = 8,"BuPu"))(8))) + theme(plot.title=element_text(size=14, face="bold", vjust=-1), plot.subtitle=element_text(size=9, face="italic", color="black"), axis.text.x = element_text(angle = 90), strip.background =element_rect(fill= "#D0F0C0"), strip.text = element_text(size = 12, face = "bold"))  + 
        labs(title = "Ethnicity of Ecuadorian Youth by Urban and Rural Population", 
             subtitle = "\nYouth corresponds with respondents in the age group 22 - 24 years old. Ethnicity is self-reported.\n", 
             caption = "Source: ENEMDU surveys collected by INEC from Dec. 2007 to Sept. 2019") +
        xlab("survey year") +
        ylab("Proportion of respondents")

## GOAL: Create histogram showing number of years completed by Ecuadorian youth by area.
histare <- ggplot(dat1, aes(years.in.education, fill = current.address.area)) + 
        geom_bar(position = "dodge", aes(y = ..prop..), alpha = 3.5/5) + 
        facet_grid(.~survey.date) +
        labs(y = "Percent", fill="Area") +
        scale_y_continuous(labels=scales::percent) +
        facet_wrap(.~survey.date) + theme_bw() +
        scale_fill_manual(values=c("#999999", "#006400")) + 
        labs(title = "Years Ecuadorian youth completed in all levels of education by area and year", 
             subtitle = "Youth corresponds with respondents in the age group 22 - 24 years old.", 
             caption = "Source: ENEMDU surveys collected by INEC from Dec. 2007 to Sept. 2019") +
        xlab("Total number of years") +
        ylab("Proportion of respondents")


## GOAL: Create histogram showing number of years completed by Ecuadorian youth by gender.
histgen <- ggplot(dat1, aes(years.in.education, fill = gender)) + 
        geom_bar(position = "dodge", aes(y = ..prop..), alpha = 3.5/5) + 
        facet_grid(.~survey.date) +
        labs(y = "Percent", fill="Gender") +
        scale_y_continuous(labels=scales::percent) +
        facet_wrap(.~survey.date) + theme_bw() +
        scale_fill_manual(values=c("#32CD32", "#9400D3")) + 
        labs(title = "Years Ecuadorian youth completed in all levels of education by gender and year", 
             subtitle = "Youth corresponds with respondents in the age group 22 - 24 years old.", 
             caption = "Source: ENEMDU surveys collected by INEC from Dec. 2007 to Sept. 2019") +
        xlab("Total number of years") +
        ylab("Proportion of respondents")


## GOAL: Create histogram showing number of years completed by Ecuadorian youth by ethnicity.
dat1$ethnicity <- factor(dat1$ethnicity, levels = c("indigenous", "afroecuadorian",
                                                    "black", "mulatto", "montubio",
                                                    "mestizo", "white", "other"),
                         labels = c("Considered Vulnerable", "Considered Vulnerable",
                                    "Considered Vulnerable", "Considered Vulnerable", 
                                    "Considered Vulnerable", "Not Considered Vulnerable", 
                                    "Not Considered Vulnerable", NA))

dat2 <- dat1 %>%
        filter(ethnicity == "Considered Vulnerable" | ethnicity == "Not Considered Vulnerable")


histeth <- ggplot(dat2, aes(years.in.education, fill = ethnicity)) + 
        geom_bar(position = "dodge", aes(y = ..prop..), alpha = 3.5/5) + 
        facet_grid(.~survey.date) +
        labs(y = "Percent", fill="Ethnicity") +
        scale_y_continuous(labels=scales::percent) +
        facet_wrap(.~survey.date) + theme_bw() +
        labs(title = "Years Ecuadorian youth completed in all levels of education by ethnicity and year", 
             subtitle = "Youth corresponds with respondents in the age group 22 - 24 years old. 
Ethnical groups considered vulnerable: indigenous;afroecuadorian; black; mulatto; montubio. 
Ethnical groups considered not vulnerable: mestizo; white.",
             caption = "Source: ENEMDU surveys collected by INEC from Dec. 2007 to Sept. 2019") +
        xlab("Total number of years") +
        ylab("Proportion of respondents")

dat2 <- dat2 %>%
        filter(survey.date == "2007" | survey.date == "2013" | survey.date == "2019") %>%
        mutate(primary.school.final.year = ifelse(survey.date == "2019", 13, 12)) %>%
        mutate(secondary.school.final.year = ifelse(survey.date == "2019", 13, 12)) 


## GOAL: Create histogram showing number of years completed by Ecuadorian youth by ethnicity, area and year. 
histareeth <- ggplot(data = dat2, aes(years.in.education, fill = ethnicity)) +
        geom_vline(data = dat2, aes(xintercept = primary.school.final.year,
                                    color = "Primary"),
                   size = 0.8, linetype = 1) + 
        geom_vline(data = dat2, aes(xintercept = secondary.school.final.year, 
                                    color = "Secondary"),
                   size = 0.8,  linetype = 1) + 
        geom_bar(position = "dodge", aes(y = ..prop..), alpha = 3.5/5) + 
        facet_grid(current.address.area~survey.date)  + 
        scale_y_continuous(labels=scales::percent)  +
        scale_fill_manual(name = "Ethnicity \n considered \n vulnerable", values=c("#32CD32", "#9400D3")) + 
        labs(title = "Number of years Ecuadorian young adults completed \n in all levels of education by gender and year", 
             subtitle = "\n Young adults refers to respondents in the age group 22 - 25 years old. The \n final school years in 2007 correspond with the old school system, 'Sistema \n Anterior'. The final school years in 2019 correspond with the new school \n system, 'Sistema Actual Reforma Curricular'. Ethnical groups considered \n vulnerable: indigenous; afroecuadorian; black; mulatto; montubio. Ethnical \n groups considered not vulnerable: mestizo; white; other.",
             caption = "Source: ENEMDU surveys collected by INEC from Dec. 2007 to Sept. 2019") +
        xlab("Total number of years") +
        ylab("Proportion of respondents") + theme_bw() +
        scale_color_manual(name = "Final School Year", values=c("#F9CCCA", "#add8e6")) +
        theme(plot.title=element_text(size=14, face="bold", vjust=-1),
              plot.subtitle=element_text(size=11, face="italic", color="black"))


## GOAL: Create histogram showing number of years completed by Ecuadorian youth by gender, area and year.
histaregen <- ggplot(data = dat2, aes(years.in.education, fill = gender)) +
        geom_vline(data = dat2, aes(xintercept = primary.school.final.year,
                                    color = "Primary"),
                   size = 0.8, linetype = 1) + 
        geom_vline(data = dat2, aes(xintercept = secondary.school.final.year, 
                                    color = "Secondary"),
                   size = 0.8,  linetype = 1) + 
        geom_bar(position = "dodge", aes(y = ..prop..), alpha = 3.5/5) + 
        facet_grid(current.address.area~survey.date)  + 
        scale_y_continuous(labels=scales::percent)  +
        scale_fill_manual(values=c("#32CD32", "#9400D3")) + 
        labs(title = "Number of years Ecuadorian young adults completed \n in all levels of education by gender and year", 
             subtitle = "\n Young adults refers to respondents in the age group 22 - 25 years old. The \n final school years in 2007 correspond with the old school system, 'Sistema \n Anterior'. The final school years in 2019 correspond with the new school \n system, 'Sistema Actual Reforma Curricular'.",
             caption = "Source: ENEMDU surveys collected by INEC from Dec. 2007 to Sept. 2019") +
        xlab("Total number of years") +
        ylab("Proportion of respondents") + theme_bw() +
        scale_color_manual(name = "Final School Year", values=c("#F9CCCA", "#add8e6")) +
        theme(plot.title=element_text(size=14, face="bold", vjust=-1),
              plot.subtitle=element_text(size=11, face="italic", color="black"))


## GOAL: Create boxplot showing number of years completed by Ecuadorian youth by gender, area and year.


dat3 <- dat1 %>%
        filter(survey.date %in% c(2007, 2013, 2019)) %>%
        filter(ethnicity == "Considered Vulnerable" | ethnicity == "Not Considered Vulnerable")

dat3$current.address.area <- relevel(dat3$current.address.area, "rural")

lengths <- dat3 %>%
        group_by(current.address.area, gender, survey.date) %>%
        summarize(n())
means <- dat3 %>%
        group_by(current.address.area, gender, survey.date) %>%
        summarize(mean(years.in.education))
names(means)[4] <- "mean" 

means$mean <- round(means$mean, digits = 2)

rowvalue <- function(x) {
        a <- c()
        for(i in 1:nrow(x)) {
                a[i] <- i
        }
        x$row <- a
        return(x)
}

lengths <- rowvalue(lengths)
means <- rowvalue(means)
labels <- merge(lengths, means, by.x = "row", by.y ="row")

labels$labels <- paste0("n=", labels$`n()`, "\n", "m=", labels$mean)
labels <- labels %>%
        select(current.address.area.x, gender.x, survey.date.x, labels)
names(labels)[1:3] <- c("current.address.area", "gender", "survey.date")

boxgenare <- ggplot(data = dat3, aes(x = gender, y = years.in.education, fill = factor(survey.date))) +
        geom_boxplot(aes(fill=factor(survey.date)), position=position_dodge(.9)) + 
        facet_grid(.~current.address.area)  + 
        labs(title = "Number of years Ecuadorian young adults completed in all levels of education \nby area, gender and year", 
             subtitle = "\n Young adults refers to respondents in the age group 22 - 25 years old. The final school years in 2007 correspond with the old school system, \n 'Sistema Anterior'. The final school years in 2019 correspond with the new school system, 'Sistema Actual Reforma Curricular'. The value \nn refers to number of observations and the value m refers to mean for each year by gender and area.\n",
             caption = "Source: ENEMDU surveys collected by INEC from Dec. 2007 to Sept. 2019") +
        xlab("") + theme_bw() + 
        ylab("Total number of years") +
        theme(plot.title=element_text(size=14, face="bold", vjust=-1),
              plot.subtitle=element_text(size=9, face="italic", color="black"),
              strip.background =element_rect(fill= "#D0F0C0"),
              strip.text = element_text(size = 12, face = "bold")) + 
        stat_summary(aes(fill=factor(survey.date), shape = "mean"), fun=mean, geom="point", position=position_dodge(.9), color="black", size=3) +
        geom_text(data = labels, aes(label = labels, y = max(dat3$years.in.education) + 1), position = position_dodge(.9), size = 2.5) +
        scale_shape_manual(name = "Shapes", values = c("mean" = "x")) +
        scale_fill_manual(name = "Survey Date", values=c("#F0F8FF", "#ADD8E6", "#6ca0dc"))
 
       
## GOAL: Create boxplot showing number of years completed by Ecuadorian youth by gender, ethnicity and year.
lengths <- dat3 %>%
        group_by(ethnicity, gender, survey.date) %>%
        summarize(n())
means <- dat3 %>%
        group_by(ethnicity, gender, survey.date) %>%
        summarize(mean(years.in.education))
names(means)[4] <- "mean" 

means$mean <- round(means$mean, digits = 2)

rowvalue <- function(x) {
        a <- c()
        for(i in 1:nrow(x)) {
                a[i] <- i
        }
        x$row <- a
        return(x)
}

lengths <- rowvalue(lengths)
means <- rowvalue(means)
labels2 <- merge(lengths, means, by.x = "row", by.y ="row")

labels2$labels <- paste0("n=", labels2$`n()`, "\n", "m=", labels2$mean)
labels2 <- labels2 %>%
        select(ethnicity.x, gender.x, survey.date.x, labels)
names(labels2)[1:3] <- c("ethnicity", "gender", "survey.date")

boxgeneth <- ggplot(data = dat3, aes(x = gender, y = years.in.education, fill = factor(survey.date))) +
        geom_boxplot(aes(fill=factor(survey.date)), position=position_dodge(.9)) + 
        facet_grid(.~ethnicity)  + 
        labs(title = "Number of years Ecuadorian young adults completed in all levels of education \nby ethnicity, gender and year", 
             subtitle = "\n Young adults refers to respondents in the age group 22 - 25 years old. The final school years in 2007 correspond with the old school system, \n 'Sistema Anterior'. The final school years in 2019 correspond with the new school system, 'Sistema Actual Reforma Curricular'. The value \nn refers to number of observations and the value m refers to mean for each year by gender and area. Ethnical groups considered vulnerable: \nindigenous; afroecuadorian; black; mulatto; montubio. Ethnical groups considered not vulnerable: mestizo; white; other.\n",
             caption = "Source: ENEMDU surveys collected by INEC from Dec. 2007 to Sept. 2019") +
        xlab("") + theme_bw() + 
        ylab("Total number of years") +
        theme(plot.title=element_text(size=14, face="bold", vjust=-1),
              plot.subtitle=element_text(size=9, face="italic", color="black"),
              strip.background =element_rect(fill= "#D0F0C0"),
              strip.text = element_text(size = 12, face = "bold")) + 
        stat_summary(aes(fill=factor(survey.date), shape = "mean"), fun=mean, geom="point", position=position_dodge(.9), color="black", size=3) +
        geom_text(data = labels2, aes(label = labels, y = max(dat3$years.in.education) + 1), position = position_dodge(.9), size = 2.5) +
        scale_shape_manual(name = "Shapes", values = c("mean" = "x")) +
        scale_fill_manual(name = "Survey Date", values=c("#F0F8FF", "#ADD8E6", "#6ca0dc"))

dat4 <- dat3 %>%
        group_by(current.address.area, gender, ethnicity) %>%
        summarize(n())
               
                     
## GOAL: Select the group of first year undergraduates.  

first.years <- dat0 %>%
        filter(higher.education.level == "undergraduate" & 
                       no.years.completed == 1 & 
                       currently.matriculated == "yes")

## GOAL: select adequate 'total' set of the population to create
## proportions. 

## METHOD: look for age ranges with at least 10 first year students.

table1 <- dcast(first.years, age ~ no.years.completed)
table1 <- subset(table1, table1[[2]] > 500)
ages.of.interest <- as.vector(table1[[1]])

## METHOD: filter data set for relevant age range. Migrants
## or citizens that have returned from abroad are removed from the
## data set. 

total <- dat0 %>%
        filter(age %in% ages.of.interest) %>%
        filter(location.prior.address == "urban" | 
                       location.prior.address == "rural" |
                       location.prior.address == "none") %>%
        filter(place.of.birth == "urban" | place.of.birth == "rural" |
                       is.na(place.of.birth))

first.years <- total %>%
        filter(higher.education.level == "undergraduate" & 
                       no.years.completed == 1 & 
                       currently.matriculated == "yes")


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
CA.table$total <- CA.table$urban+CA.table$rural
CA.table$CA.prop.urban <- CA.table$urban/CA.table$numb.urban
CA.table$CA.prop.rural <- CA.table$rural/CA.table$numb.rural
CA.table$prop.total <- CA.table$total/(CA.table$numb.rural+CA.table$numb.urban)
CA.table$CA.rural.urban.ratio <- (CA.table$CA.prop.rural/CA.table$CA.prop.urban)*100


## ----------------------------------------------------------------##

## GOAL: calculate % enrolment of individuals who identify as urban and
## rural in terms of PLACE.OF.BIRTH per survey.date.  


## METHOD: create a table of the number of individuals enrolled as 
## undergraduates at university by the variables PLACE.OF.BIRTH and SURVEY.DATE. 

first.years.PB <- first.years %>%
        filter(!is.na(place.of.birth))

PB.table <- dcast(first.years.PB, survey.date ~ place.of.birth)


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
PB.table$total <- PB.table$urban+PB.table$rural
PB.table$PB.prop.urban <- PB.table$urban/PB.table$numb.urban
PB.table$PB.prop.rural <- PB.table$rural/PB.table$numb.rural
PB.table$prop.total <- PB.table$total/(PB.table$numb.rural+PB.table$numb.urban)
PB.table$PB.rural.urban.ratio <- (PB.table$PB.prop.rural/PB.table$PB.prop.urban)*100



## ----------------------------------------------------------------##

## GOAL: calculate % enrolment of individuals who identify as urban and
## rural in terms of PRIOR.ADDRESS and CURRENT.ADDRESS per survey.date. 


## METHOD: create a variable which indicates whether someone has either lives
## in a rural/urban area OR has changed area for academic reasons. 

first.years.dat1R <- total %>%
        filter(current.address == "rural" & ever.moved == "no" | # always lived rural
                       current.address == "rural" & ever.moved == "yes" &
                       location.prior.address == "urban" & !reason.for.migration == "academic" | # moved from urban to rural but not for academic reasons
                       current.address == "urban" & ever.moved == "yes" &
                       location.prior.address == "rural" & reason.for.migration == "academic" | # moved from rural to urban for academic reasons
                       current.address == "rural" & location.prior.address == "rural") %>% # always rural but moved
        filter(higher.education.level == "undergraduate" & 
                       no.years.completed == 1 & 
                       currently.matriculated == "yes")  %>%
        mutate(identification = "rural")
first.years.dat1U <- total %>%
        filter(current.address == "urban" & ever.moved == "no" | ## always lived urban
                       current.address == "rural" & ever.moved == "yes" &
                       location.prior.address == "urban" & reason.for.migration == "academic" | # moved from urban to rural for academic reasons
                       current.address == "urban" & ever.moved == "yes" &
                       location.prior.address == "rural" & !reason.for.migration == "academic" | # moved from rural to urban but for non academic reasons
                       current.address == "urban" & location.prior.address == "urban") %>% # always urban but moved
        filter(higher.education.level == "undergraduate" & 
                       no.years.completed == 1 & 
                       currently.matriculated == "yes")  %>%
        mutate(identification = "urban")


## METHOD: Check no rows were lost or duplicated and bind rows.

isTRUE(nrow(first.years) == nrow(first.years.dat1R)+nrow(first.years.dat1U)) 
first.years.dat1 <- rbind(first.years.dat1R, first.years.dat1U)


## METHOD: create table based on survey date and identification. 
        
PA.table <- dcast(first.years.dat1, survey.date ~ identification)


## METHOD: count total number of individuals identified as urban
## by PLACE.OF.BIRTH. 

dat.urban2 <- total %>%
        filter(current.address == "urban")
list0 <- split(dat.urban2, dat.urban2$survey.date)
number.urban2 <- data.frame(unlist(lapply(list0, nrow))) 

## METHOD: count total number of individuals identified as rural
## by PLACE.OF.BIRTH.

dat.rural2 <- total %>%
        filter(current.address == "rural")
list1 <- split(dat.rural2, dat.rural2$survey.date)
number.rural2 <- data.frame(unlist(lapply(list1, nrow))) 

## METHOD: divide the number of enrolled rural and urban undergraduates per year 
## by the total rural and urban populations respectively. 

PA.table$numb.urban <- number.urban2[, 1]
PA.table$numb.rural <- number.rural2[, 1]
PA.table$total <- PA.table$urban+PA.table$rural
PA.table$PA.prop.urban <- PA.table$urban/PA.table$numb.urban
PA.table$PA.prop.rural <- PA.table$rural/PA.table$numb.rural
PA.table$prop.total <- PA.table$total/(CA.table$numb.rural+
                                               PA.table$numb.urban)
PA.table$PA.rural.urban.ratio <- (PA.table$PA.prop.rural/
                                          PA.table$PA.prop.urban)*100


## ----------------------------------------------------------------##

## GOAL: explore the difference in academic migration between
## those of rural and urban origin. 
academic.migration <- first.years.dat1 %>%
        filter(reason.for.migration == "academic") %>%
        mutate(identification = factor(
                ifelse(identification == "rural", 
                       "rural.to.urban.migration", 
                       "urban.to.rural.migration")))

dat.urban3 <- first.years.dat1U
list0 <- split(dat.urban3, dat.urban3$survey.date)
number.urban3 <- data.frame(unlist(lapply(list0, nrow))) 

## METHOD: count total number of individuals identified as rural
## by PLACE.OF.BIRTH.

dat.rural3 <- first.years.dat1R
list1 <- split(dat.rural3, dat.rural3$survey.date)
number.rural3 <- data.frame(unlist(lapply(list1, nrow)))

migration.table <- dcast(academic.migration, survey.date ~ identification)
migration.table$numb.rural <- number.rural3[, 1]
migration.table$numb.urban <- number.urban3[, 1]
migration.table$prop.r.to.u.migration <- 
        migration.table$rural.to.urban.migration/
        migration.table$numb.rural
migration.table$prop.u.to.r.migration <- 
        migration.table$urban.to.rural.migration/migration.table$numb.urban
migration.table$ratio.r.over.u <- 
        (migration.table$prop.r.to.u.migration/
                 migration.table$prop.u.to.r.migration)*100



## ----------------------------------------------------------------##

## GOAL: Map the differenet reasons of not attending higher 
## education for individuals from rural and urban areas.

## METHOD: select all respondents who have not lived abroad who
## are currently not matriculated in higher education and never have
## been matriculated in education. 

RNA.higher.edu <- dat0 %>%
        filter(location.prior.address == "urban" | 
                       location.prior.address == "rural" |
                       location.prior.address == "none") %>%
        filter(age %in% ages.of.interest) %>% 
        filter(place.of.birth == "urban" | place.of.birth == "rural" |
                       is.na(place.of.birth)) %>%
        filter(!reason.not.matriculated == "completed") %>%
        filter(higher.education.level == "none" &  
                       currently.matriculated == "no") 

## METHOD: Create a table listing the number of respondents for each 
## reason per area (rural/urban) and survey date. 
RNA.table <- dcast(no.higher.edu, survey.date + current.address ~ reason.not.matriculated)
names(RNA.table) <- gsub(" ", ".", names(RNA.table))

## METHOD: calculate for each reason the percentage of respondents who
## selected it per area (rural/urban) and survey date.

RNA.table[, 3:17] <- (RNA.table[, 3:17]/rowSums(RNA.table[, 3:17]))*100


## GOAL: explore gender gap in education in rural and urban areas. 

GE.table <- dcast(first.years, survey.date + current.address + gender ~ current.address)
GE.table$no.enrolled <- GE.table$urban + GE.table$rural
GE.table <- GE.table %>%
        select(-urban, -rural)
temp.table <- dcast(total, survey.date + current.address + gender ~ current.address)
temp.table$total <- temp.table$urban + temp.table$rural
GE.table$total <- temp.table$total
GE.table$proportion <- (GE.table$no.enrolled/GE.table$total)*100 

## Save data.
saveRDS(PA.table, "past.address.table")
saveRDS(PB.table, "place.of.birth.table")
saveRDS(CA.table, "current.address.table")
saveRDS(RNA.table, "tidy_data_set_RNA.RDS")
saveRDS(migration.table, "academic.migration")
saveRDS(GE.table, "gender_enrolment")
saveRDS()
rm(list = ls())

