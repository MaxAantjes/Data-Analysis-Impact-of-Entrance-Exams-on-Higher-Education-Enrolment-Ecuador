## This module visualises the data. 

library(pacman)
pacman::p_load(ggplot2, zoo, dplyr)

PA.table <- readRDS("past.address.table.csv")
PB.table <- readRDS("place.of.birth.table.csv")
CA.table <- readRDS("current.address.table.csv")
migration.table <- readRDS("academic.migration.csv")

## GOAL: Create plot comparing rural.urban.ratio for each method.

## METHOD: Select columns of interest for each table. 

PA.table.ratio <- PA.table %>%
        select(survey.date, PA.rural.urban.ratio)
CA.table.ratio <- CA.table %>%
        select(survey.date, CA.rural.urban.ratio)
PB.table.ratio <- PB.table %>%
        select(survey.date, PB.rural.urban.ratio)

## METHOD: merge tables into one table. 
combined.ratio <- merge(PA.table.ratio, CA.table.ratio)
combined.ratio <- merge(combined.ratio, PB.table.ratio, all.x = TRUE)

## METHOD: create a plot.

comb.ratio.plot <- ggplot(combined.ratio, aes(survey.date, y = population, color = methodology)) + 
        geom_line(aes(y = PA.rural.urban.ratio, col = "PA method")) + 
        geom_point(aes(y = PA.rural.urban.ratio, col = "PA method")) +
        geom_line(aes(y = CA.rural.urban.ratio, col = "CA method")) +
        geom_point(aes(y = CA.rural.urban.ratio, col = "CA method")) +
        geom_line(aes(y = PB.rural.urban.ratio, col = "PB method")) + 
        geom_point(aes(y = PB.rural.urban.ratio, col = "PB method")) +
        labs(title = "Ratio of the proportion of rural and urban population enrolled as first year undergraduate students", 
             subtitle = "Population 18-24 y/o. Period Dec 2007 to Sep 2019.",
             x ="survey date", y ="rural:urban enrolment ratio in %")

print(comb.ratio.plot)

## GOAL: Create a plot for rural proportions merged for different methods.

PA.table.prop <- PA.table %>%
        select(survey.date, PA.prop.rural)
CA.table.prop <- CA.table %>%
        select(survey.date, CA.prop.rural)
PB.table.prop <- PB.table %>%
        select(survey.date, PB.prop.rural)

## METHOD: merge tables into one table. 
combined.prop.rur <- merge(PA.table.prop, CA.table.prop)
combined.prop.rur <- merge(combined.prop.rur, PB.table.prop, all.x = TRUE)

comb.prop.rur.plot <- ggplot(combined.prop.rur, aes(survey.date, y = population, color = methodology)) + 
        geom_line(aes(y = PA.prop.rural, col = "PA method")) + 
        geom_point(aes(y = PA.prop.rural, col = "PA method")) +
        geom_line(aes(y = CA.prop.rural, col = "CA method")) +
        geom_point(aes(y = CA.prop.rural, col = "CA method")) +
        geom_line(aes(y = PB.prop.rural, col = "PB method")) + 
        geom_point(aes(y = PB.prop.rural, col = "PB method")) +
        labs(title = "Proportion of rural population enrolled as first year undergraduate students", 
             subtitle = "Population 18-24 y/o. Period Dec 2007 to Sep 2019.",
             x ="survey date", y ="proportion") + coord_cartesian(ylim = c(0, 0.09))
print(comb.prop.rur.plot)



## GOAL: Create a plot for urban proportions merged for different methods.

PA.table.prop <- PA.table %>%
        select(survey.date, PA.prop.urban)
CA.table.prop <- CA.table %>%
        select(survey.date, CA.prop.urban)
PB.table.prop <- PB.table %>%
        select(survey.date, PB.prop.urban)

## METHOD: merge tables into one table. 
combined.prop.urb <- merge(PA.table.prop, CA.table.prop)
combined.prop.urb <- merge(combined.prop.urb, PB.table.prop, all.x = TRUE)

comb.prop.urb.plot <- ggplot(combined.prop.urb, aes(survey.date, y = population, color = methodology)) + 
        geom_line(aes(y = PA.prop.urban, col = "PA method")) + 
        geom_point(aes(y = PA.prop.urban, col = "PA method")) +
        geom_line(aes(y = CA.prop.urban, col = "CA method")) +
        geom_point(aes(y = CA.prop.urban, col = "CA method")) +
        geom_line(aes(y = PB.prop.urban, col = "PB method")) + 
        geom_point(aes(y = PB.prop.urban, col = "PB method")) +
        labs(title = "Proportion of urban population enrolled as first year undergraduate students", 
             subtitle = "Population 18-24 y/o. Period Dec 2007 to Sep 2019.",
             x ="survey date", y ="proportion") + coord_cartesian(ylim = c(0, 0.09))
print(comb.prop.urb.plot)


library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(comb.prop.urb.plot), ggplotGrob(comb.prop.rur.plot), ggplotGrob(comb.ratio.plot)))


