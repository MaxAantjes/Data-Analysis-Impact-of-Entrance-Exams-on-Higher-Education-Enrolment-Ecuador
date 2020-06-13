## This module visualises the data. 

library(pacman)
pacman::p_load(ggplot2)

PA.table <- readRDS("past.address.table.csv")
PB.table <- readRDS("place.of.birth.table.csv")
CA.table <- readRDS("current.address.table.csv")

## GOAL: Create plot and regression model for CURRENT ADDRESS
## method. 
CA.plot <- ggplot(CA.table, aes(x=survey.date, y=ratio)) + geom_point(size = 2) + 
        labs(title = "Ratio of weighed proportions of rural/ urban population enrolled as first year undergraduate students", 
                          subtitle = "Rural and urban identification by current address. Population 17-39 y/o.",
                          x ="survey date", y ="proportion ratio")                                        
                                           
mod0 <- lm(ratio ~ survey.date, data= CA.table) 
summary(mod0)
CA.plot2 <- CA.plot + geom_smooth(method = "lm", 
                                             col = "blue")


## GOAL: Create plot and regression model for PLACE OF BIRTH
## method. 
PB.plot <- ggplot(PB.table, aes(x=survey.date, y=ratio)) + geom_point(size = 2) + 
        labs(title = "Ratio of weighed proportions of rural/ urban population enrolled as first year undergraduate students", 
             subtitle = "Rural and urban identification by place of birth. Population 17-39 y/o.",
             x ="survey date", y ="proportion ratio")                                        

mod1 <- lm(ratio ~ survey.date, data= PB.table) 
summary(mod1)
PB.plot2 <- PB.plot + geom_smooth(method = "lm", 
                                              col = "blue")

## GOAL: Create plot and regression model for CURRENT ADDRESS
## and REASON FOR MIGRATION method. 
PA.plot <- ggplot(PA.table, aes(x=survey.date, y=ratio)) + geom_point(size = 2) + 
        labs(title = "Ratio of weighed proportions of rural/ urban population enrolled as first year undergraduate students", 
             subtitle = "Rural and urban identification by current address and reasons for migration. Population 17-39 y/o.",
             x ="survey date", y ="proportion ratio") 

mod2 <- lm(ratio ~ survey.date, data= PA.table) 
summary(mod2)
PA.plot2 <- PA.plot + geom_smooth(method = "lm", 
                                  col = "blue")

