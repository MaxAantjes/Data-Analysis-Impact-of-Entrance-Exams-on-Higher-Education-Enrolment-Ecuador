
library(openxlsx, stringr)
dat0 <- read.xlsx("Registros-Administrativos-2019-2020-Inicio(1).xlsx", sheet = "Registros Administrativos", startRow = 13)

y2009s <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10555"
y2010s <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10557"
y2011s <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10559"
y2012s <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10561"
y2013s <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=12176"
y2014s <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10567"
y2015s <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10569"
y2016s <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10573"

y2009e <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10556" 
y2010e <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10558"
y2011e <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10560"
y2012e <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10562"
y2013e <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=12177"
y2014e <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10568"
y2015e <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=10571"
y2016e <- "https://educacion.gob.ec/wp-content/plugins/download-monitor/download.php?id=12205"

links <- list(y2009s, y2009e, y2010s, y2010e, y2011s, y2011e, y2012s, y2012e, 
              y2013s, y2013e, y2014s, y2014e, y2015s, y2015e, y2016s, y2016e)

load.xlsx <- function(x) {

        y <- read.xlsx(x, sheet = "Informe", startRow = 11)

        return(y)
        
}

dat0 <- lapply(links, load.xlsx)

tidy.data <- function(x) {
        
        a <- x$Periodo
        
        x <- x %>%
                select(Cod_Parroquia, Total.Docentes, Total.Estudiantes)%>%
                filter(Total.Docentes > 0) %>% 
                filter(Total.Estudiantes > 0) %>%
                na.omit %>% 
                mutate(teacher.student.ratio = Total.Docentes/Total.Estudiantes)%>%      
                group_by(Cod_Parroquia) %>%                         
                summarise_at(vars(teacher.student.ratio),
                             list(mean.teacher.student.ratio = mean)) %>%
                mutate(period = 
                               str_extract(a[1], 
                                           "[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]")) %>%
                mutate(measurement.point = 
                               str_remove(a[1], 
                                          "[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9] +")) %>%
                select(Cod_Parroquia, period, measurement.point, mean.teacher.student.ratio)
        
        names(x)[1] <- "postcode"
        return(x)
        
}

dat1 <- lapply(dat0, tidy.data)

dat2 <- do.call(rbind, dat1)
dat2 <- dat2 %>%
        group_by(postcode, period) %>%
        summarise(mean.teacher.student.ratio = mean(mean.teacher.student.ratio))



