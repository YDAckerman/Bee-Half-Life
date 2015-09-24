## Functions for the Bee-half-life project

source("~/R/helper_functions.R")

addDays <- function(.data){
    ## takes as input a data frame of shape (1,2)
    ## with column 1 being year and column 2 being
    ## either county, comtrs, or something else relevant
    ## returns a data frame of shape (#-days-in-bloom, 3) with
    ## columns county, year, and day.
    
    names = colnames(.data)
    feb1 <- paste0(.data[, names[1]],"-","2","-","1")
    mar31 <- paste0(.data[, names[1]],"-","3","-","31")
    days <- strptime(c(feb1, mar31), "%Y-%m-%d")$yday + 1
    tmp <- data.frame(var = .data[,names[2]],
               year = .data[,names[1]],
               day = days[1]:days[2],
               stringsAsFactors = FALSE)
    colnames(tmp)[1] <- names[2]
    tmp
}

