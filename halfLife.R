#### ON abound ####

### load libraries

library(dplyr)
library(ggplot2)
library(plyr)

### load data, functions, etc.

## get hf (helper functions)
source("~/R/helper_functions.R")



## for when I extend to each comtr
## bee_buf <- tbl(con, dplyr::sql("SELECT * FROM bee.almond_buff_chem_use_4_sum"))

## this will give me comtrs, but I'll save it for later
## tmp <- tbl(con, dplyr::sql("SELECT DISTINCT county_cd || base_ln_mer || township || tship_dir || range || range_dir || section FROM (SELECT * FROM pur.udc LIMIT 400) as comtrs"))
## tmp <- collect(tmp)
## tmp <- unique(tmp)

cyd <- tbl(con, dplyr::sql("SELECT DISTINCT county, year FROM bee.almond_buff_chem_use_bearing_day_sum"))
cyd <- collect(cyd)

## 
addDays <- function(.data){
    feb1 <- paste0(.data$year,"-","2","-","1")
    mar31 <- paste0(.data$year,"-","3","-","31")
    days <- strptime(c(feb1, mar31), "%Y-%m-%d")$yday + 1
    data.frame(county = .data$county,
               year = .data$year,
               day = days[1]:days[2],
               stringsAsFactors = FALSE)
}

cyd <- cyd %>% group_by(county,year) %>%
    do(addDays(.)) %>% ungroup()

## the data set is too large to bring into R, so we'll query it one piece at a
## time by county, year, and day of the bloom season (as day of the  year),
dat <- mlply(cyd, function(county, year, day){
    
    ## do the filtering, collecting:
    sql <- "SELECT * FROM bee.almond_buff_chem_use_bearing_day_sum WHERE"
    sql <- paste0(sql, " county = '", county, "' AND ")
    sql <- paste0(sql, " year = ", year, " AND ")
    sql <- paste0(sql, " dayofyear <= ", day)
    
    d <- tbl(con, dplyr::sql(sql))
    d_c <- collect(d)
    rm(d)

    if(empty(d_c)){
        return(data.frame(
            county = county,
            year = year,
            day = day,
            buff_size = 1:5,
            tot_ld_h2o = NA,
            tot_ld_aer = NA,
            tot_ld_anaer = NA,
            stringsAsFactors = FALSE
            ))
    }
    
    ## add in ld50 and half life info:
    d_c <- left_join(d_c, tox, by = "chem_code")
    d_c <- left_join(d_c, hl, by = c("chem_code" = "chemical_code"))

    ## build new row out of d_c. expected output:
    ## dataframe with 5 rows and 7 columns: county, year, buff_size,
    ##                          total ld50 for each half life type,
    ##                          and day of the season. 
    d_c <- d_c %>%
        ## change half life values
        ## group by the county, year, and buff_size so that they
        ## remain included through the final summary
        group_by(county, year, buff_size) %>%
            ## add in ld50 presence
        dplyr::mutate(
            ## LD_acre_planted = 453.592 * lbs_chem /
            ## (acre_planted * (LD50..ug.bee.1. / (10^6))),
            LD_pres = 453.592 * lbs_chem / (LD50..ug.bee.1. / (10^6))
            ) %>%
            ## add in negative lamdas for each hl (used in exp decay)
        dplyr::mutate(
            neg_lmda_h2o = log(.5) / half_life_in_water,
            neg_lmda_aer = log(.5) / aerobic_half_life_in_soil,
            neg_lmda_anaer = log(.5) / anaerobic_half_life_in_soil
            ) %>%
                ## calculate exponential decay given neg lambda
                ## and the number of days that have passed
        dplyr::mutate(
            num_ld_day_h2o = LD_pres * exp(neg_lmda_h2o *
                (day - dayofyear)),
            num_ld_day_aer = LD_pres * exp(neg_lmda_aer *
                (day - dayofyear)),
            num_ld_day_anaer = LD_pres * exp(neg_lmda_anaer *
                (day - dayofyear))
            ) %>%
                ## sum accross all applications.
        dplyr::summarise(
            tot_ld_h2o = sum(num_ld_day_h2o, na.rm = TRUE),
            tot_ld_aer = sum(num_ld_day_aer, na.rm = TRUE),
            tot_ld_anaer = sum(num_ld_day_anaer, na.rm = TRUE)
            ) %>% ungroup()

    d_c$day <- day
    d_c
}, .progress = "text", .inform = TRUE )

## idk why mdply is throwing errors.
a <- do.call(rbind, dat)
