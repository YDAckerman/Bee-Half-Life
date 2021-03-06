#### ON abound ####
### load libraries
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(plyr)
library(foreach)
library(lubridate)


### load functions
source("~/R/bee_half_life/func.R")

## connect to the database
con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   ## ,options="-c search_path=bee"
                    )

## tell it what to do:
county_wide <- FALSE
comtrs_wide <- FALSE

## get halflife info
hl <- read.csv("~/Bee_Data/pesticide_half_life.csv", stringsAsFactors = FALSE)

## prep hl data
hl$half_life_in_water <- gsub("persistent", "10000", hl$half_life_in_water)
hl$aerobic_half_life_in_soil <- gsub("persistent", "10000",
                                     hl$aerobic_half_life_in_soil)
hl$anaerobic_half_life_in_soil <- gsub("persistent", "10000",
                                       hl$anaerobic_half_life_in_soil)

hl$half_life_in_water <- gsub("unstable", ".00001", hl$half_life_in_water)
hl$aerobic_half_life_in_soil <- gsub("unstable", ".00001",
                                     hl$aerobic_half_life_in_soil)
hl$anaerobic_half_life_in_soil <- gsub("unstable", ".00001",
                                       hl$anaerobic_half_life_in_soil)

hl$half_life_in_water <- as.numeric(hl$half_life_in_water)
hl$aerobic_half_life_in_soil <- as.numeric(hl$aerobic_half_life_in_soil)
hl$anaerobic_half_life_in_soil <- as.numeric(hl$anaerobic_half_life_in_soil)

## get toxicity info
tox <- read.csv("~/Bee_Data/AI-BeeToxicity.csv", stringsAsFactors = FALSE)


if (county_wide){
    ## create the skeleton for the final dataframe
    cyd <- tbl(con, dplyr::sql("SELECT DISTINCT year, county FROM bee.almond_buff_chem_use_bearing_day_sum"))
    cyd <- collect(cyd)
    cyd <- cyd %>% group_by(year, county) %>%
        do(addDays(.)) %>% ungroup()

    ## fill the skeleton:
    ## the data set is too large to bring into R, so we'll query it one piece at a
    ## time by county, year, and day of the bloom season (as day of the  year),
    dat <- mlply(cyd, function(county, year, day){

        ## convert the day + year combo into a date object
        daysdate <- as.Date(format(strptime(paste(year, day), "%Y %j"),
                                   format = "%Y-%m-%d"))
        
        ## do the filtering, collecting:
        sql <- "SELECT * FROM bee.almond_buff_chem_use_bearing_day_sum WHERE"
        sql <- paste0(sql, " county = '", county, "' AND ")
        sql <- paste0(sql, " ((year = ", year, " AND ")
        sql <- paste0(sql, " dayofyear <= ", day, ")")
        sql <- paste0(sql, " OR (year = ", year - 1, " AND ")
        sql <- paste0(sql, " month = 12))")
        ## the initial problem with the above is demonstrated by:
        ## imagine the year is 2011 and the day is 63. The above only
        ## pulled data from days 1-63 of year 2011, leaving out all of
        ## december. With the corrections, december is included.
        
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

        d_c <- d_c %>%
            mutate(
                dateOfApp = as.Date(format(strptime(
                    paste(year, dayofyear), "%Y %j"), format = "%Y-%m-%d"))
                )
        
        d_c$year <- year
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
                                        (as.numeric(daysdate - dateOfApp))),
                                    num_ld_day_aer = LD_pres * exp(neg_lmda_aer *
                                        (as.numeric(daysdate - dateOfApp))),
                                    num_ld_day_anaer = LD_pres * exp(neg_lmda_anaer *
                                        (as.numeric(daysdate - dateOfApp)))
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
    dat <- do.call(rbind, dat)

    save(dat, file = "~/R/bee_half_life/data.rda") 
}

################################################################################
## Now for individual Comtrs
################################################################################

## 9-21-15 - Let's just look at the comtrs in the hardest hit counties (since
## otherwise I don't think I can justify the amount of time this will take)

## query pur.udc to get all applications within these sections during the target
## time window (dec-mar)

if (comtrs_wide){

    sql <- "SELECT * FROM pur.udc WHERE comtrs IN (SELECT DISTINCT comtrs FROM bee.almond_sections_bearing WHERE year = 2011 OR year = 2012 OR year = 2013) AND (year = 2010 OR year = 2011 OR year = 2012 OR year = 2013) AND ((applic_dt,applic_dt) OVERLAPS (DATE '2010-12-01', DATE '2011-03-31') OR (applic_dt,applic_dt) OVERLAPS (DATE '2011-12-01', DATE '2012-03-31') OR (applic_dt,applic_dt) OVERLAPS (DATE '2012-12-01', DATE '2013-03-31')) AND (county_cd = '15' OR county_cd = '10')"

    ## TODO: consider adding in december of 2013 as a measure of presence in
    ## 2014.

    comtrs_data <- tbl(con, dplyr::sql(sql))
    comtrs_data <- comtrs_data %>% select(year, chem_code,
                                          lbs_chm_used, acre_planted,
                                          acre_treated, applic_cnt,
                                          applic_dt, comtrs,
                                          aer_gnd_ind, site_code)
    comtrs_data <- as.data.frame(collect(comtrs_data))
    comtrs_data <- comtrs_data %>%
        mutate(dayofyear = strptime(applic_dt, "%Y-%m-%d")$yday + 1)

    ## create the skeleton for the final dataframe
    cyd <- comtrs_data %>%
        dplyr::select(year, comtrs) %>%
            dplyr::distinct(year, comtrs) %>%
                dplyr::group_by(year, comtrs) %>%
                    dplyr::do(addDays(.)) %>%
                        ungroup()

    ## get the chemicals the epa is concerned with so that we can filter out extraneous
    ## chems later
    sql <- "SELECT DISTINCT chem_code FROM bee.almond_buff_chem_use_4_sum"
    epa_chems <- tbl(con, dplyr::sql(sql))
    epa_chems <- as.data.frame(collect(epa_chems))

    ## fill in the skeleton
    data <- mlply(cyd, function(comtrs, year, day){

        comtr <- comtrs
        daysdate <- as.Date(format(strptime(paste(year, day), "%Y %j"),
                                   format = "%Y-%m-%d"))
        startdate <- as.Date(paste0(year - 1, "-12-01"))

        ## do the filtering, collecting:
        ## pull out all applications within the given year
        d_c <- comtrs_data %>% dplyr::filter(comtrs == comtr & 
                                             chem_code %in% epa_chems$chem_code &
                                             applic_dt %within%
                                             interval(startdate, daysdate))
        ## the initial problem with the above is demonstrated by:
        ## imagine the year is 2011 and the day is 63. The above only
        ## pulled data from days 1-63 of year 2011, leaving out all of
        ## december. With the corrections, december is included. yikes, but
        ## so are some other things... Nope - now it's been fixed.

        if(empty(d_c)){
            return(data.frame(
                comtrs = comtr,
                year = year,
                day = day,
                aer_gnd_ind = c("A","O","G","F"),
                tot_ld_h2o = NA,
                tot_ld_aer = NA,
                tot_ld_anaer = NA,
                stringsAsFactors = FALSE
                ))
        }
        
        ## add in ld50 and half life info:
        d_c <- left_join(d_c, tox, by = "chem_code")
        d_c <- left_join(d_c, hl, by = c("chem_code" = "chemical_code"))
        d_c$year <- year

        ## build new row out of d_c. expected output:
        ## dataframe with 5 rows and 7 columns: county, year, use indicator,
        ##                          total ld50 for each half life type,
        ##                          and day of the season. 
        d_c <- d_c %>%
            ## change half life values
            ## group by the county, year, and buff_size so that they
            ## remain included through the final summary
            group_by(comtrs, year, aer_gnd_ind) %>%
                ## add in ld50 presence
                dplyr::mutate(
                    ## LD_acre_planted = 453.592 * lbs_chem /
                    ## (acre_planted * (LD50..ug.bee.1. / (10^6))),
                    LD_pres = 453.592 * lbs_chm_used / (LD50..ug.bee.1. / (10^6))
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
                                        (as.numeric(daysdate - applic_dt))),
                                    num_ld_day_aer = LD_pres * exp(neg_lmda_aer *
                                        (as.numeric(daysdate - applic_dt))),
                                    num_ld_day_anaer = LD_pres * exp(neg_lmda_anaer *
                                        (as.numeric(daysdate - applic_dt)))
                                    ) %>%
                                        ## sum accross all applications.
                                        dplyr::summarise(
                                            tot_ld_h2o = sum(num_ld_day_h2o,
                                                na.rm = TRUE),
                                            tot_ld_aer = sum(num_ld_day_aer,
                                                na.rm = TRUE),
                                            tot_ld_anaer = sum(num_ld_day_anaer,
                                                na.rm = TRUE)
                                            ) %>% ungroup()

        d_c$day <- day
        d_c
    }, .progress = "text")

    data <- rbind_all(data)
    save(data, file = "~/R/bee_half_life/comtrs_data.rda")
}
    ## this will give me comtrs, but I'll save it for later
    ## tmp <- tbl(con, dplyr::sql("SELECT DISTINCT county_cd || base_ln_mer || township || tship_dir || range || range_dir || section FROM (SELECT * FROM pur.udc LIMIT 400) as comtrs"))
    ## tmp <- collect(tmp)
    ## tmp <- unique(tmp)
