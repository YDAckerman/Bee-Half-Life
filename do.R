#### ON ABOUND ####

library(dplyr)
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

## load in the data
load("~/R/bee_half_life/data.rda") ## this file is constructed in load.R
load("~/R/bee_half_life/comtrs_data.rda")

dat <- as.data.frame(dat)
data <- as.data.frame(data)

## ggplot(dat %>% filter(buff_size == 5),
##        aes(x = day, y = tot_ld_anaer, group = county, color = county)) +
##     geom_line() +
##     facet_wrap( ~ year)


dat_melt <- melt(dat, id.vars = c("county", "year", "buff_size", "day"))
data_melt <- melt(data, id.vars = c("comtrs", "year", "day", "aer_gnd_ind"))

ggplot(dat_melt %>% filter(buff_size == 5),
       aes(x = day,
           y = value,
           group = county,
           color = county
           )) +
    geom_line() +
    facet_grid(year ~ variable)

a <- llply(c("A","G","O"), function(x){
    ggplot(data_melt %>% dplyr::filter(aer_gnd_ind == x & !is.na(value)),
           aes(x = day,
               y = value,
               group = comtrs)) +
                   geom_line() +
                       facet_grid(year ~ variable) +
                           ggtitle(x)
})

do.call(grid.arrange, a)

data %>%
    filter(!is.na(tot_ld_h2o)) %>%
    select(comtrs) %>%
    distinct(comtrs) %>%
    summarise(num = length(comtrs))

ggplot(data %>% filter(!is.na(tot_ld_h2o)), aes(x = day, y = tot_ld_h2o, group = comtrs)) +
    geom_line()+
    facet_grid(year ~ aer_gnd_ind)


data_melt %>% group_by(year, aer_gnd_ind, variable) %>%
    summarise(num = sum(!is.na(value)))


ggplot(data_melt %>% filter(year == 2011 & aer_gnd_ind == "G" & variable == "tot_ld_aer" & day == 60), aes(x = value)) + geom_density()
