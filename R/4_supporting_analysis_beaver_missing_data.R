
library(data.table)    # import data
library(dplyr)         # data manipulation 
library(ggplot2)       # figures
library(here)          # relative files paths
library(lubridate)
library(strings)       # convert_depth_to_ordered_factor() function
library(tgc)           # season functions
library(plotly)


# model results
dat_seasons <- fread(here("results/dat_seasons.csv"), data.table = FALSE)
gap_table <- fread(here("results/gap_table.csv"), data.table = FALSE)
dd <- fread(here("results/dd.csv"), data.table = FALSE)
tgc_table <- fread(here("results/tgc_table.csv"), data.table = FALSE)
dat_filt <- fread(here("results/dat_filt.csv"), data.table = FALSE)

dat <- dat_seasons %>% 
  filter(STATION == "Beaver Point", DEPTH == 2)

plot_temperature_at_depth(dat)

mean(dat$VALUE)

dat_filt <- dat_filt %>% 
  filter(STATION == "Beaver Point", DEPTH == 2)

plot_temperature_at_depth(dat_filt)

mean(dat_filt$VALUE)

# September average
dat %>% 
  filter(TIMESTAMP >= as_datetime("2018-09-01") &
           TIMESTAMP <= as_datetime("2018-09-30")) %>% 
  summarise(MEAN = mean(VALUE))

dat %>% 
  filter(TIMESTAMP >= as_datetime("2018-09-01") &
           TIMESTAMP <= as_datetime("2018-09-30")) %>% 
  plot_temperature_at_depth()

# Week missing data average
dat %>% 
  filter(TIMESTAMP >= as_datetime("2018-09-10") &
           TIMESTAMP <= as_datetime("2018-09-16")) %>% 
  summarise(MEAN = mean(VALUE))



dd <- dd %>% filter(STATION == "Beaver Point")
tgc_table <- tgc_table %>% 
  filter(STATION == "Beaver Point", DEPTH == 2) %>% 
  select(DEPTH, n_growing_days, AVG_TEMPERATURE, n_degree_days, TGC, TGC_INITIAL_WEIGHT)

dd_rm <- count_degree_days(dat, rm_gap_days = TRUE)
tgc_table_rm <- TGC_calculate_initial_weight(
  dd_rm, tgc = c(0.25, 0.3, 0.35), final_weight = 5.5
) %>% 
  filter(DEPTH == 2) %>% 
  select(DEPTH, n_growing_days, AVG_TEMPERATURE, n_degree_days, TGC, TGC_INITIAL_WEIGHT)

tgc_table$AVG_TEMPERATURE - tgc_table_rm$AVG_TEMPERATURE
tgc_table$n_growing_days - tgc_table_rm$n_growing_days
tgc_table$n_degree_days - tgc_table_rm$n_degree_days

tgc_table$TGC_INITIAL_WEIGHT - tgc_table_rm$TGC_INITIAL_WEIGHT




