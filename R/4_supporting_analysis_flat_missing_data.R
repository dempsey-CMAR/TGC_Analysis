
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


dat_seasons %>%  
  filter(STATION == "Flat Island") %>% 
  check_for_data_gaps()


dat <- dat_seasons %>% 
  filter(STATION == "Flat Island") %>% 
  filter_out_heat_stress_events()

plot_temperature_at_depth(dat)


gap_start <- as_datetime("2019-06-04")
gap_end <- as_datetime("2019-06-05")

dat_filt <- dat %>% 
  filter(TIMESTAMP >= gap_start - days(14),
         TIMESTAMP <= gap_start + days(14))

plot_temperature_at_depth(dat_filt)



# June Gap ----------------------------------------------------------------

# Average
avg_all <- dat %>% 
  filter(DEPTH != 20) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE))

avg_filt <- dat_filt %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE))

avg_all$MEAN - avg_filt$MEAN



# November Gap ------------------------------------------------------------

gap_start <- as_datetime("2019-11-17")
gap_end <- as_datetime("2019-11-17") + days(4)

dat_filt <- dat %>% 
  filter(TIMESTAMP >= gap_start - days(14),
         TIMESTAMP <= gap_start + days(14))

plot_temperature_at_depth(dat_filt)


# Average
avg_all <- dat %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE))


avg_filt <- dat_filt %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE))

avg_all$MEAN - avg_filt$MEAN


# Bottom Temp Interval ----------------------------------------------------

dat <- dat_seasons %>% 
  filter(STATION == "Flat Island", DEPTH == 20) %>% 
  arrange(TIMESTAMP) %>% 
  mutate(
    DIFF = round(as.numeric(difftime(lead(TIMESTAMP), TIMESTAMP, unit = "min")))
    ) %>% 
  na.omit()

ggplot(dat, aes(TIMESTAMP, DIFF)) +
  geom_point(alpha = 0.5)



dat %>% 
  filter(DIFF < -50000)

dat_check <- dat %>% 
  filter(TIMESTAMP >= as_datetime("2019-11-17"), 
         TIMESTAMP <= as_datetime("2019-11-18"))






