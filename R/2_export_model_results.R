# January 14, 2021

library(data.table)    # import example data
library(dplyr)         # data manipulation 
library(glue)
library(here)          # relative files paths
library(lubridate)
library(strings)       # convert_depth_to_ordered_factor() function
library(tgc)           # season functions
library(tidyr)

# import data
dat_raw <- import_strings_data(
  add_county_col = FALSE,
  county = c("Halifax", "Guysborough_Dover Bay", "Lunenburg")
) %>%
  filter(VARIABLE == "Temperature") %>% 
  select(-WATERBODY, -LEASE, -VARIABLE, -DEPLOYMENT_PERIOD, 
         -UNITS, -MOORING) %>% 
  filter(
    STATION == "Flat Island" | 
      STATION == "Beaver Point" | 
      STATION == "Madeline Point"
  ) %>% 
  # fix Flat Island data (Vemco was at 22 m; remove 20 m HOBO that was only in 2 deployments)
  separate(SENSOR, into = c("SENSOR", NA), sep = "-") %>% 
  filter(!(STATION == "Flat Island" & DEPTH == '20' & SENSOR == "HOBO")) %>% 
  select(-SENSOR) %>% 
  mutate(
    DEPTH = if_else(STATION == "Flat Island" & DEPTH == "20", "22", DEPTH)
  ) %>% 
  convert_depth_to_ordered_factor() %>% 
  mutate(
    STATION = ordered(
      STATION, levels = c("Madeline Point", "Beaver Point", "Flat Island")
    )
  ) 


# Analysis -------------------------------------------------------------

# CAN send this to count_degree_days() (but NOT if heat stress events are filtered)
dat_seasons <-  dat_raw %>% 
  filter_in_growing_seasons() %>% 
  filter(
    (STATION == "Flat Island" & SEASON == "S3") |
      (STATION == "Beaver Point" & SEASON == "S2") |
      (STATION == "Madeline Point" & SEASON == "S2")
  ) 

# check for data gaps 
gap_table <- dat_seasons %>% 
  check_for_data_gaps(STATION) %>% 
  arrange(STATION, DEPTH) %>% 
  group_by(STATION, DEPTH) %>%
  summarize(TOTAL_GAP_DAYS = sum(GAP_LENGTH_DAYS)) %>%
  ungroup()

# degree days
dd <- count_degree_days(dat_seasons, STATION, rm_gap_days = FALSE) %>% 
  select(-SEASON, -n_OBSERVATIONS) %>% 
  relocate(AVG_TEMPERATURE, .before = n_degree_days)

# TGC model
w_t <- 5.5
tgc <- c(0.25, 0.3, 0.35)

tgc_table <- TGC_calculate_initial_weight(dd, final_weight = w_t, tgc = tgc) %>%
  arrange(STATION, DEPTH) %>% 
  select(-FINAL_WEIGHT)

# Full TGC pipeline
# tgc_table <- dat_raw %>% 
#   filter_in_growing_seasons() %>% 
#   filter(
#     (STATION == "Flat Island" & SEASON == "S3") |
#       (STATION == "Beaver Point" & SEASON == "S2") |
#       (STATION == "Madeline Point" & SEASON == "S2")
#   ) %>% 
#   count_degree_days(STATION) %>% 
#   TGC_calculate_initial_weight(final_weight = w_t, tgc = tgc) %>% 
#   arrange(STATION, DEPTH)

# filter out heat stress events for plotting (do NOT send this to count_dd)
dat_filt <- dat_seasons %>% 
  filter_out_heat_stress_events() 


# Export results ----------------------------------------------------------

results <- list(
  dat_raw = dat_raw,
  dat_seasons = dat_seasons,
  dat_filt = dat_filt,
  gap_table = gap_table,
  dd = dd,
  tgc_table = tgc_table
)

saveRDS(results, here("results/model_results.rds"))


# fwrite(dat_raw, here("results/dat_raw.csv"))
# fwrite(dat_seasons, here("results/dat_seasons.csv"))
# fwrite(dat_filt, here("results/dat_filt.csv"))
# 
# fwrite(gap_table, here("results/gap_table.csv"))
# fwrite(dd, here("results/dd.csv"))
# fwrite(tgc_table, here("results/tgc_table.csv"))






