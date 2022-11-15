# October 14, 2022

# From Cooke Aquaculture (pers comm)
# Average model from Farm Management Plan:
# Smolts 120 - 150 g in May
# Grow out 22 months to 5.5 - 6 kg

# use these numbers to calculate TGC coefficients
# (this would be useful as a dashboard)

# set up ------------------------------------------------------------------

library(dplyr)
library(here)
library(tgc)

# model results -----------------------------------------------------------

results <- readRDS(here("results/model_results.rds"))

dat_raw <- results$dat_raw
tgc_table <- results$tgc_table

# 18 months ---------------------------------------------------------------

t_avg <- round(mean(tgc_table$AVG_TEMPERATURE))

# from smallest w_0 to biggest w_t
TGC_calculate_coefficent(
  initial_weight = 0.12, 
  final_weight = 6,
  average_temp = t_avg,
  n_days = 540
)

# from biggest w_0 to smallest w_t
TGC_calculate_coefficent(
  initial_weight = 0.15, 
  final_weight = 5.5,
  average_temp = t_avg,
  n_days = 540
)

# 22 months ---------------------------------------------------------------

# from smallest w_0 to biggest w_t
TGC_calculate_coefficent(
  initial_weight = 0.12, 
  final_weight = 6,
  average_temp = t_avg,
  n_days = 660
)

# from biggest w_0 to smallest w_t
TGC_calculate_coefficent(
  initial_weight = 0.15, 
  final_weight = 5.5,
  average_temp = t_avg,
  n_days = 660
)

# Re-run results for 22 months  ---------------------------------------------------------
w_t <- 5.5                  # final weight 
tgc <- c(0.25, 0.30, 0.35)  # TGC values

tgc_table2 <- dat_raw %>%
  filter_in_growing_seasons(max_season = 660) %>%
  filter(
    (STATION == "Flat Island" & SEASON == "S3") 
  ) %>%
  count_degree_days(STATION) %>%
  TGC_calculate_initial_weight(final_weight = w_t, tgc = tgc) %>%
  arrange(STATION, DEPTH)

# estimate average growing temperature over 22 months
t_avg2 <- mean(tgc_table2$AVG_TEMPERATURE)


# from smallest w_0 to biggest w_t
TGC_calculate_coefficent(
  initial_weight = 0.12, 
  final_weight = 6,
  average_temp = t_avg2,
  n_days = 660
)

# from biggest w_0 to smallest w_t
TGC_calculate_coefficent(
  initial_weight = 0.15, 
  final_weight = 5.5,
  average_temp = t_avg2,
  n_days = 660
)

# explore temp ------------------------------------------------------------

# from smallest w_0 to biggest w_t
TGC_calculate_coefficent(
  initial_weight = 0.12, 
  final_weight = 6,
  average_temp = 6.5,
  n_days = 660
)



