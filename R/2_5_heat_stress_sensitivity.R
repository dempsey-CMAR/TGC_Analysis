# June 3, 2022

# sensitivity to heat stress threshold

library(dplyr)         # data manipulation 
library(ggplot2)       # figures
library(here)          # relative files paths
library(lubridate)
library(strings)       # convert_depth_to_ordered_factor() function
library(tgc)           # season functions
library(tidyr)
library(viridis)


theme_set(theme_light())

# model results
results <- readRDS(here("results/model_results.rds"))

dat_seasons <- results$dat_seasons
tgc_table <- results$tgc_table


colour_pal <- viridis(5, direction = -1)

w_t <- 5.5
tgc = c(0.25, 0.3, 0.35)


# Original Model ----------------------------------------------------------

tgc_table_18 <- tgc_table %>% 
  select(
    STATION, DEPTH, STOCKED_DAYS, TGC,
    filtered_18 = n_filtered_days,
    n_growing_18 = n_growing_days,
    T_avg_18 = AVG_TEMPERATURE,
    DD_18 = n_degree_days,
    weight_18 = TGC_INITIAL_WEIGHT
  ) 

# Strict Model ------------------------------------------------------------

tgc_table_16 <- dat_seasons %>%
  count_degree_days(STATION, heat_threshold = 16) %>%
  TGC_calculate_initial_weight(final_weight = w_t, tgc = tgc) %>%
  select(
    STATION, DEPTH, STOCKED_DAYS, TGC,
    filtered_16 = n_filtered_days,
    n_growing_16 = n_growing_days,
    T_avg_16 = AVG_TEMPERATURE,
    DD_16 = n_degree_days,
    weight_16 = TGC_INITIAL_WEIGHT
  )

ggplot(tgc_table_16, 
       aes(x = factor(TGC), y = weight_16, fill = DEPTH)) +
  geom_point(
    pch = 21, size = 3, alpha = 0.75,
    position = position_jitter(width = 0, height = 0, seed = 10)
  ) +
  scale_fill_manual("Depth (m)", values = colour_pal, drop = FALSE) +
  scale_x_discrete("TGC Value") +
  scale_y_continuous("Initial Weight (kg)", limits = c(0, 3.5)) +
  facet_wrap(~STATION) +
  labs(title = "16 degrees")



# Relaxed Model -----------------------------------------------------------

tgc_table_20 <- dat_seasons %>%
  count_degree_days(STATION, heat_threshold = 20) %>%
  TGC_calculate_initial_weight(final_weight = w_t, tgc = tgc) %>%
  select(
    STATION, DEPTH, STOCKED_DAYS, TGC,
    filtered_20 = n_filtered_days,
    n_growing_20 = n_growing_days,
    T_avg_20 = AVG_TEMPERATURE,
    DD_20 = n_degree_days,
    weight_20 = TGC_INITIAL_WEIGHT
  )


ggplot(tgc_table_20, 
       aes(x = factor(TGC), y = weight_20, fill = DEPTH)) +
  geom_point(
    pch = 21, size = 3, alpha = 0.75,
    position = position_jitter(width = 0, height = 0, seed = 10)
  ) +
  scale_fill_manual("Depth (m)", values = colour_pal, drop = FALSE) +
  scale_x_discrete("TGC Value") +
  scale_y_continuous("Initial Weight (kg)", limits = c(0, 3.5)) +
  facet_wrap(~STATION) +
  labs(title = "20 degrees")


# Comparison --------------------------------------------------------------

# strict
tgc_comp_16 <- tgc_table_16 %>% 
  left_join(
    tgc_table_18, by = c("STATION", "DEPTH", "STOCKED_DAYS", "TGC")
  ) %>% 
  select(STATION, DEPTH, TGC, contains(c("DD", "filtered", "n_growing", "T_avg", "weight"))) %>% 
  mutate(
    DIFF_growing = n_growing_18 - n_growing_16,
    PERCENT_DIFF_growing = 100 * (n_growing_18 - n_growing_16) / n_growing_18,
    DIFF_dd = DD_18 - DD_16,
    PERCENT_DIFF_dd = 100 * (DD_18 - DD_16) / DD_18,
    DIFF_weight = weight_18 - weight_16,
    PERCENT_DIFF_weight = 100 * (weight_18 - weight_16) / weight_18
  )


# relaxed
tgc_comp_20 <- tgc_table_20 %>% 
  left_join(
    tgc_table_18, by = c("STATION", "DEPTH", "STOCKED_DAYS", "TGC")
  ) %>% 
  select(STATION, DEPTH, TGC, contains(c("DD", "filtered", "n_growing", "T_avg", "weight"))) %>% 
  mutate(
    DIFF_growing = n_growing_18 - n_growing_20,
    PERCENT_DIFF_growing = 100 * (n_growing_18 - n_growing_20) / n_growing_18,
    DIFF_dd = DD_18 - DD_20,
    PERCENT_DIFF_dd = 100 * (DD_18 - DD_20) / DD_18,
    DIFF_weight = weight_18 - weight_20,
    PERCENT_DIFF_weight = 100 * (weight_18 - weight_20) / weight_18
  )


# export sensitivity results ----------------------------------------------

sensitivity_results <- list(
  tgc_table_16 = tgc_table_16,
  tgc_table_20 = tgc_table_20,
  
  tgc_comp_16 = tgc_comp_16,
  tgc_comp_20 = tgc_comp_20
)

saveRDS(sensitivity_results, here("results/sensitivity_results.rds"))






