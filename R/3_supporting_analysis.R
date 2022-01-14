
library(data.table)    # import example data
library(dplyr)         # data manipulation 
library(DT)            # interactive tables
library(ggplot2)       # figures
library(ggsflabel)
library(ggspatial)
library(glue)
library(here)          # relative files paths
library(leaflet)       # interactive map   
library(lubridate)
library(sf)            # static map for paper
library(strings)       # convert_depth_to_ordered_factor() function
library(tgc)           # season functions
library(plotly)

fig_width <- 19
fig_height <- 7

# import data
dat_raw <- import_strings_data(
  add_county_col = FALSE,
  county = c("Halifax", "Guysborough_Dover Bay", "Lunenburg")
) %>%
  filter(VARIABLE == "Temperature") %>% 
  select(-WATERBODY, -LEASE, -VARIABLE, -SENSOR, -DEPLOYMENT_PERIOD, 
         -UNITS, -MOORING) %>% 
  filter(
    STATION == "Flat Island" | 
      STATION == "Beaver Point" | 
      STATION == "Madeline Point"
  ) %>% 
  convert_depth_to_ordered_factor() %>% 
  mutate(
    STATION = ordered(
      STATION, levels = c("Madeline Point", "Beaver Point", "Flat Island")
    )
  )


colour_pal <- get_colour_palette(dat_raw)


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
  check_for_data_gaps(STATION, SEASON) %>% 
  arrange(STATION, SEASON, DEPTH) %>% 
  group_by(STATION, SEASON, DEPTH) %>%
  summarize(TOTAL_GAP_DAYS = sum(GAP_LENGTH_DAYS)) %>%
  ungroup()

# # summary table - make this a function
# seasons_table <- dat_seasons %>% 
#   group_by(STATION, SEASON, DEPTH) %>% 
#   summarise(START_SEASON = min(TIMESTAMP),
#             END_SEASON = max(TIMESTAMP)) %>% 
#   ungroup() %>% 
#   mutate(
#     SEASON_DAYS = difftime(END_SEASON, START_SEASON, units = "days"),
#     SEASON_DAYS = as.numeric(round(SEASON_DAYS, digits = 2)),
#     SEASON_MONTHS = round(SEASON_DAYS / 30, digits = 2)
#   ) %>% 
#   arrange(STATION,  DEPTH)


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

# filter out heat stress events for plotting (do NOT send this to count_dd)
dat_filt <- dat_seasons %>% 
  filter_out_heat_stress_events() 


# Madeline Point ----------------------------------------------------------

short <- dat_seasons %>% 
  filter(STATION == "Madeline Point")

plot_temperature_at_depth(short,
                          date_breaks_major = "1 month",
                          date_labels_format = "%Y-%b")

ggsave(
  filename = "madeline_all.png",
  path = here("figs/supporting"),
  device = "png",
  width = fig_width, height = fig_height, units = "cm",
  dpi = 600
)

short %>% 
  filter(DEPTH == 2 | DEPTH == 22) %>% 
  plot_temperature_at_depth(date_breaks_major = "1 month",
                            date_labels_format = "%Y-%b") 

ggsave(
  filename = "madeline_shallow_deep.png",
  path = here("figs/supporting"),
  device = "png",
  width = fig_width, height = fig_height, units = "cm",
  dpi = 600
)

p <- short %>% 
  filter(TIMESTAMP > as_datetime("2019-07-15") &
           TIMESTAMP < as_datetime("2019-09-15")) %>% 
  plot_temperature_at_depth() +
  scale_y_continuous(name = "Temperature")+
  geom_hline(yintercept = 18, col = "red")

ggplotly(p)

ggsave(
  p,
  filename = "madeline_warm.png",
  path = here("figs/supporting"),
  device = "png",
  width = fig_width, height = fig_height, units = "cm",
  dpi = 600
)

short %>% 
  mutate(TIMESTAMP = TIMESTAMP - hours(3)) %>% 
  filter(TIMESTAMP > as_datetime("2019-09-05") &
           TIMESTAMP < as_datetime("2019-09-10")) %>% 
  plot_temperature_at_depth(date_breaks_major = "1 day")

ggsave(
  filename = "madeline_Dorian-ts_corrected.png",
  path = here("figs/supporting"),
  device = "png",
  width = fig_width, height = fig_height, units = "cm",
  dpi = 600
)

dat_filt %>% 
  filter(STATION == "Madeline Point") %>% 
  plot_temperature_at_depth()

dat_filt %>% 
  filter(STATION == "Madeline Point") %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE))

# average temp from July to mid- Sept
short_summer_stats <- short %>% 
  filter(
    TIMESTAMP >= as_datetime("2019-07-01") & 
      TIMESTAMP <= as_datetime("2019-08-31")
  ) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE),
            SD = sd(VALUE),
            MIN = min(VALUE),
            MAX = max(VALUE)) %>% 
  ungroup() %>% 
  mutate(across(2:5, .fns = ~round(., digits = 2)))

short_winter_stats <- short %>% 
  filter(
    TIMESTAMP >= as_datetime("2019-09-15") &
      TIMESTAMP < as_datetime("2020-02-15")
  ) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE),
            SD = sd(VALUE),
            MIN = min(VALUE),
            MAX = max(VALUE)) %>% 
  ungroup() %>% 
  mutate(across(2:5, .fns = ~round(., digits = 2)))


short_heat_events <- identify_heat_stress_events(short)



# diurnal variability
short_var <- short %>% 
  filter(
    TIMESTAMP >= as_datetime("2019-07-01") & 
      TIMESTAMP <= as_datetime("2019-09-15")
  ) %>% 
  mutate(DATE = as_date(TIMESTAMP)) %>% 
  group_by(DEPTH, DATE) %>% 
  summarise(MIN = min(VALUE), MAX = max(VALUE)) %>% 
  mutate(DIFF = MAX - MIN) %>% 
  arrange(desc(DIFF))

# tgc

x1 <- tgc_table %>% 
  filter(STATION == "Madeline Point") %>% 
  group_by(TGC) %>% 
  summarise(
    MIN = min(TGC_INITIAL_WEIGHT),
    MAX = max(TGC_INITIAL_WEIGHT)
  ) %>% 
  mutate(DIFF = MAX - MIN)

x2 <- tgc_table %>% 
  filter(STATION == "Madeline Point", DEPTH != 22) %>% 
  group_by(TGC) %>% 
  summarise(
    MIN = min(TGC_INITIAL_WEIGHT),
    MAX = max(TGC_INITIAL_WEIGHT)
  ) %>% 
  mutate(DIFF = MAX - MIN)


# Beaver Point ------------------------------------------------------------

med <- dat_seasons %>% 
  filter(STATION == "Beaver Point") %>% 
  select(-SEASON)

med_filt <- dat_filt  %>% 
  filter(STATION == "Beaver Point") 

p <- plot_temperature_at_depth(med,
                          date_breaks_major = "1 month",
                          date_labels_format = "%Y-%b")

ggsave(
  p,
  filename = "beaver_all.png",
  path = here("figs/supporting"),
  device = "png",
  width = 25, height = 10, units = "cm",
  dpi = 600
)

p <- plot_filtered_data(med, med_filt,
                        date_breaks_major = "1 month",
                        date_labels_format = "%Y-%b") 

ggsave(
  p,
  filename = "beaver_filt.png",
  path = here("figs/supporting"),
  device = "png",
  width = 25, height = 10, units = "cm",
  dpi = 600
)

p <- med %>% 
  filter(TIMESTAMP >= as_datetime("2018-08-15") &
           TIMESTAMP <= as_datetime("2018-10-15")) %>% 
  plot_temperature_at_depth(plotly_friendly = TRUE)


ggplotly(p)


med %>%
  filter(DEPTH != 15, DEPTH != 10) %>% 
  plot_temperature_at_depth()

med %>% 
  filter(TIMESTAMP >= as_datetime("2018-06-01") &
           TIMESTAMP <= as_datetime("2018-08-15")) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE),
            SD = sd(VALUE),
            MIN = min(VALUE),
            MAX = max(VALUE)) %>% 
  ungroup() %>% 
  mutate(across(2:5, .fns = ~round(., digits = 2)))



med %>% 
  filter(TIMESTAMP >= as_datetime("2018-07-12") &
           TIMESTAMP <= as_datetime("2018-08-15")) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE),
            SD = sd(VALUE),
            MIN = min(VALUE),
            MAX = max(VALUE)) %>% 
  ungroup() %>% 
  mutate(across(2:5, .fns = ~round(., digits = 2)))



med %>% 
  filter(TIMESTAMP >= as_datetime("2018-11-01") &
           TIMESTAMP <= as_datetime("2019-01-19")) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE),
            SD = sd(VALUE),
            MIN = min(VALUE),
            MAX = max(VALUE)) %>% 
  ungroup() %>% 
  mutate(across(2:5, .fns = ~round(., digits = 2)))



tgc_table %>% 
  filter(STATION == "Beaver Point") %>% 
  ggplot( aes(x = factor(TGC), y = TGC_INITIAL_WEIGHT, fill = DEPTH)) +
  geom_point(
    pch = 21, size = 3, alpha = 1,
    #position = position_jitter(width = 0.15, height = 0, seed = 10)
  ) +
  scale_fill_manual("Depth (m)", values = colour_pal, drop = TRUE) +
  scale_x_discrete("TGC Value") +
  scale_y_continuous("Initial Weight (kg)") +
  theme_light() +
  theme(legend.position = "none")

ggsave(
  filename = "beaver_w0.png",
  path = here("figs/supporting"),
  device = "png",
  width = 10, height = 7, units = "cm",
  dpi = 600
)


med_heat_events <- identify_heat_stress_events(med) %>% 
  mutate(DURATION = round(
    as.numeric(difftime(stress_end, stress_start, units = "days")), digits = 2) 
)




# Flat Island ----------------------------------------------------------

long <- dat_seasons %>% 
  filter(STATION == "Flat Island")

plot_temperature_at_depth(long,
                          date_breaks_major = "2 month",
                          date_labels_format = "%Y-%b")

ggsave(
  filename = "flat_all.png",
  path = here("figs/supporting"),
  device = "png",
  width = fig_width, height = fig_height, units = "cm",
  dpi = 600
)



long %>% 
  filter(TIMESTAMP > as_datetime("2019-06-02") & 
           TIMESTAMP < as_datetime("2019-06-08")) %>% 
  plot_temperature_at_depth(date_breaks_major = "1 day",
                            date_labels_format = "%Y-%b-%d") 

ggsave(
  filename = "flat_gap1.png",
  path = here("figs/supporting"),
  device = "png",
  width = fig_width, height = fig_height, units = "cm",
  dpi = 600
)



# Beaver Point ----------------------------------------------------------

med <- dat_seasons %>% 
  filter(STATION == "Beaver Point")

med %>% 
  filter(TIMESTAMP > as_datetime("2018-09-11") &
           TIMESTAMP < as_datetime("2018-09-15")) %>% 
  plot_temperature_at_depth()






