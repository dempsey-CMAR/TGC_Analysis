
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
library(RColorBrewer)
library(sf)            # static map for paper
library(strings)       # convert_depth_to_ordered_factor() function
library(tgc)           # season functions
library(plotly)
library(tidyr)

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

# model results
results <- readRDS(here("results/model_results.rds"))

# dat_raw <- results$dat_raw
dat_seasons <- results$dat_seasons
dat_filt <- results$dat_filt
gap_table <- results$gap_table
dd <- results$dd
tgc_table <- results$tgc_table


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
    MAX = max(TGC_INITIAL_WEIGHT),
    AVG = mean(TGC_INITIAL_WEIGHT)
  ) %>% 
  mutate(DIFF = MAX - MIN)

x2 <- tgc_table %>% 
  filter(STATION == "Madeline Point", DEPTH != 22) %>% 
  group_by(TGC) %>% 
  summarise(
    MIN = min(TGC_INITIAL_WEIGHT),
    MAX = max(TGC_INITIAL_WEIGHT),
    AVG = mean(TGC_INITIAL_WEIGHT)
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
  filter(TIMESTAMP >= as_datetime("2018-10-01") &
           TIMESTAMP <= as_datetime("2019-01-19")) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE),
            SD = sd(VALUE),
            MIN = min(VALUE),
            MAX = max(VALUE)) %>% 
  ungroup() %>% 
  mutate(across(2:5, .fns = ~round(., digits = 2)))



med %>% 
  filter(TIMESTAMP >= as_datetime("2018-09-01") &
           TIMESTAMP <= as_datetime("2018-09-30")) %>% 
  plot_temperature_at_depth(plotly_friendly = TRUE) %>% 
  ggplotly()

med %>% 
  filter(TIMESTAMP >= as_datetime("2018-09-01") &
           TIMESTAMP <= as_datetime("2018-09-30")) %>% 
  summarise(MEAN = mean(VALUE))

mean(med$VALUE)


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

tgc_table %>% 
  filter(STATION == "Beaver Point") %>% 
  group_by(TGC) %>% 
  summarise(
    MIN = min(TGC_INITIAL_WEIGHT),
    MAX = max(TGC_INITIAL_WEIGHT),
    AVG = mean(TGC_INITIAL_WEIGHT)
  ) %>% 
  mutate(DIFF = MAX - MIN)

tgc_table %>% 
  filter(STATION == "Beaver Point", DEPTH == 5 | DEPTH == 10) %>% 
  select(TGC, DEPTH, TGC_INITIAL_WEIGHT) %>% 
  arrange(TGC, DEPTH)


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



long %>% 
  filter(TIMESTAMP > as_datetime("2019-06-01") & 
           TIMESTAMP < as_datetime("2019-09-30")) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE),
            SD = sd(VALUE),
            MIN = min(VALUE),
            MAX = max(VALUE)) %>% 
  ungroup() %>% 
  mutate(across(2:5, .fns = ~round(., digits = 2)))


long %>% 
  filter(TIMESTAMP > as_datetime("2019-11-01") & 
           TIMESTAMP < as_datetime("2020-03-31")) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE),
            SD = sd(VALUE),
            MIN = min(VALUE),
            MAX = max(VALUE)) %>% 
  ungroup() %>% 
  mutate(across(2:5, .fns = ~round(., digits = 2)))



long %>% 
  filter(TIMESTAMP > as_datetime("2020-04-01") & 
           TIMESTAMP < as_datetime("2020-09-30")) %>% 
  group_by(DEPTH) %>% 
  summarise(MEAN = mean(VALUE),
            SD = sd(VALUE),
            MIN = min(VALUE),
            MAX = max(VALUE)) %>% 
  ungroup() %>% 
  mutate(across(2:5, .fns = ~round(., digits = 2)))

tgc_table %>% 
  filter(STATION == "Flat Island") %>% 
  group_by(TGC) %>% 
  summarise(
    MIN = min(TGC_INITIAL_WEIGHT),
    MAX = max(TGC_INITIAL_WEIGHT),
    AVG = mean(TGC_INITIAL_WEIGHT)
  ) %>% 
  mutate(DIFF = MAX - MIN)


# TGC ---------------------------------------------------------------------

dd_input <- seq(500, 5000, 500)

dd <- data.frame(n_degree_days = dd_input)

w0 <- TGC_calculate_initial_weight(dd, final_weight = 5.5, tgc = c(0.25, 0.3, 0.35))

w0 <- w0 %>% 
  mutate(dw = 3 * (1.83 - TGC/1000*n_degree_days)^2)

ggplot(w0, aes(n_degree_days, TGC_INITIAL_WEIGHT, col = factor(TGC))) +
  geom_point() +
  geom_line() +
  theme_light()


ggplot(w0, aes(n_degree_days, dw, col = factor(TGC))) +
  geom_point() +
  geom_line() +
  theme_light()



w0_wide <- w0 %>% 
  pivot_wider(names_from = "TGC", values_from = "TGC_INITIAL_WEIGHT") %>% 
  rename(REMEDIAL = `0.25`,
         AVERAGE = `0.3`,
         ELITE = `0.35`) %>% 
  mutate(
    DIFF_REMEDIAL = REMEDIAL - AVERAGE,
    DIFF_ELITE = AVERAGE - ELITE,
    SIGN = DIFF_REMEDIAL > DIFF_ELITE,
    RANGE = REMEDIAL - ELITE
  )


w0_diff <- w0 %>% 
  arrange(TGC, n_degree_days) %>% 
  group_by(TGC) %>% 
  mutate(DIFF =  TGC_INITIAL_WEIGHT - lead(TGC_INITIAL_WEIGHT))

