# November 7, 2022

# uses the sensorstrings package to extract measured sensor depth from the 
## vemco sensors to estimate the tidal range at each location

# must be connected to CMAR shared drive


# Set Up ------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(sensorstrings)

path <- "Y:/Coastal Monitoring Program/Data_Strings/"
  
# short -------------------------------------------------------------------

path_short <- file.path(paste0(path, "Madeline Point/Madeline Point 2019-04-25"))

dat_short <- ss_compile_deployment_data(path_short)

dat_short %>% 
  filter(
    !is.na(sensor_depth_measured_m),
    sensor_depth_measured_m > 5
  ) %>% 
  group_by(sensor_type) %>% 
  summarise(
    MIN = min(sensor_depth_measured_m),
    MAX = max(sensor_depth_measured_m)
  )

ggplot(dat_short, aes(timestamp_utc, sensor_depth_measured_m)) +
  geom_point() +
  facet_wrap(~sensor_type)

# medium ------------------------------------------------------------------

path_med <- file.path(paste0(path, "Beaver Point/Beaver Point 2018-02-28"))

# deployment 1
dat_med <- ss_compile_vemco_data(
  path_med,
  # information from the deployment log
  sn_table = data.frame(
    sensor_type = "vr2ar", 
    sensor_serial_number = 547111, 
    depth = 15
  ),
  deployment_dates = data.frame(start = "2018-02-28", end = "2018-09-13")  
)

dat_med %>% 
  filter(
    !is.na(sensor_depth_measured_m),
    sensor_depth_measured_m > 9
  ) %>% 
  group_by(sensor_type) %>% 
  summarise(
    MIN = min(sensor_depth_measured_m),
    MAX = max(sensor_depth_measured_m)
  )

ggplot(dat_med, aes(timestamp_utc, sensor_depth_measured_m)) +
  geom_point() +
  facet_wrap(~sensor_type)


# deployment 2
path_med <- file.path(paste0(path, "Beaver Point/Beaver Point 2018-09-13"))

dat_med <- ss_compile_vemco_data(
  path_med,
  # information from deployment log
  sn_table = data.frame(
    sensor_type = "vr2ar", 
    sensor_serial_number = 547111, 
    depth = 15
  ),
  deployment_dates = data.frame(start = "2018-09-13", end = "2019-04-12")  
)

dat_med %>% 
  filter(
    !is.na(sensor_depth_measured_m),
    sensor_depth_measured_m > 5
  ) %>% 
  group_by(sensor_type) %>% 
  summarise(
    MIN = min(sensor_depth_measured_m),
    MAX = max(sensor_depth_measured_m)
  )

ggplot(dat_med, aes(timestamp_utc, sensor_depth_measured_m)) +
  geom_point() +
  facet_wrap(~sensor_type)


# long --------------------------------------------------------------------

path_long <- file.path(paste0(path, "Flat Island/Flat Island 2019-06-06"))

dat_long <- ss_compile_vemco_data(
  path_long,
  # from deployment log
  sn_table = data.frame(
    sensor_type = "vr2ar", 
    sensor_serial_number = 547087, 
    depth = 20
  ),
  deployment_dates = data.frame(start = "2019-06-06", end = "2019-11-17")  
)

dat_long %>% 
  filter(
    !is.na(sensor_depth_measured_m),
    sensor_depth_measured_m > 5
  ) %>% 
  group_by(sensor_type) %>% 
  summarise(
    MIN = min(sensor_depth_measured_m),
    MAX = max(sensor_depth_measured_m)
  )

ggplot(dat_long, aes(timestamp_utc, sensor_depth_measured_m)) +
  geom_point() +
  facet_wrap(~sensor_type)


