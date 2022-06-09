# June 3, 2022

library(cowplot)       # to inset map figure
library(dplyr)         # data manipulation 
library(ggplot2)       # figures
library(ggpubr)        # to arrange figures
library(ggspatial)     # for north arrow and scale bar
library(glue)          # used in map figure
library(here)          # relative file paths
library(lubridate)     # dates
library(patchwork)
library(RColorBrewer)  # for TGC model figure
library(rnaturalearth) # for map of North America
library(rnaturalearthhires) # for map of NS
library(readr)         # export table
library(sf)            # static map 
library(strings)       # convert_depth_to_ordered_factor() function
library(tgc)           # season functions
library(tidyr)         # to format the sensitivity results
library(viridis)       # colour palette

# max size
#width = 17, height = 22.5, units = "cm"


# Import results ----------------------------------------------------------
results <- readRDS(here("results/model_results.rds"))

dat_raw <- results$dat_raw
dat_seasons <- results$dat_seasons
dat_filt <- results$dat_filt
gap_table <- results$gap_table
dd <- results$dd
tgc_table <- results$tgc_table

sensitiviy <- readRDS(here("results/sensitivity_results.rds"))
tgc_comp_16 <- sensitiviy$tgc_comp_16
tgc_comp_20 <- sensitiviy$tgc_comp_20


# Plot params -------------------------------------------------------------
theme_set(theme_light())
colour_pal <- viridis(5, direction = -1)

# Figure 1 ----------------------------------------------------------------

# not made in R



# Figure 2 ----------------------------------------------------------------
dd_input <- seq(500, 5000, 1)

dd_example <- data.frame(n_degree_days = dd_input)

w0 <- TGC_calculate_initial_weight(
  dd_example, final_weight = 5.5, tgc = c(0.25, 0.30, 0.35)
) %>% 
  mutate(
    TGC = as.character(TGC),
    TGC = if_else(TGC == "0.3", paste0(TGC, "0"), TGC)
  )

# w0 <- w0 %>% 
#   mutate(dw = 3 * (1.83 - TGC/1000*n_degree_days)^2)

ggplot(w0, aes(n_degree_days, TGC_INITIAL_WEIGHT, col = TGC)) +
  geom_line(size = 1.5) +
  scale_x_continuous("Number of Degree Days") +
  scale_y_continuous("Initial Weight (kg)") +
  scale_colour_manual("TGC Value", values = brewer.pal(3, "Dark2")) +
  guides(colour = guide_legend(keyheight = 0.75)) +
  theme(
    text = element_text(size = 8), 
    legend.position = c(0.87, 0.77),
    legend.background = element_rect(fill = "white", colour = "darkgrey", size = 0.5),
    legend.title = element_text(size = 7)
  ) 

ggsave(
  filename = "figure2.png",
  path = here("paper/figs"),
  device = "png",
  width = 10, height = 7, units = "cm",
  dpi = 600
)

# Figure 3 ----------------------------------------------------------------

# north america - small scale for inset
na <- ne_countries(
  continent = "north america", returnclass = "sf", scale = "small"
)

# north america - large scale for main map
can <- ne_countries(
  continent = "north america", returnclass = "sf", scale = "large"
)

# extract station coordinates and make labels
dat_map <- dat_raw %>%
  distinct(STATION, .keep_all = TRUE) %>%
  create_station_label(
    short = "Madeline Point", 
    medium = "Beaver Point", 
    long = "Flat Island"
  ) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(can)) 

# main map extents
x_min <- -66.5
x_max <- -59.8
y_min <- 43.5
y_max <- 47

# box for inset map
ns_box <- data.frame(
  lat = c(y_min, y_min, y_max, y_max),
  long = c(x_min, x_max, x_max, x_min)
) %>% 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(na)) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

# main map
p1 <- ggplot() +
  geom_sf(data = can, size = 0.5) +
  geom_sf(
    data = dat_map, 
    pch = 21, col = "lightblue", fill = "blue", size = 3
  ) +
  geom_sf_label(
    data = dat_map, aes(label = LABEL),
    nudge_y = -0.27, size = 2.7, alpha = 0.6, label.padding = unit(0.15, "lines")
  ) +
  scale_x_continuous(limits =  c(x_min, x_max), breaks = c(seq(-60, -66, -2))) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = c(seq(43, 47, 1))) +
  annotation_scale(location = "br") +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(0.75, "cm"),
    width = unit(0.75, "cm")
  ) +
  theme(
    axis.title = element_blank(),
    text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank()
  ) 

# p1

# inset 
p2 <- ggplot() +
  geom_sf(data = na, size = 0.25) +
  geom_sf(data = ns_box, col = "red", fill = NA, size = 0.25) +
  scale_x_continuous(limits = c(-130, -55)) +
  scale_y_continuous(limits = c(30, 70)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid = element_blank(), 
    plot.background = element_rect(fill = NA, color = NA)
  )

# p2

p3 <- cowplot::ggdraw() +
  draw_plot(p1) +
  draw_plot(p2, x = 0.08, y = 0.65, width = 0.3, height = 0.3)

# p3

ggsave(
  p3,
  filename = "figure3.png",
  path = here("paper/figs"),
  device = "png",
  width = 12, height = 9, units = "cm",
  dpi = 600
)


# Figure 4 ----------------------------------------------------------------
ylims <- c(-1, 21)

mad <- dat_seasons %>% 
  filter(STATION == "Madeline Point") 

mad_filt <- mad %>% 
  filter_out_heat_stress_events()

p4_A <- mad %>% 
  select(-SEASON) %>% 
  plot_filtered_data(
    mad_filt,
    colour_palette = colour_pal,
    ylims = ylims,
    date_axis_name = NULL,
    date_breaks_major = "1 month",
    date_labels_format = "%Y-%b",
  ) +
  labs(title = "Madeline Point (Short Season)")

p4_A

# Beaver Point
beaver <- filter(dat_seasons, STATION == "Beaver Point") 

beaver_filt <- filter_out_heat_stress_events(beaver)

p4_B <- beaver %>% 
  select(-SEASON) %>% 
  plot_filtered_data(
    beaver_filt,
    legend_position = "right",
    colour_palette = colour_pal,
    ylims = ylims,
    date_axis_name = NULL,
    date_breaks_major = "1 month",
    date_labels_format = "%Y-%b"
  ) +
  labs(title = "Beaver Point (Medium Season)")

p4_B

# Flat Island
flat <- filter(dat_seasons, STATION == "Flat Island") 

flat_filt <- filter_out_heat_stress_events(flat)

p4_C <- flat %>% 
  select(-SEASON) %>% 
  plot_filtered_data(
    flat_filt,
    colour_palette = colour_pal,
    ylims = ylims,
    date_breaks_major = "2 month",
    date_labels_format = "%Y-%b"
  ) +
  labs(title = "Flat Island (Long Season)")

p4_C

p4 <- ggarrange(
  p4_A, p4_B, p4_C, 
  ncol = 1, 
  labels = "AUTO",
  font.label = list(face = "bold"),
#  label.x = 0.92, label.y = 0.95,
  common.legend = TRUE, legend = "bottom"
)

p4

ggsave(
  filename = "figure4.png",
  path = here("paper/figs"),
  device = "png",
  width = 25, height = 27, units = "cm",
  dpi = 600
)


# Figure 5 ----------------------------------------------------------------

dd %>% 
  mutate(
    PERCENT_HEAT_STRESS = (n_filtered_days / STOCKED_DAYS) * 100
  ) %>% 
  create_station_label(
    short = "Madeline Point", 
    medium = "Beaver Point", 
    long = "Flat Island"
  ) %>%
  ggplot(aes(LABEL, PERCENT_HEAT_STRESS, fill = DEPTH)) +
  geom_point(pch = 21, size = 3, alpha = 0.75) +
  scale_fill_manual("Depth (m)", values = colour_pal) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "% heat stress days") 

ggsave(
  filename = "figure5.png",
  path = here("paper/figs"),
  device = "png",
  width = 17, height = 6, units = "cm",
  dpi = 600
)

# Figure 6 ----------------------------------------------------------------
point_size <- 4

p6_A <- dd %>% 
  create_station_label(
    short = "Madeline Point", 
    medium = "Beaver Point", 
    long = "Flat Island"
  ) %>%
  ggplot(aes(LABEL, n_degree_days, fill = DEPTH)) +
  geom_point(pch = 21, size = point_size, alpha = 0.75) +
  scale_fill_manual("Depth (m)", values = colour_pal) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Number of Degree Days") 

p6_A


p6_B  <- tgc_table %>% 
  create_station_label(
    short = "Madeline Point", 
    medium = "Beaver Point", 
    long = "Flat Island"
  ) %>%
ggplot(
   aes(x = factor(TGC), y = TGC_INITIAL_WEIGHT, fill = DEPTH)
) +
  geom_point(pch = 21, size = point_size, alpha = 0.75) +
  scale_fill_manual("Depth (m)", values = colour_pal, drop = FALSE) +
  scale_x_discrete("TGC Value") +
  scale_y_continuous("Initial Weight (kg)") +
  facet_wrap(~LABEL) +
  theme(
    strip.background = element_rect(fill = "white", colour = "darkgrey"),
    strip.text = element_text(colour = "grey30", size = 10)
  )


p6_B

p6_A / p6_B +
  plot_annotation(tag_levels = 'A') +
  plot_layout(heights = c(1, 1.25),
                guides = 'collect') &
  theme(plot.tag = element_text(face = 'bold')) 


ggsave(
  filename = "figure6.png",
  path = here("paper/figs"),
  device = "png",
  width = 25, height = 15, units = "cm",
  dpi = 600
)


# Figure 7 ----------------------------------------------------------------

dat_sens <- tgc_comp_16 %>% 
  select(STATION, DEPTH, TGC, PERCENT_DIFF_16 = PERCENT_DIFF_weight) %>% 
  left_join(
    select(tgc_comp_20, STATION, DEPTH, TGC, PERCENT_DIFF_20 = PERCENT_DIFF_weight), 
    by = c("STATION", "DEPTH", "TGC")) %>% 
  filter(TGC == 0.3) %>% 
  pivot_longer(
    cols = c("PERCENT_DIFF_16", "PERCENT_DIFF_20"), names_to = "MODEL",
    values_to = "PERCENT_DIFF"
  ) %>% 
  mutate(
    PERCENT_DIFF = -1 * PERCENT_DIFF, # so that the strict model bars are on top
    MODEL = if_else(MODEL == "PERCENT_DIFF_16", "16 degrees C", "20 degrees C")
  ) %>% 
  create_station_label(
    short = "Madeline Point", 
    medium = "Beaver Point", 
    long = "Flat Island",
    sep = " "
  )

ggplot(
  dat_sens, 
  aes(DEPTH, PERCENT_DIFF, fill = DEPTH, linetype = MODEL, group = DEPTH)
) +
  geom_col(position = "dodge", col = "black" , width = 0.5) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(name = "Depth (m)") +
  scale_y_continuous("Percent Difference (%)") + 
  scale_fill_manual(name = "Depth (m)", values = colour_pal, guide = "none") + 
  scale_linetype_manual(name = "Heat Stress\nThreshold", values = c(1, 2)) + 
  facet_wrap(~LABEL, scales = "free_y", ncol = 1) +
  theme(
    strip.background = element_rect(fill = "white", colour = "darkgrey"),
    strip.text = element_text(colour = "grey30", size = 10),
  ) +
  guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA))))

ggsave(
  filename = "figure7_v2.png",
  path = here("paper/figs"),
  device = "png",
  dpi = 600,
  width = 17,
  height = 12,
  units = "cm"
)





# Table 2 -----------------------------------------------------------------

table2 <- dd %>%
  mutate(
    START_SEASON = format(as_date(START_SEASON)),
    END_SEASON = format(as_date(END_SEASON)),
    AVG_TEMPERATURE = round(AVG_TEMPERATURE, digits = 2),
    n_degree_days = round(n_degree_days)
  ) %>% 
  rename(
    Station = STATION,
    `Depth (m)` = DEPTH,
    `Stocking Date` = START_SEASON,
    `Harvest Date` = END_SEASON,
    `Number of Stocked Days` = STOCKED_DAYS,
    `Number of Filtered Days` = n_filtered_days,
    `Number of Growing Days` = n_growing_days,
    `Average Temperature` = AVG_TEMPERATURE,
    `Number of Degree Days` = n_degree_days
  )
  
write_csv(table2, file = here("paper/tables/Table2.csv"))
  
  
  
  

