library(indycar)
library(ggplot2)
library(dplyr)

df <- load_data("https://racetools.com/logfiles/IndyCar/2023/Gallagher%20Grand%20Prix(Indianapolis%20Motor%20Speedway%20RC)-Race_(R.I)_2023-08-12.zip")
df <- preprocess_laps(df)
df %>% plot_laps_boxplot()
df %>% plot_sector_quickest_heatmap()
df %>% plot_lap_times(drivers = c("Rahal", "Dixon"))
df %>% table_pit_delta()
df %>% plot_tire_life()

df <- df %>% add_manufacturer()

df %>% group_by(driver_name, manufacturer, tire_stint) %>%
  summarise(stint_length = max(tire_life),
            stint_pace = sum(lap_time)) %>%
  group_by(
    manufacturer
  ) %>%
  summarise(avg_stint_length = mean(stint_length),
            avg_stint_pace = mean(stint_pace))

df %>%
  filter(stringr::str_detect(status_desc, "Pit")) %>%
  group_by(driver_name, n_stops) %>%
  mutate(pos_lost = pos - lag(pos)) %>%
  select(driver_name, lap, n_stops, pos_lost) %>%
  filter(!is.na(pos_lost)) %>%
  rename(stop_num = n_stops)
