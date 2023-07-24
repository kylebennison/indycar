library(statRdaysCFB)
library(data.table)
library(gt)
library(tidyverse)

colors <- function(team){
  reference <- list(
    "mclaren" = "orange",
    "penske" = "black",
    "ganassi" = "red"
  )
  
  return(reference[team] %>% as.character())
}

RACE_PACE <- 1.07
path <- "C:/Users/Kyle/Downloads/Honda Indy Toronto(Streets of Toronto)-Race_(R.I)_2023-07-16/Honda Indy Toronto-Race_R_2023-07-16.csv"


# Functions ---------------------------------------------------------------

pick_fastest <- function(df, drivers=unique(df$driver_name)){
  return(df %>% 
    filter(driver_name %in% drivers) %>% 
    group_by(driver_name) %>% 
    slice_min(lap_time))
}

# Load Data ---------------------------------------------------------------

df <- fread(path)

df <- as_tibble(df)


# Preprocess --------------------------------------------------------------

colnames(df) <- janitor::make_clean_names(colnames(df))

options(digits.secs=4)
df <- df %>% 
  mutate(tod = strptime(tod, format="%H:%M:%OS"))

## Calc TOD at each sector to calc car ahead regardless of lap
sectors <- df %>% 
  group_by(driver_name) %>% 
  mutate(lap_start_tod = lag(tod, n=1L, order_by=lap)) %>% 
  mutate(lap_start_tod = if_else(
    lap == 1,
    tod - lap_time,
    lap_start_tod)) %>%  # first lap start time is calculated by subtracting lap 1 lap time from lap 1 finish TOD
  pivot_longer(cols = matches("s[0-9]+"), names_to = "sector", values_to = "sector_time")

# NA missing sector times
sectors <- sectors %>% 
  group_by(driver_name, lap) %>% 
  mutate(missing_sectors = sum(sector_time == 0)) %>%
  mutate(sector_time = if_else(
    sector_time == 0 & missing_sectors > 0,
    NA, # divide missing time evenly among the sectors
    sector_time
  ))

# NOTE: lap 1 sector times are the sum of the lap number
# So prev lap tod + current lap sector times == current lap tod
# Calc cumsum sector time
sectors <- sectors %>% 
  group_by(driver_name, lap) %>% 
  mutate(cum_sector_time = cumsum(sector_time))

# Calc sector TOD
sectors <- sectors %>% 
  mutate(sector_complete_tod = lap_start_tod + cum_sector_time)

# Group by sector and order by TOD to calc car ahead at that sector.
# Don't calculate car ahead for sector if you didn't pass through that sector.
sectors <- sectors %>%
  group_by(sector) %>%
  mutate(
    car_ahead_s = if_else(
      is.na(sector_complete_tod),
      NA,
      lag(sector_complete_tod, n = 1L, order_by = sector_complete_tod) - sector_complete_tod
    ),
    driver_ahead = if_else(
      is.na(sector_complete_tod),
      NA,
      lag(driver_name, n = 1L, order_by = sector_complete_tod)
    )
  )

# This should be running order instead of position
sectors %>% 
  arrange(sector, sector_complete_tod)

# TODO: Create mini-laps and get running order for all non-NA mini-laps, then calc overtakes

# Pivot back
df_cum <- sectors %>% 
  pivot_wider(id_cols = c(driver_name, lap),
              names_from = sector,
              values_from = cum_sector_time,
              names_glue = "{sector}_cum")


# NOTE: Last sector is the s/f line, so cum sum time equals full lap time
# Join back
df <- df %>% 
  left_join(df_cum, by = c("driver_name", "lap"))

# calc position at end of lap
df <- df %>% 
  group_by(lap) %>% 
  mutate(pos = rank(tod))

# Cum lap time
df <- df %>% 
  group_by(driver_name) %>% 
  mutate(cum_lap_time = cumsum(lap_time))

# Gap to leader, gap to next
df <- df %>% 
  group_by(lap) %>% 
  mutate(leader_tod = min(tod),
         next_tod = lag(tod, n = 1L, order_by = tod)
  ) %>% 
  mutate(gap_to_leader = leader_tod - tod,
         gap_to_next = next_tod - tod,
         ) %>% 
  ungroup() %>% 
  mutate(
    car_ahead_tod = lag(tod, 1L, order_by = tod),
    car_behind_tod = lead(tod, 1L, order_by = tod),
    gap_to_car_ahead = car_ahead_tod - tod,
    gap_to_car_behind = car_behind_tod - tod
    )

# Track Status Update
df <- df %>%
  group_by(driver_name) %>% 
  mutate(status_desc = case_when(
    status == "P" ~ "Pit In",
    lag(status, 1L, order_by = lap) == "P" ~ "Pit Out",
    status == "Y" ~ "Yellow",
    status == "" & lag(status, 1L, order_by = lap) == "Y" ~ "Yellow Ending",
    status == "" & lag(status, 2L, order_by = lap) == "Y" ~ "Restart",
    TRUE ~ status
  )) %>% 
  mutate(is_yellow = if_else(str_detect(status_desc, "Yellow|Y"), 1, 0))

# Tire Stint
df <- df %>% 
  group_by(driver_name) %>% 
  mutate(n_stops = cumsum(status == "P"),
         tire_stint = cumsum(status_desc == "Pit Out") + 1)

# tire life
df <- df %>% 
  group_by(driver_name, tire_stint) %>% 
  mutate(tire_life = row_number())

# Define traffic
# Traffic is being close to the car ahead (usually multiple cars)
# Stuck in traffic is following close and not improving position over multiple corners
# You are going slower than you'd like, evidenced by the fact that when you finally do pass
# you pull away from the car behind
# TODO: Calc using sectors, not laps, then apply to full lap.
df <- df %>% 
  group_by(driver_name) %>% 
  mutate(
    in_traffic = if_else(
      gap_to_car_ahead > -.75  # close to car ahead
      & lag(pos, 1L, order_by = lap) == pos,  # Not improving position
      1,
      0
    ),
    # NOTE: This does not account for being held up by lap traffic, only people ahead
    # in position
    was_held_up = if_else(
      lag(in_traffic, 1L, order_by = lap) == 1  # in traffic prev. lap
      & lag(lap_time, 1L, order_by = lap) > lap_time  # lap time improved
      & lag(pos, 1L, order_by = lap) > pos,  # advanced position (passed traffic)
      1,
      0
    ))

# Number of passes made
# TODO: Calc at sector level.
df <- df %>% 
  group_by(driver_name) %>% 
  mutate(pos_gained = lag(pos, n = 1L, order_by = lap) - pos,
         overtakes = if_else(pos_gained > 0, pos_gained, 0))

field_green_lap_time <- df %>% 
  filter(is_yellow == 0
         & ! status_desc %in% c("Pit In", "Pit Out")
         & lap_time <= (1.5 * min(lap_time))) %>% 
  pull(lap_time) %>% mean()

# TODO: How to identify overtakes of backmarkers?
# instead of grouping by lap, group by leader time and calc the order of the field
# relative to the leader until the leader passes s/f again.


# Analysis ----------------------------------------------------------------

# create a folder to hold any plots made today
lubridate::today()
PLOT_SAVE_PATH <- paste0(getwd(), "/plots/", lubridate::today())
dir.create(PLOT_SAVE_PATH)

# Green Flag Lap Times by number of stops
df %>% 
  filter(is_yellow == 0
         & ! status_desc %in% c("Pit In", "Pit Out")
         & lap_time <= (1.5 * min(lap_time))) %>% 
  group_by(driver_name) %>% 
  mutate(total_stops = max(n_stops)) %>% 
  filter(total_stops != 0) %>% 
  group_by(total_stops) %>% 
  summarise(avg_lap_time = mean(lap_time), std_dev_lap_time = sd(lap_time)) %>% 
  gt() %>% 
  fmt_auto() %>% 
  tab_header(title = "Green Flag Lap Times", subtitle = "By Strategy") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  data_color(columns = c(avg_lap_time, std_dev_lap_time), palette = "viridis") %>% 
  gt::gtsave(filename="green_flag_laptimes.png", path = PLOT_SAVE_PATH)

# For leaders
top_3 <- df %>% 
  filter(lap == max(lap) & pos <= 3) %>% pull(driver_name)

df %>% 
  filter(is_yellow == 0
         & ! status_desc %in% c("Pit In", "Pit Out")
         & lap_time <= (1.5 * min(lap_time))
         & driver_name %in% top_3) %>%
  group_by(driver_name) %>% 
  summarise(avg_lap_time = mean(lap_time), std_dev_lap_time = sd(lap_time)) %>% 
  gt() %>% 
  fmt_auto() %>% 
  tab_header(title = "Green Flag Lap Times", subtitle = "Leaders") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  gt::gtsave(filename="green_flag_laptimes_leaders.png", path = PLOT_SAVE_PATH)

df %>% 
  filter(is_yellow == 0
         & ! status_desc %in% c("Pit In", "Pit Out")
         & lap_time <= (1.5 * min(lap_time))
         & driver_name %in% top_3) %>%
  ggplot(aes(x=lap, y=lap_time, color=driver_name)) +
  geom_line(size=2,alpha=.5) +
  staturdays_theme +
  theme(panel.grid.major = element_line(color = "grey90"),
        plot.caption.position = "plot") +
  labs(color = "Driver", x = "Lap", y = "Time (s)", caption = "Green non-pit laps only")
  
ggsave(filename = "green_laptime_plot_leaders.png", path = PLOT_SAVE_PATH,
       width = 1600, height = 900, units = "px", dpi = 300)
  
# O'Ward's Pit Delta
# TODO: Generalize (make a function)
oward_green_lap_time <- df %>% 
  filter(is_yellow == 0
         & ! status_desc %in% c("Pit In", "Pit Out")
         & lap_time <= (1.5 * min(lap_time))
         & driver_name %in% c("O'Ward")) %>%
  pull(lap_time) %>% mean()

df %>% 
  group_by(driver_name) %>% 
  filter(status == "P" | (lag(status, 1L, order_by = lap) == "P"),
         driver_name == "O'Ward") %>% 
  mutate(pit_delta = lap_time - oward_green_lap_time) %>% 
  mutate(pit_stop = (row_number() + 1) %/%2) %>% 
  group_by(pit_stop) %>% 
  summarise(total_pit_delta = sum(pit_delta)) %>% 
  gt() %>% 
  tab_header(title = "O'Ward Pit Stop Delta", subtitle = "Time Lost Over Avg. Green Flag Lap (s)") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  fmt_auto() %>% 
  gtsave(filename="oward_pit_delta.png", path = PLOT_SAVE_PATH)
  

# n overtakes
# TODO: Do at sector level too
df %>% 
  summarise(total_overtakes = sum(overtakes, na.rm=TRUE)) %>% 
  arrange(desc(total_overtakes)) %>% 
  head(10) %>% 
  gt() %>% 
  tab_header(title = "Overtakes for Position", subtitle = "Top 10 Overtakers") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  fmt_auto() %>% 
  gtsave(filename="most_overtakes.png", path = PLOT_SAVE_PATH)

df %>% 
  summarise(total_overtakes = sum(overtakes, na.rm=TRUE)) %>% 
  arrange(desc(total_overtakes)) %>% 
  tail(10) %>% 
  gt() %>% 
  tab_header(title = "Overtakes for Position", subtitle = "Bottom 10 Overtakers") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  fmt_auto() %>% 
  gtsave(filename="least_overtakes.png", path = PLOT_SAVE_PATH)

# Tire age
df %>% 
  filter(is_yellow == 0, lap_time <= (1.5 * min(lap_time)),
         !status_desc %in% c("Pit In", "Pit Out")) %>% 
  group_by(tire, tire_life) %>% 
  summarise(lap_time = mean(lap_time)) %>% 
  ggplot(aes(x=tire_life, y=lap_time, color=tire)) +
  geom_line(size=2, alpha=.7) +
  staturdays_theme +
  theme(panel.grid.major = element_line(color = "grey90"),
        plot.caption.position = "plot") +
  labs(color = "Tire", x = "Tire Life", y = "Time (s)", caption = "Green non-pit laps only")

ggsave(filename = "tire_dropoff.png", path = PLOT_SAVE_PATH,
       width = 1600, height = 900, units = "px", dpi = 300)

# TODO: Generalize
df %>% filter(
  driver_name == "O'Ward"
) %>% 
  group_by(tire_stint, tire) %>% 
  summarise(stint_length = max(tire_life), stint_pace = mean(lap_time), stint_std_dev = sd(lap_time)) %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(title = "O'Ward's Tire Stints") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  fmt_auto() %>% 
  gtsave(filename="oward_tire_stints.png", path = PLOT_SAVE_PATH)

# Fastest laps overall
df %>% 
  ungroup() %>% 
  slice_min(lap_time, n = 10) %>% 
  select(driver_name, lap, lap_time) %>% 
  gt() %>% 
  tab_header(title = "Fastest Laps Overall") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  fmt_auto() %>% 
  data_color(columns = c(lap_time), palette = "viridis", reverse = TRUE) %>% 
  gtsave(filename="fastest_laps.png", path = PLOT_SAVE_PATH)

# By driver
df %>% 
  group_by(driver_name) %>% 
  slice_min(lap_time) %>% 
  select(driver_name, lap, lap_time) %>% 
  arrange(lap_time) %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(title = "Fastest Lap by Driver") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  fmt_auto() %>% 
  data_color(columns = c(lap_time), palette = "viridis", reverse = TRUE) %>% 
  gtsave(filename="fastest_laps_driver.png", path = PLOT_SAVE_PATH)

df %>% 
  filter(lap_time < RACE_PACE * min(lap_time)) %>% 
  ggplot(aes(x = lap_time, y = driver_name)) +
  geom_boxplot()
  
## Sector Analysis
# Who was chasing who at the end of the race?
sectors %>% group_by(driver_name, driver_ahead) %>% 
  filter(lap >= 60, car_ahead_s > -1) %>% 
  summarise(n = n()) %>% 
  group_by(driver_name) %>% 
  slice_max(n) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  gt() %>% 
  fmt_auto() %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  data_color(columns = where(~is.numeric(.x)), palette = "viridis") %>% 
  tab_header(title = "Following Closely", subtitle = "Sectors within 1 second of car ahead") %>% 
  tab_footnote("Lap 60+")

# TODO: Number of overtakes and being overtaken, and net overtakes
  

# Sector heatmap
quickest_lap <- df %>% 
  pick_fastest() %>% 
  ungroup() %>% 
  filter(lap_time < RACE_PACE * min(lap_time)) %>% 
  select(driver_name, matches("s[0-9]+")) %>% 
  select(!matches("cum")) %>% 
  pivot_longer(cols = !driver_name, names_to = "sector", values_to = "time") %>% 
  group_by(sector) %>% 
  mutate(pct = round((time - min(time))/min(time), 4),
         time_lost = round(time - min(time), 4)) %>%
  group_by(driver_name) %>% 
  mutate(row_num = row_number())

# Pct. based sectors
quickest_lap %>% 
  ungroup() %>% 
  mutate(text_color = if_else(pct < .2 * max(pct) | pct > .8 * max(pct), "white", "black"),
         sector = str_sub(sector, 2)) %>% 
  ggplot(aes(x = reorder(sector, row_num), y = driver_name, fill = pct)) +
  geom_tile(alpha=1, width = .9, height=.95) +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint = max(quickest_lap$pct)/2,
                       labels=scales::percent) +
  geom_text(aes(label = scales::number(time, accuracy = .0001), color = if_else(text_color=="white", "white", "black"))) +
  scale_color_identity() +
  theme(panel.background = element_blank()) +
  labs(title="Fastest Lap Sector Times",
       subtitle="Colored by Percent Above Quickest in Sector",
       fill="",
       caption="@staturdays",
       x = "Sector") +
  staturdays_theme +
  theme(axis.title.y = element_blank())

# Unvetted ----------------------------------------------------------------

# NOTE: min(tod) is the time of the completion of the first lap, so short 100 seconds 
# the full race time. For full race time, sum lap times.
df %>% 
  group_by(driver_name) %>% 
  summarise(total_race_time = max(tod) - min(tod),
            total_lap_time = max(cum_lap_time)) %>% 
  select(driver_name, total_race_time, total_lap_time) %>% 
  arrange(total_race_time)

# TODO: Gap on the first lap of a status!=Y lap from first to last

df %>% 
  group_by(driver_name) %>% 
  mutate(total_stops = sum(status == "P")) %>% 
  filter(status == "P" | (lag(status, 1L, order_by = lap) == "P"),
         total_stops == 2) %>% 
  mutate(pit_delta = lap_time - field_avg_green_lap_time) %>% 
  mutate(pit_stop = (row_number() + 1) %/%2) %>% 
  group_by(driver_name, pit_stop) %>% 
  summarise(total_pit_delta = sum(pit_delta)) %>% 
  group_by(pit_stop) %>% 
  summarise(avg_pit_delta = mean(total_pit_delta)) %>% 
  gt() %>% 
  tab_header(title = "Field Pit Stop Delta", subtitle = "Time Lost Over Avg. Green Flag Lap (s)") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  fmt_auto()

df %>% group_by(driver_name) %>% 
  summarise(in_traffic = sum(in_traffic, na.rm = TRUE),
            was_held_up = sum(was_held_up, na.rm = TRUE),
            n_stops = max(tire_stint)) %>% View()

# NOTE: Last lap of yellow flag is considered "green", so mark it as "YE" yellow ending
# NOTE: Filter for driver's who completed all laps to avoid one driver with a very long
# lap time throwing off the average

## Tire difference
# Tire lap times


# What were the other driver's pit deltas on the 2-stopper?


# TODO: Where did most people stop on the 2-stopper vs. the 3-stopper?

# What was the tire dropoff for each compound throughout the race?

# What was the time gained by O'Ward with fresh tires relative to his peers?

# Should Pato have 2-stopped (what was the pit time lost vs. the dropoff from a longer tire stint?)

# Best re-starters? Who passes the most people on first lap after green?

# Fastest total laps times (ignores starting position 
# to find the cars that got around the track the fastest)
df %>% 
  group_by(driver_name) %>% 
  mutate(total_lap_times = cumsum(lap_time),
         laps_complete = max(lap)) %>% 
  filter(total_lap_times == max(total_lap_times)) %>% 
  select(driver_name, total_lap_times, laps_complete) %>% 
  ungroup() %>% 
  filter(laps_complete == max(laps_complete))
  arrange(total_lap_times)

# Lap Times
df %>% filter(
  driver_name %in% c("Power", "Palou")
) %>% 
  ggplot(aes(x = lap, y = lap_time, color = driver_name)) +
  geom_line(size=2, alpha=.5) +
  geom_line(data=gap, aes(y=gap_to_palou))

# Lap Times Gap
gap <- df %>% filter(
  driver_name %in% c("Power", "Palou")
) %>% 
  arrange(lap, driver_name) %>% 
  group_by(lap) %>% 
  mutate(palou = lag(lap_time, n=1L)) %>% 
  mutate(gap_to_palou = palou - lap_time) %>%
  drop_na(gap_to_palou)
  
gap %>% 
  ggplot(aes(x=lap, y=gap_to_palou)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype="dashed", color="red")

# Lap by lap pace, palou vs. power
total_gap <- df %>% 
  group_by(driver_name) %>% 
  arrange(lap) %>% 
  mutate(total_time = cumsum(lap_time)) %>% 
  filter(
  driver_name %in% c("Power", "Palou")
) %>% 
  arrange(lap, driver_name) %>% 
  group_by(lap) %>% 
  mutate(palou = lag(total_time, n=1L)) %>% 
  mutate(gap_to_palou = palou - total_time) %>%
  drop_na(gap_to_palou)

total_gap %>% 
  ggplot(aes(x=lap, y=gap_to_palou)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(caption = "Lap by Lap Pace. Positive = Power Quicker")

# Time of Day Gap
total_gap <- df %>% 
  group_by(driver_name) %>% 
  arrange(lap) %>% 
  filter(
    driver_name %in% c("Power", "Palou")
  ) %>% 
  arrange(lap, driver_name) %>% 
  group_by(lap) %>% 
  mutate(palou = lag(tod, n=1L)) %>% 
  mutate(gap_to_palou = palou - tod) %>%
  drop_na(gap_to_palou)

total_gap %>% 
  ggplot(aes(x=lap, y=gap_to_palou)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(caption = "Lap by Lap Pace. Positive = Power Quicker")

df %>% group_by(driver_name) %>% 
  arrange(lap) %>% 
  mutate(total_time = cumsum(lap_time)) %>% 
  filter(driver_name %in% c("Palou", "Power")) %>% 
  ggplot(aes(x=lap, y=total_time, color=driver_name)) +
  geom_line()

df %>% 
  filter(driver_name %in% c("Palou", "Power", "Kirkwood", "Castroneves", "Rahal")) %>% 
  ggplot(aes(x=lap, y=lubridate::as.difftime(tod), color=driver_name)) +
  geom_line()
