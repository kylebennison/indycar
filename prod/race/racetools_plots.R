library(statRdaysCFB)
library(data.table)
library(tidyverse)
library(gt)

# Set WD and create a folder to hold any plots made today
setwd("C:/Users/Kyle/Documents/Projects/Data Projects/Indycar")
lubridate::today()
PLOT_SAVE_PATH <- paste0(getwd(), "/plots/", lubridate::today())
dir.create(PLOT_SAVE_PATH)

colors <- function(team){
  reference <- list(
    "mclaren" = "orange",
    "penske" = "black",
    "ganassi" = "red"
  )
  
  return(reference[team] %>% as.character())
}


# Load Data ---------------------------------------------------------------

path <- "C:/Users/Kyle/Downloads/Honda Indy 200 at Mid-Ohio(Mid-Ohio Sports Car Course)-Race_(R.I)_2023-07-02/Honda Indy 200 at Mid-Ohio-Race_R_2023-07-02.csv"

df <- fread(path)


# Preprocess --------------------------------------------------------------

colnames(df) <- janitor::make_clean_names(colnames(df))

options(digits.secs=4)
df <- df %>% 
  mutate(tod = strptime(tod, format="%H:%M:%OS"))

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

# Green Flag Lap Times by number of stops
df %>% 
  filter(is_yellow == 0
         & ! status_desc %in% c("Pit In", "Pit Out")
         & lap_time <= (1.5 * min(lap_time))) %>% 
  group_by(driver_name) %>% 
  mutate(total_stops = max(n_stops)) %>% 
  group_by(total_stops) %>% 
  summarise(avg_lap_time = mean(lap_time), std_dev_lap_time = sd(lap_time)) %>% 
  gt() %>% 
  fmt_auto() %>% 
  tab_header(title = "Green Flag Lap Times", subtitle = "By Strategy") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  gt::gtsave(filename="green_flag_laptimes.png", path = PLOT_SAVE_PATH)

# For leaders
df %>% 
  filter(is_yellow == 0
         & ! status_desc %in% c("Pit In", "Pit Out")
         & lap_time <= (1.5 * min(lap_time))
         & driver_name %in% c("O'Ward", "Dixon", "Palou")) %>%
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
         & driver_name %in% c("O'Ward", "Dixon", "Palou")) %>%
  ggplot(aes(x=lap, y=lap_time, color=driver_name)) +
  geom_line(size=2,alpha=.5) +
  staturdays_theme +
  theme(panel.grid.major = element_line(color = "grey90"),
        plot.caption.position = "plot") +
  labs(color = "Driver", x = "Lap", y = "Time (s)", caption = "Green non-pit laps only")
  
ggsave(filename = "green_laptime_plot_leaders.png", path = PLOT_SAVE_PATH,
       width = 1600, height = 900, units = "px", dpi = 300)
  
# O'Ward's Pit Delta
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
df %>% 
  summarise(total_overtakes = sum(overtakes, na.rm=TRUE)) %>% 
  arrange(desc(total_overtakes)) %>% 
  head(10) %>% 
  gt() %>% 
  tab_header(title = "Overtakes for Position", subtitle = "Top 10 Overtakers") %>% 
  cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>% 
  fmt_auto() %>% 
  gtsave(filename="most_overtakes.png", path = PLOT_SAVE_PATH)

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
