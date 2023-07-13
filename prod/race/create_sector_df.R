sectors <- df %>% 
  group_by(driver_name) %>% 
  mutate(lap_start_tod = lag(tod, n=1L, order_by=lap)) %>% 
  mutate(lap_start_tod = if_else(
    lap == 1,
    tod - lap_time,
    lap_start_tod)) %>%  # first lap start time is calculated by subtracting lap 1 lap time from lap 1 finish TOD
  select(driver_name, lap_time, tod, lap_start_tod, lap, status, status_desc, matches("s[0-9]+")) %>% 
  pivot_longer(cols = matches("s[0-9]+"), names_to = "sector", values_to = "sector_time")

# NOTE: lap 1 sector times are the sum of the lap number
# So prev lap tod + current lap sector times == current lap tod

sectors <- sectors %>% 
  group_by(driver_name, lap) %>% 
  mutate(cum_sector_time = cumsum(sector_time))

# NOTE: Last 2 sectors and first 2 sectors of next lap register as "0.0000" if 
# driver pits. To rectify, subtract cumsum sector time up until that point
# from total lap time and apply the difference to each sector.
# TODO: Try making these sectors NA instead, but then I can't calc cum_sum for that lap.
sectors <- sectors %>% 
  group_by(driver_name, lap) %>% 
  mutate(missing_sectors = sum(sector_time == 0)) %>%
  mutate(sector_time = if_else(
    sector_time == 0 & missing_sectors > 0,
    (lap_time - max(cum_sector_time)) / missing_sectors, # divide missing time evenly among the sectors
    sector_time
  ))

# Now I need to recalc cum_sector_time to include the missing sectors
sectors <- sectors %>% 
  group_by(driver_name, lap) %>% 
  mutate(cum_sector_time = cumsum(sector_time)) %>% 
  group_by(driver_name) %>% 
  mutate(mini_lap = row_number(),
         sector_tod = lap_start_tod + cum_sector_time)

# Calc position for each sector
sectors %>% 
  group_by(mini_lap) %>% 
  mutate(pos = rank(sector_tod)) %>% View()

# Sandbox -------------------------------------------------------------------------


herta_lap_1_sector_sum <- sectors %>% filter(driver_name == "Herta") %>% 
  filter(lap == 2) %>% 
  summarise(s = sum(sector_time)) %>% pull(s)

herta_laps <- df %>% 
  filter(driver_name == "Herta") %>% 
  select(lap, tod)

herta_laps$tod[2] - herta_laps$tod[1]
