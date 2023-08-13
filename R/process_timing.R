#' Get the fastest lap for a list of drivers
#'
#' @param df A dataframe of laptimes and drivers
#' @param drivers The list of drivers to return. Defaults to all.
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' get_fastest(df = laps, drivers = c("Grosjean", "O'Ward"))
get_fastest <- function(df, drivers=unique(df$driver_name)){
  return(df %>%
           dplyr::filter(driver_name %in% drivers) %>%
           dplyr::group_by(driver_name) %>%
           dplyr::slice_min(lap_time))
}

#' Clean lap data and add features
#'
#' Does all the following:
#' - Cleans column names
#' - converts timing to POSIX
#' - adds cumulative sector times per lap
#' - adds positions
#' - adds cumulative lap times
#' - adds gaps to leader, car ahead, car behind, next position
#' - track status
#' - Yellow flag indicator
#' - tire stint
#' - tire life
#' - number of stops
#' - number of positions gained/lost and overtakes
#'
#' @param df a laps dataframe
#' @param traffic_gap double (optional): gap in seconds inside of which driver is
#' defined as being in traffic
#'
#' @return tibble
#' @export
#'
#' @examples
#' processed_df <- laps %>% preprocess_laps()
preprocess_laps <- function(df, traffic_gap = .75){
  colnames(df) <- janitor::make_clean_names(colnames(df))

  options(digits.secs=4)
  df <- df %>%
    dplyr::mutate(tod = strptime(tod, format="%H:%M:%OS"))

  ##### Add Cumulative Sector Times to Lap DF #####
  sectors <- preprocess_sectors(df)

  # Pivot back
  df_cum <- sectors %>%
    tidyr::pivot_wider(id_cols = c(driver_name, lap),
                names_from = sector,
                values_from = cum_sector_time,
                names_glue = "{sector}_cum")


  # NOTE: Last sector is the s/f line, so cum sum time equals full lap time
  # Join back
  df <- df %>%
    dplyr::left_join(df_cum, by = c("driver_name", "lap"))

  # calc position at end of lap
  df <- df %>%
    dplyr::group_by(lap) %>%
    dplyr::mutate(pos = rank(tod))

  # Cum lap time
  df <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(cum_lap_time = cumsum(lap_time))

  # Gap to leader, gap to next, gap to car ahead and behind
  df <- df %>%
    dplyr::group_by(lap) %>%
    dplyr::mutate(leader_tod = min(tod),
           next_tod = dplyr::lag(tod, n = 1L, order_by = tod)
    ) %>%
    dplyr::mutate(gap_to_leader = leader_tod - tod,
           gap_to_next = next_tod - tod,
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      car_ahead_tod = dplyr::lag(tod, 1L, order_by = tod),
      car_behind_tod = dplyr::lead(tod, 1L, order_by = tod),
      gap_to_car_ahead = car_ahead_tod - tod,
      gap_to_car_behind = car_behind_tod - tod
    )

  # Track Status Update
  df <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(status_desc = dplyr::case_when(
      status == "P" ~ "Pit In",
      dplyr::lag(status, 1L, order_by = lap) == "P" ~ "Pit Out",
      dplyr::lag(status, 1L, order_by = lap) == "YP" ~ "Yellow Pit Out",
      status == "Y" ~ "Yellow",
      status == "YP" ~ "Yellow Pit In",
      status == "" & dplyr::lag(status, 1L, order_by = lap) == "Y" ~ "Yellow Ending",  # The end of this lap the green flag flies
      status == "" & dplyr::lag(status, 2L, order_by = lap) == "Y" ~ "Restart",  # This is the first full lap back under green
      TRUE ~ status
    )) %>%
    dplyr::mutate(is_yellow = dplyr::if_else(stringr::str_detect(status_desc, "Yellow|Y"), 1, 0))

  # Tire Stint
  df <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(n_stops = cumsum(status == "P" | status == "YP"),
           tire_stint = cumsum(status_desc == "Pit Out" | status_desc == "Yellow Pit Out") + 1)

  # tire life
  df <- df %>%
    dplyr::group_by(driver_name, tire_stint) %>%
    dplyr::mutate(tire_life = dplyr::row_number())

  # Define traffic
  # Traffic is being close to the car ahead (usually multiple cars)
  # Stuck in traffic is following close and not improving position over multiple corners
  # You are going slower than you'd like, evidenced by the fact that when you finally do pass
  # you pull away from the car behind
  # TODO: Calc using sectors, not laps, then apply to full lap.
  df <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(
      in_traffic = dplyr::if_else(
        gap_to_car_ahead > -traffic_gap  # close to car ahead
        & dplyr::lag(pos, 1L, order_by = lap) == pos,  # Not improving position
        1,
        0
      ),
      # NOTE: This does not account for being held up by lap traffic, only people ahead
      # in position
      was_held_up = dplyr::if_else(
        dplyr::lag(in_traffic, 1L, order_by = lap) == 1  # in traffic prev. lap
        & dplyr::lag(lap_time, 1L, order_by = lap) > lap_time  # lap time improved
        & dplyr::lag(pos, 1L, order_by = lap) > pos,  # advanced position (passed traffic)
        1,
        0
      ))

  # Number of passes made
  # TODO: Calc at sector level. instead then join back for a whole lap.
  df <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(pos_gained = dplyr::lag(pos, n = 1L, order_by = lap) - pos,
           overtakes = dplyr::if_else(pos_gained > 0, pos_gained, 0))

  return(df)
}

preprocess_sectors <- function(df){

  ## Calc TOD at each sector to calc car ahead regardless of lap
  sectors <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(lap_start_tod = dplyr::lag(tod, n=1L, order_by=lap)) %>%
    dplyr::mutate(lap_start_tod = dplyr::if_else(
      lap == 1,
      tod - lap_time,
      lap_start_tod)) %>%  # first lap start time is calculated by subtracting lap 1 lap time from lap 1 finish TOD
    tidyr::pivot_longer(cols = matches("s[0-9]+"), names_to = "sector", values_to = "sector_time")

  # NA missing sector times
  sectors <- sectors %>%
    dplyr::group_by(driver_name, lap) %>%
    dplyr::mutate(missing_sectors = sum(sector_time == 0)) %>%
    dplyr::mutate(sector_time = dplyr::if_else(
      sector_time == 0 & missing_sectors > 0,
      NA,
      sector_time
    ))

  # NOTE: lap 1 sector times are the sum of the lap number
  # So prev lap tod + current lap sector times == current lap tod
  # Calc cumsum sector time
  sectors <- sectors %>%
    dplyr::group_by(driver_name, lap) %>%
    dplyr::mutate(cum_sector_time = cumsum(sector_time))

  # Calc sector TOD
  sectors <- sectors %>%
    dplyr::mutate(sector_complete_tod = lap_start_tod + cum_sector_time)

  # Group by sector and order by TOD to calc car ahead at that sector.
  # Don't calculate car ahead for sector if you didn't pass through that sector.
  sectors <- sectors %>%
    dplyr::group_by(sector) %>%
    dplyr::mutate(
      car_ahead_s = dplyr::if_else(
        is.na(sector_complete_tod),
        NA,
        dplyr::lag(sector_complete_tod, n = 1L, order_by = sector_complete_tod) - sector_complete_tod
      ),
      driver_ahead = dplyr::if_else(
        is.na(sector_complete_tod),
        NA,
        dplyr::lag(driver_name, n = 1L, order_by = sector_complete_tod)
      )
    )

  # This should be running order instead of position
  sectors %>%
    dplyr::arrange(sector, sector_complete_tod)

  # TODO: Create mini-laps and get running order for all non-NA mini-laps, then calc overtakes

  return(sectors)
}

#' Filter Out Yellow Flag Laps
#'
#' Filters out yellow flag laps.
#'
#' @param df a laps df
#' @param keep_yellows (bool): whether to return only the yellow laps. Defaults to False.
#'
#' @return tibble
#' @export
#'
#' @examples
#' no_yellows <- laps %>% filter_yellow()
filter_yellow <- function(df, keep_yellows=FALSE){
  if(keep_yellows){
    keep <- 1
  } else{
    keep <- 0
  }
  return(df %>%
    dplyr::filter(is_yellow == keep))
}


#' Filter Out Slow Laps
#'
#' Filters laps slower than designated race pace (defaults to 107% of fastest
#' lap time).
#'
#' @param df a laps df
#' @param race_pace (double): percentage over fastest lap to filter out. Defaults to 1.07 (107%).
#'
#' @return a tibble
#' @export
#'
#' @examples
#' fast_laps <- laps %>% filter_slow(race_pace=1.1)
filter_slow <- function(df, race_pace = 1.07){
  return(df %>%
           dplyr::filter(lap_time <= min(df$lap_time) * race_pace))
}

#' Filter Out Pit In and Pit Out Laps
#'
#' @param df a laps df
#'
#' @return tibble
#' @export
#'
#' @examples
#' no_pits <- laps %>% filter_pits()
filter_pits <- function(df){
  return(df %>%
           dplyr::filter(!stringr::str_detect(status_desc, "Pit")))
}


get_field_green_lap_time <- function(df){

  green_lap_time <- df %>%
    filter_yellow() %>%
    filter_pits() %>%
    filter_slow(race_pace = 1.25) %>%
    pull(lap_time) %>%
    mean()

  return(green_lap_time)
}


get_field_yellow_lap_time <- function(df){

  yellow_lap_time <- df %>%
    filter_yellow(keep_yellows=TRUE) %>%
    pull(lap_time) %>%
    mean()

  return(yellow_lap_time)
}

get_top_n_drivers <- function(df, n=3){
  df %>%
    dplyr::filter(lap == max(lap) & pos <= n) %>% dplyr::pull(driver_name)
}
