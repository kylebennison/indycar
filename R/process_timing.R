#' Load Data from racetools.com
#'
#' Loads a zip file from the internet, unzips it, and returns it as a tibble dataframe.
#'
#' @param url A url to a zip file (ideally from racetools.com)
#'
#' @return tibble
#' @export
#'
#' @examples
load_data <- function(url){

  # Create a tempfile
  temp <- tempfile()

  # Download from URL
  download.file(url, temp)
  # Unzip to tmp/ folder
  unzip(temp, exdir = "tmp/")
  fname <- list.files("tmp", pattern = ".csv")
  df <- data.table::fread(glue::glue("tmp/{fname}"))
  # Remove temp and tmp folders and files
  unlink(temp)
  unlink("tmp", recursive = TRUE)

  return(tibble::as_tibble(df))
}


#' Get the fastest lap for a list of drivers
#'
#' @param df A dataframe of laptimes and drivers
#' @param drivers The list of drivers to return. Defaults to all.
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' get_fastest(df = df, drivers = c("Palou", "O'Ward"))
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
#' @param df
#' @param traffic_gap double (optional): gap in seconds inside of which driver is
#' defined as being in traffic
#'
#' @return
#' @export
#'
#' @examples
preprocess_laps <- function(df, traffic_gap = .75){
  colnames(df) <- janitor::make_clean_names(colnames(df))

  options(digits.secs=4)
  df <- df %>%
    dplyr::mutate(tod = strptime(tod, format="%H:%M:%OS"))

  ##### Add Cumulative Sector Times to Lap DF #####
  sectors <- preprocess_sectors(df)

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
    ungroup() %>%
    dplyr::mutate(
      car_ahead_tod = dplyr::lag(tod, 1L, order_by = tod),
      car_behind_tod = dplyr::lead(tod, 1L, order_by = tod),
      gap_to_car_ahead = car_ahead_tod - tod,
      gap_to_car_behind = car_behind_tod - tod
    )

  # Track Status Update
  df <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(status_desc = case_when(
      status == "P" ~ "Pit In",
      dplyr::lag(status, 1L, order_by = lap) == "P" ~ "Pit Out",
      status == "Y" ~ "Yellow",
      status == "" & dplyr::lag(status, 1L, order_by = lap) == "Y" ~ "Yellow Ending",  # The end of this lap the green flag flies
      status == "" & dplyr::lag(status, 2L, order_by = lap) == "Y" ~ "Restart",  # This is the first full lap back under green
      TRUE ~ status
    )) %>%
    dplyr::mutate(is_yellow = if_else(str_detect(status_desc, "Yellow|Y"), 1, 0))

  # Tire Stint
  df <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(n_stops = cumsum(status == "P"),
           tire_stint = cumsum(status_desc == "Pit Out") + 1)

  # tire life
  df <- df %>%
    dplyr::group_by(driver_name, tire_stint) %>%
    dplyr::mutate(tire_life = row_number())

  # Define traffic
  # Traffic is being close to the car ahead (usually multiple cars)
  # Stuck in traffic is following close and not improving position over multiple corners
  # You are going slower than you'd like, evidenced by the fact that when you finally do pass
  # you pull away from the car behind
  # TODO: Calc using sectors, not laps, then apply to full lap.
  df <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(
      in_traffic = if_else(
        gap_to_car_ahead > -traffic_gap  # close to car ahead
        & dplyr::lag(pos, 1L, order_by = lap) == pos,  # Not improving position
        1,
        0
      ),
      # NOTE: This does not account for being held up by lap traffic, only people ahead
      # in position
      was_held_up = if_else(
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
           overtakes = if_else(pos_gained > 0, pos_gained, 0))

  return(df)
}

preprocess_sectors <- function(df){

  ## Calc TOD at each sector to calc car ahead regardless of lap
  sectors <- df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(lap_start_tod = dplyr::lag(tod, n=1L, order_by=lap)) %>%
    dplyr::mutate(lap_start_tod = if_else(
      lap == 1,
      tod - lap_time,
      lap_start_tod)) %>%  # first lap start time is calculated by subtracting lap 1 lap time from lap 1 finish TOD
    pivot_longer(cols = matches("s[0-9]+"), names_to = "sector", values_to = "sector_time")

  # NA missing sector times
  sectors <- sectors %>%
    dplyr::group_by(driver_name, lap) %>%
    dplyr::mutate(missing_sectors = sum(sector_time == 0)) %>%
    dplyr::mutate(sector_time = if_else(
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
      car_ahead_s = if_else(
        is.na(sector_complete_tod),
        NA,
        dplyr::lag(sector_complete_tod, n = 1L, order_by = sector_complete_tod) - sector_complete_tod
      ),
      driver_ahead = if_else(
        is.na(sector_complete_tod),
        NA,
        dplyr::lag(driver_name, n = 1L, order_by = sector_complete_tod)
      )
    )

  # This should be running order instead of position
  sectors %>%
    arrange(sector, sector_complete_tod)

  # TODO: Create mini-laps and get running order for all non-NA mini-laps, then calc overtakes

  return(sectors)
}

#' Filter Out Yellow Flag Laps
#'
#' Filters out yellow flag laps.
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
filter_yellow <- function(df){
  return(df %>%
    dplyr::filter(is_yellow == 0))
}


#' Filter Out Slow Laps
#'
#' Filters laps slower than designated race pace (defaults to 107% of fastest
#' lap time).
#'
#' @param df
#' @param race_pace
#'
#' @return
#' @export
#'
#' @examples
filter_slow <- function(df, race_pace = 1.07){
  return(df %>%
           dplyr::filter(lap_time <= min(df$lap_time) * race_pace))
}

#' Filter Out Pit In and Pit Out Laps
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
filter_pits <- function(df){
  return(df %>%
           dplyr::filter(!status_desc %in% c("Pit In", "Pit Out")))
}
