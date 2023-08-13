#' List of teams and colors
#'
#' @description
#' Team names and primary car colors
#'
#' @export
team_colors <- c(
  "Juncos" = "#11bf4e",
  "McLaren" = "#FF8000",
  "Meyer Shank" = "#de0bd7",
  "Ed Carpenter" = "#adadad",
  "Rahal" = "#3243fc",
  "Penske" = "black",
  "Andretti" = "#e9f502",
  "Dale Coyne" = "#686e04",
  "Ganassi" = "#cf1130",
  "Foyt" = "#525252"
)

#' List of teams and enging manufacturers
#'
#' @description
#' Team name and engine manufacturer name.
#'
#' @export
team_manufacturers <- c(
  "Juncos" = "Chevy",
  "McLaren" = "Chevy",
  "Meyer Shank" = "Honda",
  "Ed Carpenter" = "Chevy",
  "Rahal" = "Honda",
  "Penske" = "Chevy",
  "Andretti" = "Honda",
  "Dale Coyne" = "Honda",
  "Ganassi" = "Honda",
  "Foyt" = "Chevy"
)

#' Get Team Colors For a Given Team
#'
#' @param team_name (str): team name
#'
#' @description
#' Gets a team's primary colors from a team name
#'
#' @return (character): hex color of team
#'
#' @export
get_team_colors <- function(team_name){
  return(team_colors[team_name] %>% as.character())
}

#' List of drivers and teams
#'
#' @description
#' Driver last name and team name
#'
#' @export
driver_teams <- c(
  "Rossi" = "McLaren",
  "Dixon" = "Ganassi",
  "Herta" = "Andretti",
  "Palou" = "Ganassi",
  "Power" = "Penske",
  "McLaughlin" = "Penske",
  "Newgarden" = "Penske",
  "Kirkwood" = "Andretti",
  "O'Ward" = "McLaren",
  "Ilott" = "Juncos",
  "Lundgaard" = "Rahal",
  "Armstrong" = "Ganassi",
  "Rahal" = "Rahal",
  "Castroneves" = "Meyer Shank",
  "Pagenaud" = "Meyer Shank",
  "Rosenqvist" = "McLaren",
  "DeFrancesco" = "Andretti",
  "Grosjean" = "Andretti",
  "Malukas" = "Dale Coyne",
  "Ericsson" = "Ganassi",
  "Pedersen" = "Foyt",
  "VeeKay" = "Ed Carpenter",
  "Canapino" = "Juncos",
  "Harvey" = "Rahal",
  "Lundqvist" = "Meyer Shank",
  "Hunter-Reay" = "Ed Carpenter",
  "Ray Robb" = "Dale Coyne",
  "Ferrucci" = "Foyt"
)


#' Get list of driver names and team colors
#'
#' @return list
#' @export
get_driver_colors <- function(){
  driver_colors <- c(team_colors[driver_teams])
  names(driver_colors) <- names(driver_teams)
  return(driver_colors)
}


# Plots -------------------------------------------------------------------
# TODO:
#' Green Flag Lap Times by number of stops
#'
#' @param df (tibble): a laps df
#' @param drivers (list): a list of drivers. Defaults to all drivers.
#'
#' @return a gt object
#' @export
table_pit_strategy_lap_times <- function(df, drivers){

  # If list of drivers isn't provided, return all drivers
  if(missing(drivers)){
    drivers <- get_top_n_drivers(df, 50)
  }

  df %>%
    dplyr::filter(driver_name %in% drivers) %>%
    filter_yellow() %>%
    filter_pits() %>%
    filter_slow() %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(total_stops = max(n_stops)) %>%
    dplyr::filter(total_stops != 0) %>%
    dplyr::group_by(total_stops) %>%
    dplyr::summarise(avg_lap_time = mean(lap_time), std_dev_lap_time = sd(lap_time)) %>%
    gt::gt() %>%
    gt::fmt_auto() %>%
    gt::tab_header(title = "Green Flag Lap Times", subtitle = "By Strategy") %>%
    gt::cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>%
    gt::data_color(columns = c(avg_lap_time, std_dev_lap_time), palette = "viridis")
}

plot_lap_times <- function(df, drivers){

  # If list of drivers isn't provided, return all drivers
  if(missing(drivers)){
    drivers <- get_top_n_drivers(df, 50)
  }

  df %>%
    dplyr::filter(driver_name %in% drivers) %>%
    filter_yellow() %>%
    filter_pits() %>%
    filter_slow() %>%
    ggplot2::ggplot(ggplot2::aes(x=lap, y=lap_time, color=driver_name)) +
    ggplot2::geom_line(size=2,alpha=.5) +
    indycar_theme +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "grey90"),
          plot.caption.position = "plot") +
    ggplot2::labs(color = "Driver", x = "Lap", y = "Time (s)", caption = "Green non-pit laps only")
}

plot_laps_boxplot <- function(df){
  df %>%
    filter_pits() %>%
    filter_slow() %>%
    filter_yellow() %>%
    dplyr::group_by(driver_name) %>%
    dplyr::mutate(median_lap = median(lap_time)) %>%
    ggplot2::ggplot(ggplot2::aes(x=lap_time, y=reorder(driver_name, desc(median_lap)))) +
    ggplot2::geom_boxplot(ggplot2::aes(fill=driver_name), alpha = .7) +
    indycar_theme +
    ggplot2::labs(x = "Lap Time",
         y = "") +
    ggplot2::scale_fill_manual(values = get_driver_colors(), guide="none")
}

# Pit Delta
# TODO: If under yellow, compare delta to yellow lap time
table_pit_delta <- function(df){

  green_lap_time <- df %>%
    get_field_green_lap_time()

  yellow_lap_time <- df %>%
    get_field_yellow_lap_time()


  df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::filter(stringr::str_detect(status_desc, "Pit")) %>%
    dplyr::mutate(pit_delta = if_else(is_yellow == 1,
                                      lap_time - yellow_lap_time,
                                      lap_time - green_lap_time)) %>%
    dplyr::mutate(pit_stop = (dplyr::row_number() + 1) %/% 2) %>%
    dplyr::group_by(driver_name, pit_stop) %>%
    dplyr::summarise(total_pit_delta = sum(pit_delta)) %>%
    gt::gt() %>%
    gt::tab_header(title = "Pit Stop Delta", subtitle = "Time Lost Over Avg. Green Flag Lap (s)") %>%
    gt::cols_label_with(
      fn = function(x)
        (janitor::make_clean_names(x, case = "title"))
    ) %>%
    gt::fmt_auto()
}

# # Top/bottom n overtakers
# TODO: Do at sector level too
table_overtakes <- function(df, n=10, top=TRUE){

  if(top){
    slice_df <- head
  } else {
    slice_df <- tail
  }

df %>%
  dplyr::summarise(total_overtakes = sum(overtakes, na.rm=TRUE)) %>%
  dplyr::arrange(desc(total_overtakes)) %>%
  slice_df(n) %>%
  gt::gt() %>%
  gt::tab_header(title = "Overtakes for Position", subtitle = "Top 10 Overtakers") %>%
  gt::cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>%
  gt::fmt_auto()

}


# Tires -------------------------------------------------------------------

# Tire age
plot_tire_life <- function(df) {

  df %>%
    filter_yellow() %>%
    filter_slow(1.5) %>%
    filter_pits() %>%
    dplyr::group_by(tire, tire_life) %>%
    dplyr::summarise(lap_time = mean(lap_time)) %>%
    ggplot2::ggplot(ggplot2::aes(x=tire_life, y=lap_time, color=tire)) +
    ggplot2::geom_line(size=2, alpha=.7) +
    indycar_theme +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      plot.caption.position = "plot") +
    ggplot2::labs(
      color = "Tire",
      x = "Tire Life",
      y = "Time (s)",
      caption = "Green non-pit laps only")
}

# Driver stints
table_tire_stints <- function(df){

  df %>%
    dplyr::group_by(driver_name, tire_stint, tire) %>%
    dplyr::summarise(stint_length = max(tire_life), stint_pace = mean(lap_time), stint_std_dev = sd(lap_time)) %>%
    dplyr::ungroup() %>%
    gt::gt() %>%
    gt::tab_header(title = "Tire Stints") %>%
    gt::cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>%
    gt::fmt_auto()
}


# Fastest Laps ------------------------------------------------------------

# Fastest laps overall
table_fastest_laps <- function(df, n=10){

  df %>%
    dplyr::ungroup() %>%
    dplyr::slice_min(lap_time, n = n) %>%
    dplyr::select(driver_name, lap, lap_time) %>%
    gt::gt() %>%
    gt::tab_header(title = "Fastest Laps Overall") %>%
    gt::cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>%
    gt::fmt_auto() %>%
    gt::data_color(columns = c(lap_time), palette = "viridis", reverse = TRUE)
}

# Each driver's fast lap
# By driver
table_fastest_laps_driver <- function(df){

  df %>%
    dplyr::group_by(driver_name) %>%
    dplyr::slice_min(lap_time) %>%
    dplyr::select(driver_name, lap, lap_time) %>%
    dplyr::arrange(lap_time) %>%
    dplyr::ungroup() %>%
    gt::gt() %>%
    gt::tab_header(title = "Fastest Lap by Driver") %>%
    gt::cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>%
    gt::fmt_auto() %>%
    gt::data_color(columns = c(lap_time), palette = "viridis", reverse = TRUE)
}


# Sectors -----------------------------------------------------------------

## Sector Analysis
# Who was chasing who at the end of the race?
table_sectors_following <- function(sectors, gap=1){

  sectors %>% dplyr::group_by(driver_name, driver_ahead) %>%
    filter_slow() %>%
    filter_yellow() %>%
    filter_pits() %>%
    dplyr::filter(car_ahead_s > -gap) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::group_by(driver_name) %>%
    dplyr::slice_max(n) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::ungroup() %>%
    gt::gt() %>%
    gt::fmt_auto() %>%
    gt::cols_label_with(fn = function(x) (janitor::make_clean_names(x, case = "title"))) %>%
    gt::data_color(columns = where(~is.numeric(.x)), palette = "viridis") %>%
    gt::tab_header(title = "Following Closely", subtitle = glue::glue("Sectors within {gap} second of car ahead"))
}

# Heatmap
# Sector heatmap
plot_sector_quickest_heatmap <- function(df){

  quickest_lap <- df %>%
    get_fastest() %>%
    dplyr::ungroup() %>%
    filter_slow() %>%
    dplyr::select(driver_name, lap_time, matches("s[0-9]+")) %>%
    dplyr::select(!matches("cum")) %>%
    tidyr::pivot_longer(cols = !c(driver_name, lap_time), names_to = "sector", values_to = "time") %>%
    dplyr::group_by(sector) %>%
    dplyr::mutate(pct = round((time - min(time))/min(time), 4),
                  time_lost = round(time - min(time), 4)) %>%
    dplyr::group_by(driver_name, lap_time) %>%
    dplyr::mutate(row_num = dplyr::row_number())

  # Pct. based sectors
  quickest_lap %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text_color = dplyr::if_else(pct < .2 * max(pct) | pct > .8 * max(pct), "white", "black"),
                  sector = stringr::str_sub(sector, 2)) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(sector, row_num), y = reorder(driver_name, desc(lap_time)), fill = pct)) +
    ggplot2::geom_tile(alpha=1, width = .9, height=.95) +
    ggplot2::scale_fill_gradient2(low="blue", mid="white", high="red", midpoint = max(quickest_lap$pct)/2,
                                  labels=scales::percent) +
    ggplot2::geom_text(ggplot2::aes(label = scales::number(time, accuracy = .0001), color = dplyr::if_else(text_color=="white", "white", "black"))) +
    ggplot2::scale_color_identity() +
    ggplot2::theme(panel.background = ggplot2::element_blank()) +
    ggplot2::labs(title="Fastest Lap Sector Times",
                  subtitle="Colored by Percent Above Quickest in Sector",
                  fill="",
                  caption="@staturdays",
                  x = "Sector") +
    indycar_theme +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())

}
