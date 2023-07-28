# Get Racetools Data
# TODO:
# Add another func for non-interactive downloads. Just call it get_data().

#' Get the URL to a session zip file from racetools.com.
#'
#' This function will prompt you several times for which race you are interested in.
#'
#' @param root_url
#'
#' @return
#' @export
#'
#' @examples
get_url <- function(root_url="https://racetools.com/logfiles/IndyCar/2023/"){

  res <- readLines(root_url)

  # Get list of races
  pattern <- "[0-9]+\\s\\-\\s[A-Za-z\\s\\.]+"
  races <- stringr::str_extract_all(string=res, pattern=pattern)[[3]]

  # Ask user which race they want
  print("Races:")
  print(races)
  response <- readline(glue::glue("Pick Race 1-{length(races)}. \n"))

  # Find closest match to what they typed
  race_name <- races[as.integer(response)]

  # Get list of files from a race
  race_page <- readLines(URLencode(paste0(root_url, race_name)))

  pattern <- "[0-9A-Za-z\\-\\s\\_\\(\\)\\.\\%]+.zip"
  files <- stringr::str_extract_all(string = race_page, pattern = pattern)
  files <- files[[3]]

  # Only keep encoded urls
  return_files <- files[grepl("\\%", files)]

  # Return File URL to use with load_data
  print("URLs:")
  print(return_files)
  response <- readline(glue::glue("Pick a file 1-{length(return_files)}. \n"))

  url <- return_files[as.integer(response)]

  return(URLencode(paste0(root_url, URLencode(race_name), "/", url)))
}

#' Get a dataframe of session data from racetools.com
#'
#' Provide the event and session and we will attempt to find the zip file,
#' download and unzip it, and return the dataframe.
#'
#' @param event (string): the name of the event weekend (ex. "Toronto").
#' @param session (string): c("Practice", "Qualifications", "Race")
#' @param root_url (string): the url to start the search in. You would only
#' update this if you wanted a different year's data.
#'
#' @return dataframe
#' @export
#'
#' @examples
get_data <- function(event, session, root_url="https://racetools.com/logfiles/IndyCar/2023/"){

  res <- readLines(root_url)

  # Get list of races
  pattern <- "[0-9]+\\s\\-\\s[A-Za-z\\s\\.]+"
  races <- stringr::str_extract_all(string=res, pattern=pattern)[[3]]

  # Find race that contains the string they typed.
  race_name <- races[stringr::str_detect(races, event)]

  # Get list of files from a race
  race_page <- readLines(URLencode(paste0(root_url, race_name)))

  pattern <- "[0-9A-Za-z\\-\\s\\_\\(\\)\\.\\%]+.zip"
  files <- stringr::str_extract_all(string = race_page, pattern = pattern)
  files <- files[[3]]

  # Only keep encoded urls
  return_files <- files[grepl("\\%", files)]

  # Keep only files that match the session of interest
  return_files <- return_files[stringr::str_detect(return_files, session)]

  # Filter to indycar only
  # NOTE: All indycar sessions have "(XX.I)" in their name, e.g. "(P1.I)" or "(R.I)"
  url <- return_files[stringr::str_detect(return_files, "\\([A-Z0-9]+\\.I\\)")]


  # Return File URL to use with load_data
  url <- URLencode(paste0(root_url, URLencode(race_name), "/", url))

  df <- load_data(url=url)

  return(df)
}
