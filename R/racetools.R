cache_name <- function(){
  paste0(dirname(tempdir()), "/indycar")
}

#' Create cache folder for faster loading of data
#'
#' @return None
#' @export
enable_cache <- function(){
  newdir <- paste0(dirname(tempdir()), "/indycar")
  dir.create(newdir)
  message(glue::glue("New cache directory created at {newdir}"))
}


#' Save file as an RDS to indycar cache
#'
#' @param df a tibble
#' @param filename the name of the file (should match the original name of the file read in)
#'
#' @return None
save_to_cache <- function(df, filename){
  if(!dir.exists(cache_name())){
    enable_cache()
  }
  filename <- paste0(cache_name(), "/", filename)
  saveRDS(df, file=filename)
  message(glue::glue("Saved copy to cache at {filename}"))
  return(filename)
}


#' Load Data from racetools.com
#'
#' Loads a zip file from the internet, unzips it, and returns it as a tibble dataframe.
#'
#' @param url A url to a zip file (ideally from racetools.com)
#'
#' @return a tibble of laps data
#' @export
#'
load_data <- function(url){

  # Formulate what the resulting filename will be
  spaces <- gsub("%20", " ", basename(url))
  parens <- gsub("\\([a-zA-Z0-9 \\.]+\\)", "", spaces)
  scores <- gsub("__", "_", parens)
  fname_rds <- gsub("zip", "rds", scores)

  out <- tryCatch({
    expected_loc <- paste0(cache_name(), "/", fname_rds)
    message(glue::glue("Searching cache at {expected_loc}..."))
    df <- readRDS(expected_loc)
    message("Success")
  },
  warning=function(cond){
    message("Couldn't find cached file. Loading from url...")
    # Create a tempfile
    temp <- tempfile()

    # Download from URL
    download.file(url, temp)
    # Unzip to tmp/ folder
    unzip(temp, exdir = "tmp/")
    fname <- list.files("tmp", pattern = ".csv")
    df <- data.table::fread(glue::glue("tmp/{fname}"))
    # Save to cache as an RDS
    save_to_cache(tibble::as_tibble(df), fname_rds)
    # Remove temp and tmp folders and files
    unlink(temp)
    unlink("tmp", recursive = TRUE)

    return(tibble::as_tibble(df))
  })

  return(df)

}

# Get Racetools Data
# TODO:
# Add another func for non-interactive downloads. Just call it get_data().

#' Get the URL to a session zip file from racetools.com.
#'
#' This function will prompt you several times for which race you are interested in.
#'
#' @param root_url the base url to do the search for. Defaults to the current season in Indycar on racetools.com
#'
#' @return str: the url of the session of interest
#' @export
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
