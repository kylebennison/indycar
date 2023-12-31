% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_timing.R
\name{preprocess_laps}
\alias{preprocess_laps}
\title{Clean lap data and add features}
\usage{
preprocess_laps(df, traffic_gap = 0.75)
}
\arguments{
\item{df}{a laps dataframe}

\item{traffic_gap}{double (optional): gap in seconds inside of which driver is
defined as being in traffic}
}
\value{
tibble
}
\description{
Does all the following:
\itemize{
\item Cleans column names
\item converts timing to POSIX
\item adds cumulative sector times per lap
\item adds positions
\item adds cumulative lap times
\item adds gaps to leader, car ahead, car behind, next position
\item track status
\item Yellow flag indicator
\item tire stint
\item tire life
\item number of stops
\item number of positions gained/lost and overtakes
}
}
\examples{
processed_df <- laps \%>\% preprocess_laps()
}
