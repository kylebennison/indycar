% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_timing.R
\name{filter_pits}
\alias{filter_pits}
\title{Filter Out Pit In and Pit Out Laps}
\usage{
filter_pits(df)
}
\arguments{
\item{df}{a laps df}
}
\value{
tibble
}
\description{
Filter Out Pit In and Pit Out Laps
}
\examples{
no_pits <- laps \%>\% preprocess_laps() \%>\%  filter_pits()
}
