% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_n_missing_df.r
\name{get_n_missing_df}
\alias{get_n_missing_df}
\title{Get the number of missing values for each column.}
\usage{
get_n_missing_df(.dat, ...)
}
\arguments{
\item{.dat}{Input tibble or dataframe.}

\item{...}{Additional arguments to pass to \code{\link[=get_n_perc_txt]{get_n_perc_txt()}}.}
}
\value{
A tibble with 2 columns. One for the column and one for the number of missing values.
}
\description{
Get the number of missing values for each column.
}
