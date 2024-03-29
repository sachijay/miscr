% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cat_summary.r
\name{get_cat_summary}
\alias{get_cat_summary}
\title{Generates a summary table for a categorical variable}
\usage{
get_cat_summary(
  .data,
  .cat_var,
  ...,
  .output_digits = 1,
  .with_percentage = TRUE,
  .drop = dplyr::group_by_drop_default(.data)
)
}
\arguments{
\item{.data}{A data frame or a data frame extension (e.g. a tibble).}

\item{.cat_var}{Primary (categorical) variable of interest.}

\item{...}{Additional variables to count by. This can be left blank.}

\item{.output_digits}{Number of decimal points in the output percentages. The default is set to 1.}

\item{.with_percentage}{A logical indicating if the percentage mark (\%) should be shown. The default is \code{TRUE}.}

\item{.drop}{Drop groups formed by factor levels that don't appear in the data. Passed directly to \code{dplyr::group_by()}.}
}
\value{
A tibble with counts and percentages for \code{.cat_var} grouped by variables in \code{...}.
}
\description{
Generates a summary table for a categorical variable
}
\details{
The percentages are calculated for each set of \code{...} combinations.
}
\note{
The \code{.cat_var} and \code{...} are converted to factor variables.
}
\examples{
library(dplyr)

starwars \%>\% 
get_cat_summary(.cat_var = gender,
                species, eye_color)
}
