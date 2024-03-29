% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownHelpers.R
\name{filter_LP}
\alias{filter_LP}
\title{filter_LP}
\usage{
filter_LP(
  numeric_vector,
  threshold,
  passequal = FALSE,
  prepend = "",
  return_survival_ratio = FALSE,
  return_conclusion = FALSE,
  na.rm = TRUE,
  plot.hist = TRUE,
  saveplot = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{numeric_vector}{Values to be filtered.}

\item{threshold}{A numeric value below which "numeric_vector" passes.}

\item{passequal}{Pass if a value is smaller, or equal than the threshold. FALSE by default.}

\item{prepend}{Text prepended to the results.}

\item{return_survival_ratio}{Return a number with the survival ratio (TRUE).
or a logical index vector of the survivors (FALSE). return_conclusion must be FALSE}

\item{return_conclusion}{Return conclusion sentence that (also printed). return_survival_ratio must be FALSE}

\item{na.rm}{Remove NA-s? Default: TRUE}

\item{plot.hist}{Plot the histogram of the input data}

\item{saveplot}{Save the histogram as PDF, FALSE by defeault}

\item{verbose}{print output to console? Default: yes.}

\item{...}{Additional arguments for the histogram}
}
\description{
Filter values that fall below the low-pass threshold (X <).
}
\examples{
filter_LP(
  numeric_vector = rnorm(1000, 6), threshold = 5,
  prepend = "From all values ", return_survival_ratio = FALSE
)
}
