% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownHelpers.R
\name{md.tableWriter.DF.w.dimnames}
\alias{md.tableWriter.DF.w.dimnames}
\title{md.tableWriter.DF.w.dimnames}
\usage{
md.tableWriter.DF.w.dimnames(
  df,
  FullPath = ww.set.path_of_report(),
  percentify = FALSE,
  title_of_table = NA,
  print2screen = FALSE,
  WriteOut = FALSE
)
}
\arguments{
\item{df}{Input data frame to be plotted}

\item{FullPath}{Full path to the file.}

\item{percentify}{Format numbers between 0-1 to percentages 0-100.}

\item{title_of_table}{Title above the table (in the markdown report).}

\item{print2screen}{Print the markdown formatted table to the sceen.}

\item{WriteOut}{Write the table into a TSV file.}
}
\description{
Take an R data frame with row- and column- names, parse a markdown table from it,
and write it to the markdown report, set by "path_of_report".
}
\examples{
df <- matrix(1:9, 3)
rownames(df) <- 6:8
rownames(df) <- 9:11
md.tableWriter.DF.w.dimnames(df, percentify = FALSE, title_of_table = NA)
}
