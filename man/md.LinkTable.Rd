% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownHelpers.R
\name{md.LinkTable}
\alias{md.LinkTable}
\title{md.LinkTable}
\usage{
md.LinkTable(tableOfLinkswRownames)
}
\arguments{
\item{tableOfLinkswRownames}{A dataframe where every entry is a string containing an html link.}
}
\description{
Take a dataframe where every entry is a string containing an html link, parse and write out.
a properly formatted markdown table.
}
\examples{
x <- data.frame(A = c("http://www.google.com", "http://www.yahoo.com"), B = c("http://www.bing.com", "http://www.duckduckgo.com"))
rownames(x) <- c("Google", "Yahoo")
md.LinkTable(x)

}
