% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownHelpers.R
\name{ww.md.image.link.parser}
\alias{ww.md.image.link.parser}
\title{ww.md.image.link.parser}
\usage{
ww.md.image.link.parser(...)
}
\arguments{
\item{...}{Variables (strings, vectors) to be collapsed in consecutively.}
}
\description{
Format a markdown image reference (link) from the file path to the file.
It can parse the file path, if you pass it in separate variables and strings.
E.g. ww.md.image.link.parser(Directory, "MyImage.png").
}
\examples{
ww.md.image.link.parser("/MyPlot.jpg")
ww.md.image.link.parser(getwd(), "/MyPlot.jpg")
}
