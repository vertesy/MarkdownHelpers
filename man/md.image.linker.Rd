% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownHelpers.R
\name{md.image.linker}
\alias{md.image.linker}
\title{md.image.linker}
\usage{
md.image.linker(fname_wo_ext, OutDir_ = ww.set.OutDir())
}
\arguments{
\item{fname_wo_ext}{Name of the image file where markdown links going to point to.}

\item{OutDir_}{The output directory (absolute / full path).}
}
\description{
Format a markdown image reference (link) to a .pdf and .png versions of graph,
and insert both links to the markdown report, set by "path_of_report".
If the "b.png4Github" variable is set, the .png-link is set up such,
that you can upload the whole report with the .png image into your GitHub repo's wiki,
under "Reports"/OutDir/ (Reports is a literal string, OutDir is the last/deepest
directory name in the "OutDir" variable. See create_set_OutDir() function.).
This function is called by the ~wplot functions.
}
\examples{
md.image.linker(fname_wo_ext = "MyPlot")
}
