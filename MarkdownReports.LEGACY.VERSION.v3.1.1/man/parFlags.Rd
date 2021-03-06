% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownReports.R
\name{parFlags}
\alias{parFlags}
\title{parFlags}
\usage{
parFlags(prefix = "", ..., pasteflg = T, collapsechar = ".")
}
\arguments{
\item{prefix}{Append something before?}

\item{...}{Paramter variables}

\item{pasteflg}{Boolean: paste the parameters-flags together?}

\item{collapsechar}{Separating character between each parameters-flag}
}
\description{
Create a string from the names of the (boolean) parameters (T or F) of true values. Use it for Suffixing plot names with the parameters that were used for that plot.
}
\examples{
pearson = T; filtered =T; normalized = F; MyPlotname = parFlags(prefix = "MyPlot" , pearson, filtered, normalized ); MyPlotname
}
