% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownReports.R
\name{wbarplot_dfCol}
\alias{wbarplot_dfCol}
\title{wbarplot_dfCol}
\usage{
wbarplot_dfCol(
  df,
  ...,
  colName,
  col = unless.specified("b.def.colors", "gold1"),
  savefile = unless.specified("b.save.wplots"),
  w = unless.specified("b.defSize", 7),
  h = w,
  PNG = unless.specified("b.usepng", F)
)
}
\arguments{
\item{df}{Input data frame to be plotted}

\item{...}{Pass any other parameter of the corresponding
plotting function (most of them should work).}

\item{colName}{Which column to plot (by name).}

\item{col}{Color of the plot.}

\item{savefile}{Save plot as pdf in OutDir, TRUE by default.}

\item{w}{Width of the saved pdf image, in inches.}

\item{h}{Height of the saved pdf image, in inches.}

\item{PNG}{Set to true if you want to save the plot as PNG instead of the default PDF.}
}
\description{
wbarplot for a column of a data frame.
}
\examples{
try.dev.off()
df <- cbind(a = rnorm(1:10), b = rnorm(10))
wbarplot_dfCol(df, colName = "a", col = "gold1", w = 7)
}
