% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownReports.R
\name{wscatter.fill}
\alias{wscatter.fill}
\title{wscatter.fill}
\usage{
wscatter.fill(
  df2col = cbind(A = rnorm(100), B = rnorm(100)),
  ...,
  color,
  xlim = range(df2col[, 1]),
  ylim = range(df2col[, 2]),
  zlim = range(color),
  nlevels = 20,
  pch = 21,
  cex = 1,
  plotname = substitute(df2col),
  plot.title = plotname,
  plot.axes,
  key.title,
  key.axes,
  asp = NA,
  xaxs = "i",
  yaxs = "i",
  las = 1,
  axes = TRUE,
  frame.plot = axes,
  xlab,
  ylab,
  savefile = unless.specified("b.save.wplots"),
  w = unless.specified("b.defSize", 7),
  h = w,
  incrBottMarginBy = 0,
  mdlink = ww.set.mdlink(),
  PNG = unless.specified("b.usepng", F)
)
}
\arguments{
\item{df2col}{Input data, a 2 column dataframe}

\item{...}{Pass any other parameter of the corresponding plotting function (most of them should
work).}

\item{color}{Filling color of the symbols}

\item{xlim}{Manually set the range of canvas in X dimension}

\item{ylim}{Defines the Y axis range. Replacement for the standard "ylim" argument.}

\item{zlim}{Manually set the range of colors numbers (Z dimension)}

\item{nlevels}{Number of steps in the color gradient}

\item{pch}{Define the symbol for each data point. A number (0-25) or any string between ""-s.}

\item{cex}{Size of the symbols}

\item{plotname}{The name of the file saved.}

\item{plot.title}{The title of the plot.}

\item{plot.axes}{Draw axis ticks}

\item{key.title}{...}

\item{key.axes}{...}

\item{asp}{numeric, giving the aspect ratio y/x. See help('plot.window').}

\item{xaxs}{The style of axis interval calculation to be used for the X-axis. See help('par').}

\item{yaxs}{The style of axis interval calculation to be used for the X-axis. See help('par').}

\item{las}{numeric in {0, 1, 2, 3}; the style of axis labels. See help('par').}

\item{axes}{Draw axes and box}

\item{frame.plot}{No description.}

\item{xlab}{X axis label}

\item{ylab}{Y axis label}

\item{savefile}{Save plot as pdf in OutDir, TRUE by default.}

\item{w}{Width of the saved pdf image, in inches.}

\item{h}{Height of the saved pdf image, in inches.}

\item{incrBottMarginBy}{Increase the blank space at the bottom of the plot. Use if labels do not
fit on the plot.}

\item{mdlink}{Insert a .pdf and a .png image link in the markdown report, set by
"path_of_report".}

\item{PNG}{Set to true if you want to save the plot as PNG instead of the default PDF.}
}
\description{
A scatterplot with color gradient and color legend. Modified from:
http://stackoverflow.com/questions/20127282/r-color-scatterplot-points-by-col-value-with-legend
}
\examples{
try.dev.off()
mydf <- cbind("A" = rnorm(100), "B" = rnorm(100))
wscatter.fill(
  df2col = mydf, color = rnorm(100), nlevels = 15, pch = 21,
  xlab = "The X Dimension. Wooaaahh"
)
}
