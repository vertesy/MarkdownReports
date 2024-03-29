% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownReports.R
\name{wLinRegression}
\alias{wLinRegression}
\title{wLinRegression}
\usage{
wLinRegression(
  DF,
  coeff = c("pearson", "spearman", "r2")[3],
  textlocation = "topleft",
  cex = 1,
  OverwritePrevPDF = unless.specified("b.save.wplots"),
  ...
)
}
\arguments{
\item{DF}{The same dataframe as you provided to wplot() before you called this function}

\item{coeff}{What coefficient to display? Either "all", "pearson", "spearman"
correlation values or "r2" for the Coefficient of Determination.}

\item{textlocation}{where to put the legend?}

\item{cex}{font size; 1 by default}

\item{OverwritePrevPDF}{Save the plot immediately with the same name the last
wplot* function made (It is stored in plotnameLastPlot variable). Never inserts an mdlink.}

\item{...}{Additional parameters for the line to display.}
}
\description{
Add linear regression, and descriptors to line to your scatter plot.
Provide the same dataframe as you provided to wplot() before you called this function
}
\examples{
try.dev.off()
x <- cbind(a = rnorm(1:10), b = rnorm(10))
wplot(x)
# wLinRegression(x, coeff = c("pearson", "spearman", "r2")[3])
}
