% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownReports.R
\name{setup_logging_markdown}
\alias{setup_logging_markdown}
\title{setup_logging_markdown (deprecated, use with create_set_OutDir, will be removed from V3)}
\usage{
setup_logging_markdown(fname, title = "", append = TRUE, b.png4Github = TRUE)
}
\arguments{
\item{fname}{Name of the report file.}

\item{title}{Title of the report.}

\item{append}{Set append to TRUE if you do not want to overwrite the previous report.
Use continue_logging_markdown() if you return logging into an existing report.}

\item{b.png4Github}{A global variable, defined by this and used by the other functions.
If TRUE (default), any link to the .png versions of images will be created in a
GitHub compatible format. That means, when you upload your markdown report and the .png images
to your GitHub wiki under "Reports/" the links will correctly display the images online.}
}
\description{
Setup the markdown report file, create a sub directory in "OutDir". Its name is stamped with
the script name and the modification time. Create the "path_of_report" variable used by
all log-writing and ~wplot functions.
}
\examples{
setup_logging_markdown (fname = "Analysis.md", title = "My Analysis",
append = TRUE, b.png4Github = TRUE)
}
