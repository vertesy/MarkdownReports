rm(list=ls(all.names = TRUE)); try(dev.off())

# Functions ------------------------

try (source ('/Users/abelvertesy/TheCorvinas/R/CodeAndRoll.R'),silent= F)
library(stringr)

# devtools::install_github(repo = "vertesy/RoxygenReady/RoxygenReady")
require(RoxygenReady)

pp = "/Users/abelvertesy/MarkdownReports/MarkdownReports.R"
RoxygenReady(pp)



