######################################################################################################
# Recompile_the_MarkdownReports_Package.R
######################################################################################################
# source("~/MarkdownReports/Development/Recompile_the_MarkdownReports_Package.R")
rm(list=ls(all.names = TRUE));
try(dev.off())

# Functions ------------------------
# devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
try (source ('~/TheCorvinas/R/CodeAndRoll.R'),silent= F)
require(devtools)
require(roxygen2)
require("stringr")

# Setup ------------------------

RepositoryDir = 	"~/MarkdownReports/"
PackageName = 	"MarkdownReports"
fname = 	kollapse(PackageName,".R")

PackageDir = kollapse(RepositoryDir, PackageName)
Package_FnP = 	kollapse(PackageDir, "/R/", fname)
DESCRIPTION <- list("Title" = "Generate Scientific Figures and Reports Easily",
					"Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "a.vertesy@hubrecht.eu", role = c("aut", "cre"))',
					"Description" = "An R function library to create scientific figures and reports easily.",
					"License" = "GNU GPL 3"
)


# Compile a package ------------------------------------------------
setwd("~/MarkdownReports/MarkdownReports/")
getwd()
document()

# Install your package ------------------------------------------------
setwd(RepositoryDir)
install(PackageName)
require("MarkdownReports")

# Test your package ------------------------------------------------
help("wplot")

# # Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
# require("MarkdownReports")
#
# # Clean up if not needed anymore ------------------------------------------------
# # installed.packages()
# remove.packages("MarkdownReports")
