######################################################################################################
# Workflow_to_Create_an_R_Package.R
# 15 August 2017 (Tuesday) 12:28
######################################################################################################
# source("~/MarkdownReports/Workflow_to_Create_an_R_Package.R")
# rm(list=ls(all.names = TRUE));
try.dev.off()

# install.packages("devtools")
# Functions ------------------------
# devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
try (source ('~/Github_repos/TheCorvinas/R/CodeAndRoll.R'),silent= F)

require(devtools)
# install.packages("roxygen2")
require(roxygen2)
require("stringr")


kollapse <-function (..., print = T) {
  if (print == T) {
    print(paste0(c(...), collapse = ""))
  }
  paste0(c(...), collapse = "")
}

# Setup ------------------------

RepositoryDir = 	"~/Github_repos/MarkdownReports/"
PackageName = 	"MarkdownReports"
fname = 	kollapse(PackageName,".R")

PackageDir = kollapse(RepositoryDir, PackageName)
Package_FnP = 	kollapse(PackageDir, "/R/", fname)

# devtools::use_package("vioplot")
DESCRIPTION <- list("Title" = "Generate Scientific Figures and Reports Easily",
    "Authors@R" = person(given = "Abel", family = "Vertesy", email = "a.vertesy@hubrecht.eu", role = c("aut", "cre")),
    "Description" = "MarkdownReports is a set of R functions that allows you to generate precise figures easily, and create clean reports about what you just discovered with your analysis script. It helps you to:
    1. Create scientifically accurate figures and save them automatically as vector graphic (.pdf), that you can use from presentation to posters anywhere.
    2. Note down your findings easily in a clear and nicely formatted way, parsed from your variables into english sentences.
    3. Link & display your figures automatically inside your report, right there where they are needed.
    4. Version your findings, annotating which parameters were used to reach certain results.
    5. Share your report with others via email, Github or a personal website.", "License" = "GNU GPL 3",
		"Version"= "2.8", "Imports" = "vioplot, gplots, VennDiagram"
)

setwd(RepositoryDir)
if ( !dir.exists(PackageName) ) { create(PackageName, description = DESCRIPTION) }

# go and write fun's ------------------------------------------------------------------------
# file.edit(Package_FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(Package_FnP)

# replace output files ------------------------------------------------
BackupOldFile = 	kollapse(Package_FnP, ".bac", print = F)
AnnotatedFile = 	kollapse(Package_FnP, ".annot.R", print = F)
file.copy(from = Package_FnP, to = BackupOldFile, overwrite = T)
file.copy(from = AnnotatedFile, to = Package_FnP, overwrite = T)

# Manual editing of descriptors ------------------------------------------------
# file.edit(Package_FnP)

# Compile a package ------------------------------------------------
setwd(PackageName)
getwd()
document()


# Install your package ------------------------------------------------
setwd(RepositoryDir)
install(PackageName)
require("MarkdownReports")

# Test your package ------------------------------------------------
help("wplot")

# Test if you can install from github ------------------------------------------------
devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
require("MarkdownReports")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("MarkdownReports")






