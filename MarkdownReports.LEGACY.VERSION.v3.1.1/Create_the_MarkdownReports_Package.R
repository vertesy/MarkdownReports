######################################################################################################
# Create_the_MarkdownReports_Package.R
######################################################################################################
# source("~/MarkdownReports/Create_the_MarkdownReports_Package.R")
# rm(list=ls(all.names = TRUE));
try.dev.off()

# install.packages("devtools")
# Functions ------------------------
# devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
 try (source ('~/GitHub/CodeAndRoll/CodeAndRoll.R'),silent= F)

irequire(devtools)
irequire(roxygen2)
irequire("stringr")


kollapse <-function (..., print = T) {
  if (print == T) {
    print(paste0(c(...), collapse = ""))
  }
  paste0(c(...), collapse = "")
}

# Setup ------------------------

RepositoryDir <- 	"~/GitHub/MarkdownReports/"
package.name <- 	"MarkdownReports"
package.version <- "3.1.1.0"
fname <-	paste0(package.name,".R")

PackageDir = paste0(RepositoryDir, package.name)
Package_FnP <-		paste0(PackageDir, "/R/", fname)

# devtools::use_package("vioplot")
DESCRIPTION <- list("Title" = "Generate Scientific Figures and Reports Easily",
    "Authors@R" = person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role = c("aut", "cre"))
    , "Description" = "MarkdownReports is a set of R functions that allows you to generate precise figures easily, and create clean markdown reports about what you just discovered with your analysis script. It helps you to:
    1. Create scientifically accurate figures and save them automatically as vector graphic (.pdf), that you can use from presentation to posters anywhere.
    2. Note down your findings easily in a clear and nicely formatted way, parsed from your variables into english sentences.
    3. Link & display your figures automatically inside your report, right there where they are needed.
    4. Version your findings, annotating which parameters were used to reach certain results.
    5. Share your report with others via email, Github or a personal website."
    , "License" = "GPL-3"
    , "Version"= package.version
    , "Imports" = "stats, methods, gplots, RColorBrewer, colorRamps, vioplot, VennDiagram"
    , "BugReports"= "https://github.com/vertesy/MarkdownReports/issues"
)

setwd(RepositoryDir)
if ( !dir.exists(package.name) ) { create(path = RepositoryDir, description = DESCRIPTION)
  } else { setup(path = PackageDir, description = DESCRIPTION, rstudio = F ) }

# go and write fun's ------------------------------------------------------------------------
# file.edit(Package_FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(Package_FnP)

# replace output files ------------------------------------------------
BackupOldFile <-	paste0(RepositoryDir, "Development", ".bac", print = F)
AnnotatedFile <-	paste0(RepositoryDir, "Development", ".annot.R", print = F)
file.copy(from = Package_FnP, to = BackupOldFile, overwrite = T)
file.copy(from = AnnotatedFile, to = Package_FnP, overwrite = T)

# Manual editing of descriptors ------------------------------------------------
# file.edit(Package_FnP)

# Compile a package ------------------------------------------------
setwd(RepositoryDir)
setwd(package.name)
getwd()
devtools::document()


# Install your package ------------------------------------------------
setwd(RepositoryDir)
install(package.name)
require("MarkdownReports")

{
  "update cff version"
  citpath <- paste0(RepositoryDir, 'CITATION.cff')
  xfun::gsub_file(file = citpath, perl = T
                  , "^version: v.+", paste0("version: v", package.version))
}


# Test your package ------------------------------------------------
help("wplot")

# Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
# require("MarkdownReports")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("MarkdownReports")



check(PackageDir)
as.package(PackageDir)


# source("https://install-github.me/r-lib/desc")
library(desc)
desc$set("MarkdownReports", "foo")
desc$get(MarkdownReports)
