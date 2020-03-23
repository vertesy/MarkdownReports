######################################################################################################
# Workflow_to_Create_an_R_Package.R
# 01 Jan 2018
######################################################################################################
# source("~/MarkdownReports/Workflow_to_Create_an_R_Package.R")
rm(list=ls(all.names = TRUE));
try.dev.off()
# install.packages("devtools")
# Functions ------------------------
# devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
try (source ('~/GitHub/CodeAndRoll/CodeAndRoll.R'),silent= FALSE)

irequire(devtools)
irequire(roxygen2)
irequire("stringr")

kollapse <-function (..., print = TRUE) {
  if (print == TRUE) {
    print(paste0(c(...), collapse = ""))
  }
  paste0(c(...), collapse = "")
}

# Setup ------------------------
PackageName = 	"MarkdownReportsDev"
setwd("~/GitHub/")

RepositoryDir = kollapse("~/GitHub/",PackageName,"/")
fname = 	kollapse(PackageName,".R")
Package_FnP = 	kollapse(RepositoryDir, "R/", fname)

BackupDir = "~/GitHub/MarkdownReports/Development2/"
dir.create(BackupDir)

# devtools::use_package("vioplot")
DESCRIPTION <- list("Title" = "Generate Scientific Figures and Reports Easily"
    , "Author" = person(given = "Abel", family = "Vertesy", email = "a.vertesy@hubrecht.eu", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "a.vertesy@hubrecht.eu", role =  c("aut", "cre") )'
    , "Description" = "MarkdownReports is a set of R functions that allows you to generate precise figures easily,
    and create clean markdown reports about what you just discovered with your analysis script. It helps you to:
    1. Create scientifically accurate (annotated) figures with very short code, making use of variable-, row- and columnnames.
    2. Save figures automatically as vector graphic (.pdf), that you can use from presentation to posters anywhere.
    3. Incorporate your figures automatically in a markdown report file.
    4. Describe your figures & findings in the same report in a clear and nicely formatted way, parsed from your variables into english sentences.
    5. Share your report, by exporting your report to .pdf, .html or .docx, or via Github or a personal website."
    , "License" = "GPL-3 + file LICENSE"
    , "Version"= "3.3.0.1"
    , "Packaged" =  Sys.time()
    , "Repository" =  "CRAN"
    , "Imports" = "stats, methods, sm, graphics, grDevices, gplots, RColorBrewer, colorRamps, clipr, vioplot, VennDiagram"
    # , "Suggests" = ""
    , "BugReports"= "https://github.com/vertesy/MarkdownReports/issues"
)

setwd(RepositoryDir)
if ( !dir.exists(RepositoryDir) ) { create(path = RepositoryDir, description = DESCRIPTIO, rstudio = FALSE)
} else {
    getwd()
    file.remove(c("DESCRIPTION","NAMESPACE"))
    setup(path = RepositoryDir, description = DESCRIPTION, rstudio = FALSE )
  }

# go and write fun's ------------------------------------------------------------------------
# file.edit(Package_FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(Package_FnP)

# replace output files ------------------------------------------------
BackupOldFile = 	kollapse(BackupDir, "Development", ".bac", print = FALSE)
AnnotatedFile = 	kollapse(BackupDir, "Development", ".annot.R", print = FALSE)
file.copy(from = Package_FnP, to = BackupOldFile, overwrite = TRUE)
# file.copy(from = AnnotatedFile, to = Package_FnP, overwrite = TRUE)

# Manual editing of descriptors ------------------------------------------------
# file.edit(Package_FnP)

# Compile a package ------------------------------------------------
setwd(RepositoryDir)
getwd()
document()


# Install your package ------------------------------------------------
# # setwd(RepositoryDir)
# install(RepositoryDir)
# require("MarkdownReportsDev")
# # remove.packages("MarkdownReports")
# # Test your package ------------------------------------------------
# help("wplot")
cat("\014")
# devtools::run_examples()


# Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
# require("MarkdownReports")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("MarkdownReports")


check(RepositoryDir, cran = TRUE)
# as.package(RepositoryDir)
#
#
# # source("https://install-github.me/r-lib/desc")
# # library(desc)
# # desc$set("MarkdownReports", "foo")
# # desc$get(MarkdownReports)
#
#
# system("cd ~/GitHub/MarkdownReportsDev/; ls -a; open .Rbuildignore")
#
