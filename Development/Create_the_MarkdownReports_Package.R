######################################################################################################
# Workflow_to_Create_an_R_Package.R
######################################################################################################
# source("/Users/abelvertesy/MarkdownReports/Workflow_to_Create_an_R_Package.R")
rm(list=ls(all.names = TRUE));
try(dev.off())

# Functions ------------------------
# devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
try (source ('/Users/abelvertesy/TheCorvinas/R/CodeAndRoll.R'),silent= F)
require(devtools)
require(roxygen2)
require("stringr")

# Setup ------------------------

RepositoryDir = 	"/Users/abelvertesy/MarkdownReports/"
PackageName = 	"MarkdownReports"
fname = 	kollapse(PackageName,".R")

PackageDir = kollapse(RepositoryDir, PackageName)
Package_FnP = 	kollapse(PackageDir, "/R/", fname)
DESCRIPTION <- list("Title" = "Generate Scientific Figures and Reports Easily",
					"Authors@R" = person(given = "Abel", family = "Vertesy", email = "a.vertesy@hubrecht.eu", role = c("aut", "cre")),
					"Description" = "An R function library to create scientific figures and reports easily.",
					"License" = "GNU GPL 3"
)

setwd(RepositoryDir)
if ( !dir.exists(PackageName) ) { create(PackageName, description = DESCRIPTION) }

# go and write fun's ------------------------------------------------------------------------
file.edit(Package_FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(Package_FnP)

# replace output files ------------------------------------------------
BackupOldFile = 	kollapse(Package_FnP, ".bac", print = F)
AnnotatedFile = 	kollapse(Package_FnP, ".annot.R", print = F)
file.copy(from = Package_FnP, to = BackupOldFile, overwrite = T)
file.copy(from = AnnotatedFile, to = Package_FnP, overwrite = T)

# Manual editing of descriptors ------------------------------------------------
file.edit(Package_FnP)

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
# installed.packages()
remove.packages("MarkdownReports")






