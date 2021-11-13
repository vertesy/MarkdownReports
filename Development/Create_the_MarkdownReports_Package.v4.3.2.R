######################################################################################################
# Workflow_to_Create_an_R_Package.R
# 3- Oct 2021
######################################################################################################
# source("~/GitHub/Packages/MarkdownReports/Development/Create_the_MarkdownReports_Package.v4.3.2.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)

# Functions ------------------------
# install_version("devtools", version = "2.0.2", repos = "http://cran.at.r-project.org") # install.packages("devtools")
require("devtools")
require("roxygen2")
require("stringr")

# devtools::install_github(repo = "vertesy/CodeAndRoll2")
require('CodeAndRoll2')
require('Stringendo')
# try (source('~/GitHub/Packages/CodeAndRoll/CodeAndRoll.R'),silent= FALSE) # ONLY If Stringendo not yet exist
# try (source('~/GitHub/Packages/Rocinante/R/Rocinante.R'),silent= FALSE) # ONLY If Stringendo not yet exist



# Setup ------------------------
PackageName = 	"MarkdownReports"
setwd("~/GitHub/")

RepositoryDir = kollapse("~/GitHub/Packages/", PackageName, "/")
fname = 	kollapse(PackageName, ".R")
Package_FnP = 	kollapse(RepositoryDir, "R/", fname)

BackupDir = "~/GitHub/Packages/MarkdownReports/Development/"
dir.create(BackupDir)

# devtools::use_package("vioplot")
DESCRIPTION <- list("Title" = "Generate Scientific Figures and Reports Easily"
    , "Author" = person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )'
    , "Description" = "MarkdownReports is a set of R functions that allows you to generate precise figures easily,
    and create clean markdown reports about what you just discovered with your analysis script. It helps you to:
    1. Create scientifically accurate (annotated) figures with very short code, making use of variable-, row- and columnnames.
    2. Save figures automatically as vector graphic (.pdf), that you can use from presentation to posters anywhere.
    3. Incorporate your figures automatically in a markdown report file.
    4. Describe your figures & findings in the same report in a clear and nicely formatted way, parsed from your variables into english sentences.
    5. Share your report, by exporting your report to .pdf, .html or .docx, or via Github or a personal website."
    , "License" = "GPL-3 + file LICENSE"
    , "Version" = "4.5.2"
    , "Packaged" =  Sys.time()
    , "Repository" =  "CRAN"
    , "Depends" =  "CodeAndRoll2, Stringendo, MarkdownHelpers"
    , "Imports" = "base, clipr, colorRamps, devtools, gplots, graphics, grDevices,  methods, ReadWriter, RColorBrewer, sessioninfo, sm, stats, utils, VennDiagram, vioplot"
    # , "Suggests" = ""
    , "BugReports"= "https://github.com/vertesy/MarkdownReports/issues"
)


setwd(RepositoryDir)
if ( !dir.exists(RepositoryDir) ) { create(path = RepositoryDir, description = DESCRIPTION, rstudio = TRUE)
} else {
    getwd()
    try(file.remove(c("DESCRIPTION","NAMESPACE"))) #, "MarkdownReports.Rproj"
    create_package(path = RepositoryDir, fields = DESCRIPTION, open = F)
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
devtools::document()


# Install your package ------------------------------------------------
# # setwd(RepositoryDir)
devtools::install(RepositoryDir, upgrade = F)
# require("MarkdownReports")
# # remove.packages("MarkdownReports")
# # Test your package ------------------------------------------------
# help("wplot")
# cat("\014")
# devtools::run_examples()


# Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/MarkdownReports")
# devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
# require("MarkdownReports")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("MarkdownReports")
#
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
# system("cd ~/GitHub/MarkdownReports/; ls -a; open .Rbuildignore")
#
# Check package dependencies ------------------------------------------------
depFile = paste0(RepositoryDir, 'Development/Dependencies.R')

(f.deps <- NCmisc::list.functions.in.file(filename = Package_FnP))
# clipr::write_clip(f.deps)

sink(file = depFile); print(f.deps); sink()
p.deps <- gsub(x = names(f.deps), pattern = 'package:', replacement = '')
write(x = p.deps, file = depFile, append = T)
p.dep.declared <- trimws(unlist(strsplit(DESCRIPTION$Imports, ",")))
p.dep.new <- sort(union( p.deps, p.dep.declared))
# clipr::write_clip(p.dep.new)
