######################################################################################################
# Create_the_MarkdownReports_Package.R
######################################################################################################
# source("~/GitHub/Packages/MarkdownReports/Development/Create_the_MarkdownReports_Package.R")
# rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)

# Functions ------------------------
RepositoryDir <- "~/GitHub/Packages/MarkdownReports/"

# Setup ------------------------
"TAKE A LOOK AT"
file.edit("~/GitHub/Packages/MarkdownReports/Development/config.R")


PackageTools::document_and_create_package(RepositoryDir, config_file = 'config.R')
'git add commit push to remote'


# Install your package ------------------------------------------------
"disable rprofile by"
rprofile()

devtools::install_local(RepositoryDir, upgrade = F)
# devtools::

# Test if you can install from github ------------------------------------------------
remote.path <- file.path(DESCRIPTION$'github.user', DESCRIPTION$'package.name')
pak::pkg_install(remote.path)
# unload(PackageTools)
# require("PackageTools")
# # remove.packages("PackageTools")

# CMD CHECK ------------------------------------------------
checkres <- devtools::check(RepositoryDir, cran = FALSE)



# Automated Codebase linting to tidyverse style ------------------------------------------------
styler::style_pkg(RepositoryDir)


# Extract package dependencies ------------------------------------------------
PackageTools::extract_package_dependencies(RepositoryDir)


# Visualize function dependencies within the package------------------------------------------------
{
  warning("works only on the installed version of the package!")
  pkgnet_result <- pkgnet::CreatePackageReport(DESCRIPTION$'package.name')
  fun_graph     <- pkgnet_result$FunctionReporter$pkg_graph$'igraph'

  PackageTools::convert_igraph_to_mermaid(graph = fun_graph, openMermaid = T, copy_to_clipboard = T)
}


# Try to find and add missing @importFrom statements------------------------------------------------
if (F) {
  (FNP <- list.files(file.path(RepositoryDir, "R"), full.names = T))
  for (Fx in FNP) {
    PackageTools::add_importFrom_statements(Fx, exclude_packages = "")
  }
}
