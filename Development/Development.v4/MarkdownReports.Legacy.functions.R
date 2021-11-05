## MarkdownReports.R   ------------------------------
# author: Abel Vertesy
# date: Oct 30 2021
# source("~/Github/Packages/MarkdownReports/R/MarkdownReports.Legacy.functions.R")

# Legacy functions ---------------------------------------------------------------------------------

#' setup_logging_markdown (deprecated, use with create_set_OutDir, will be removed from V3)
#'
#' Setup the markdown report file, create a sub directory in "OutDir". Its name is stamped with
#' the script name and the modification time. Create the "path_of_report" variable used by
#' all log-writing and ~wplot functions.
#' @param fname Name of the report file.
#' @param title Title of the report.
#' @param append Set append to TRUE if you do not want to overwrite the previous report.
#' Use continue_logging_markdown() if you return logging into an existing report.
#' @param b.png4Github A global variable, defined by this and used by the other functions.
#' If TRUE (default), any link to the .png versions of images will be created in a
#' GitHub compatible format. That means, when you upload your markdown report and the .png images
#'  to your GitHub wiki under "Reports/" the links will correctly display the images online.
#' @export
#' @examples setup_logging_markdown (fname = "Analysis.md", title = "My Analysis",
#' append = TRUE, b.png4Github = TRUE)

setup_logging_markdown <-
  function(fname,
           title = "",
           append = TRUE,
           b.png4Github = TRUE) {
    OutDir = ww.set.OutDir()

    path_of_report <- kollapse(OutDir, fname, ".log.md")

    if (nchar(title)) {
      write(paste("# ", title), path_of_report, append = append)
    } else {
      write(paste("# ", fname, "Report"), path_of_report, append = append)
    }
    write(kollapse("    Modified: ", format(Sys.time(), "%d/%m/%Y | %H:%M | by: "), fname),
          path_of_report,
          append = TRUE)
    BackupDir = kollapse( OutDir, "/", substr(fname, 1, nchar(fname)), "_",
                          format(Sys.time(), "%Y_%m_%d-%Hh"), print = FALSE
    )
    if (!exists(BackupDir)) {
      dir.create(BackupDir, showWarnings = FALSE)
      ww.assign_to_global("BackupDir", BackupDir, 1)
    }
    ww.assign_to_global("path_of_report", path_of_report, 1)
    ww.assign_to_global("b.png4Github", b.png4Github, 1)
  }

#' log_settings_MarkDown (Legacy)
#'
#' Log the parameters & settings used in the script in a table format.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples a = 1; b = 2; log_settings_MarkDown (a,b)

log_settings_MarkDown <- function(...) {
  print("Use md.LogSettingsFromList() for a list of parameters")
  call <- match.call()
  namez = sapply(as.list(call[-1]), deparse)
  value = c(...)
  value = as.data.frame(value)
  rownames(value) = namez
  md.tableWriter.DF.w.dimnames(value, title_of_table = "Settings")
}


