## MarkdownReports.R   ------------------------------
# author: Abel Vertesy
# date: Oct 30 2021
# source("~/Github/Packages/MarkdownReports/R/MarkdownReports.R")

utils::globalVariables(c('OutDirOrig', 'OutDir', 'ParentDir', 'path_of_report', 'plotnameLastPlot',
                         'b.scriptname', 'b.usepng', 'b.png4Github', 'b.mfrow_def',
                         'b.bg_def', 'b.Subdirname', 'b.report.not.found', 'b.def.color'))

# Table of Contents ------------------------------------
# - Setup
# - Plots
# - Plots for cycling over data frame columns or rows
# - A4 pdfs for multi-plots
# - Add-ons to exisiting plots
# - Graphics and Internal function


# ______________________________________________________________________________________________----
# Setup ----
# _________________________________________________________________________________________________


#' @title setup_MarkdownReports
#'
#' @description Setup the markdown report file and the output directory, create a sub directory in "OutDir".
#' Its name is stamped with the script name and the modification time. Create the "path_of_report"
#' variable used by all log-writing and ~wplot functions.
#'
#' @param OutDir The output directory (absolute / full path).
#' @param title Manually set the title of the report.
#' @param append Set append to TRUE if you do not want to overwrite the previous report.
#' @param backupfolder Create a time-stamped backup folder inside the working directory (OutDir).
#' @param recursive.folder Create output folder recursively, if parent folders do not exist. Parameter for dir.create().
#' Use continue_logging_markdown() if you return logging into an existing report.
#' FALSE by default: rerunning the script overwrites the previous report. Archive reports manually
#' into the timestamped subfolder within the OutDir.
#' @param b.defSize Default width of plot EXCEPT in pdfA4plot_on(), assuming h = w by default.
#' c("def" = 7, "A4" = 8.27, "1col.nature" = 3.50, "2col.nature" = 7.20, "1col.cell" = 3.35,
#' "1.5col.cell" = 4.49, "2col.cell" = 6.85)
#' @param b.defSize.fullpage Default width of plot in pdfA4plot_on()A global background variable
#' used by pdfA4plot_on.
#' @param b.usepng A global background variable used by the plotting functions. If TRUE, a link to
#' the .png versions of the saved plot will be created. The .png file itself is not created.
#' @param b.png4Github A global background variable used by the plotting functions.
#' If TRUE (default), the link to the .png versions of the saved plot will be created in a
#' GitHub compatible format.  That means, when you upload your markdown report and the .png
#' images to your GitHub wiki under "Reports/" the links will correctly display the images online.
#' @param b.mdlink A global background variable used by the plotting functions. If TRUE (default),
#' all saved (.pdf) plots will be linked into your report.
#' @param b.save.wplots A global background variable used by the plotting functions.
#' If TRUE (default), plots will be saved to a .pdf file.
#' @param addTableOfContents write 'TOC' below the header of the file, This is compiled to a
#' proper Table Of Contents by, e.g. Typora.
#' @param scriptname Name of the script file you are running.
#' This filename is written in the title field of .pdf files,
#' so that you know which script generated that file.
#' Example: "GeneFilt.hist by MyFilteringScript".
#' @param b.def.color Set the default color for all wplot* functions.
#' @param setDir Set the working directory to OutDir? Default: TRUE
#' @param saveSessionInfo save 'sessioninfo::session_info()' results to '.session_info.DATE.txt.gz'
#' @param saveParameterList save the list of parameters stored in the variable name provides ("p" by default) as a table in the markdown report. Uses the md.LogSettingsFromList() function. Set to FALSE to disable this option.
#' @export
#' @import sessioninfo vioplot
#' @examples setup_MarkdownReports( scriptname = "MyRscript.R", title = "Awesome Ananlysis",
#' append = TRUE, b.png4Github = TRUE)

setup_MarkdownReports <-
  function(OutDir = getwd(),
           scriptname = basename(OutDir),
           title = "",
           setDir = TRUE,
           recursive.folder = TRUE,
           backupfolder = TRUE,
           append = FALSE,
           addTableOfContents = FALSE,
           saveSessionInfo = TRUE,
           saveParameterList = "p",
           b.defSize = c(
             "def" = 7,
             "A4" = 8.27,
             "1col.nature" = 3.50,
             "2col.nature" = 7.20,
             "1col.cell" = 3.35,
             "1.5col.cell" = 4.49,
             "2col.cell" = 6.85
           )[1],
           b.defSize.fullpage = 8.27,
           b.usepng = FALSE,
           b.png4Github = FALSE,
           b.mdlink = TRUE,
           b.save.wplots = TRUE,
           b.def.color = "gold1") {
    if (!exists(OutDir)) {
      dir.create(OutDir, showWarnings = FALSE, recursive = recursive.folder)
    }
    OutDir = AddTrailingSlash(OutDir) # add '/' if necessary
    OutDir = RemoveDoubleSlash(OutDir)

    ww.assign_to_global("OutDir", OutDir, 1)
    Stringendo::iprint("All files will be saved under 'OutDir': ", OutDir)
    path_of_report <- paste0(OutDir, scriptname, ".log.md")
    ww.assign_to_global("path_of_report", path_of_report, 1)
    Stringendo::iprint("MarkdownReport location is stored in 'path_of_report': ",
           path_of_report)

    if (nchar(title)) {
      write(paste("# ", title), path_of_report, append = append)
    } else {
      write(paste("# ", scriptname, "Report"), path_of_report, append = append)
    }
    write(paste0(
      "   Modified: ",
      format(Sys.time(), "%d/%m/%Y | %H:%M | by: "),
      scriptname
    ),
    path_of_report,
    append = TRUE)

    if (addTableOfContents)
      write('[TOC]', path_of_report, append = TRUE)
    BackupDir = kollapse(
      OutDir,
      "/",
      substr(scriptname, 1, nchar(scriptname)),
      "_",
      format(Sys.time(), "%Y_%m_%d-%Hh"),
      print = FALSE
    )
    if (setDir) {
      setwd(OutDir)
    }
    if (saveSessionInfo) {
      defWidth = options("width")$width
      options("width"= 200)
      # sink(file = paste0(".sessionInfo.", format(Sys.time(), format ="%Y.%m.%d" ),".txt"), type = "output")
      # sessioninfo::session_info()
      # sink()
      writeLines(
        utils::capture.output(
          sessioninfo::session_info()
        ),con = paste0(".sessionInfo.", format(Sys.time(), format ="%Y.%m.%d" ),".txt")
      )

      options("width"= defWidth)
      rm(defWidth)
      llprint(".sessionInfo* is saved in the working directory (OutDir).")
    }
    if (!exists(BackupDir) & backupfolder) {
      dir.create(BackupDir, showWarnings = FALSE)
      ww.assign_to_global("BackupDir", BackupDir, 1)
    }
    saveParameterList
    if (saveParameterList != FALSE) {
      if (exists(saveParameterList)) {
        md.LogSettingsFromList(saveParameterList)
      } else { Stringendo::iprint ("No parameter list is defined in variable: ", saveParameterList,
                       ". It has to be a list of key:value pairs like: p$thr=10")}
    }
    ww.assign_to_global("b.defSize", b.defSize, 1)
    ww.assign_to_global("b.defSize.fullpage", b.defSize.fullpage, 1)
    ww.assign_to_global("b.mdlink", b.mdlink, 1)
    ww.assign_to_global("b.save.wplots", b.save.wplots, 1)
    ww.assign_to_global("b.usepng", b.usepng, 1)
    ww.assign_to_global("b.png4Github", b.png4Github, 1)
    ww.assign_to_global("b.scriptname", scriptname, 1)
    ww.assign_to_global("b.def.color", b.def.color, 1)
    ww.assign_to_global("b.report.not.found",
                        "Path to the Markdown report file is not defined in path_of_report", 1)
  }

# create_set_SubDir
#'
#' Create or set the output directory of the script, and set the "NewOutDir" variable that is
#' used by all ~wplot functions. Opening pair of the create_set_Original_OutDir function.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @param ParentDir Change the "OutDirOrig" variable to the
#' current OutDir (before setting it to a subdir).
#' @param define.ParentDir Report on what was the parent directory of the new subdir.
#' @param setDir Change working directory to the newly defined subdirectory
#' @param verbose Print directory to screen? Default: TRUE
#' @export
#' @examples create_set_SubDir (makeOutDirOrig = TRUE, setDir = TRUE, "MySubFolder")

create_set_SubDir <-
  function(..., define.ParentDir = TRUE,
           setDir = TRUE,
           verbose = TRUE) {
    b.Subdirname = kollapse(...)
    OutDir = ww.set.OutDir()

    NewOutDir = kollapse(OutDir, ..., print = FALSE)

    NewOutDir = AddTrailingSlash(NewOutDir) # add '/' if necessary
    NewOutDir = RemoveDoubleSlash(NewOutDir)
    if (verbose) Stringendo::iprint("All files will be saved under 'NewOutDir': ", NewOutDir)
    if (!dir.exists(NewOutDir)) {
      dir.create(NewOutDir, showWarnings = FALSE)
    }
    if (setDir) {
      setwd(NewOutDir)
    }
    if (define.ParentDir) {
      if (exists("ParentDir")) # If this function has been run already, you have "ParentDir", which will be overwritten.
        if (verbose) Stringendo::iprint("ParentDir was defined as:", ParentDir)
      if (verbose) Stringendo::iprint("ParentDir will be:", OutDir)
      ww.assign_to_global("ParentDir", OutDir, 1)
    } #if
    if (verbose) Stringendo::iprint("Call *create_set_Original_OutDir()* when chaning back to the main dir.")
    ww.assign_to_global("OutDir", NewOutDir, 1)
    ww.assign_to_global("b.Subdirname", b.Subdirname, 1)
    # Flag that md.image.linker uses
  }

# create_set_Original_OutDir
#'
#' Closing pair of the create_set_SubDir function. Call when chaning back to the main dir.
#' Set the output directory of the script, and set the "NewOutDir" variable that is
#'  used by all ~wplot functions.
#'
#' @param NewOutDir The new OutDir
#' @param b.Subdirname The current (sub) working directory
#' @param setDir Change working directory to the newly defined subdirectory.
#' @param verbose Print directory to screen? Default: TRUE
#' @export
#' @examples create_set_Original_OutDir (getwd(),"/")

create_set_Original_OutDir <-
  function(NewOutDir = OutDirOrig,
           b.Subdirname = FALSE,
           setDir = TRUE,
           verbose = TRUE) {
    if (verbose) Stringendo::iprint("All files will be saved under the original OutDir: ", NewOutDir)
    if (!exists(NewOutDir)) {
      dir.create(NewOutDir, showWarnings = FALSE)
    }
    if (setDir) {
      setwd(NewOutDir)
    }
    ww.assign_to_global("OutDir", NewOutDir, 1)
    ww.assign_to_global("b.Subdirname", b.Subdirname, 1)
  }


#' continue_logging_markdown
#'
#' Continue writing to an existing report file.
#' @param b.scriptname Name of the report file.
#' @export
#' @examples OutDir = paste0(getwd(),"/", collapse = "")
#' continue_logging_markdown (b.scriptname = "Analysis")

continue_logging_markdown <- function(b.scriptname) {
  path = ww.set.OutDir()
  path_of_report <-
    kollapse(path, b.scriptname, ".log.md", print = FALSE)
  Stringendo::iprint("Writing report in:", path_of_report)
  ww.assign_to_global("path_of_report", path_of_report, 1)

  BackupDir = kollapse(path,
                       "/",
                       substr(b.scriptname, 1, (nchar(b.scriptname) - 2)),
                       format(Sys.time(), "%Y_%m_%d-%Hh"),
                       print = FALSE)
  if (!exists(BackupDir)) {
    dir.create(BackupDir, showWarnings = FALSE)
    ww.assign_to_global("BackupDir", BackupDir, 1)
  }
}

#' create_set_OutDir
#'
#' Create or set the output directory of the script, and set the "OutDir" variable that is used by
#' all ~wplot functions.
#'
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @param setDir Set the working directory to OutDir? Default: TRUE
#' @param verbose Print directory to screen? Default: TRUE
#'
#' @export
#' @examples create_set_OutDir (setDir = TRUE, getwd(),"/"   )

create_set_OutDir <- function(..., setDir = TRUE, verbose = TRUE) {
  OutDir = kollapse(..., print = FALSE)
  OutDir = AddTrailingSlash(OutDir) # add '/' if necessary
  OutDir = RemoveDoubleSlash(OutDir)
  if (verbose) Stringendo::iprint("All files will be saved under 'OutDir': ", OutDir)
  if (!exists(OutDir)) {
    dir.create(OutDir, showWarnings = FALSE)
  }
  if (setDir) {
    setwd(OutDir)
  }
  ww.assign_to_global("OutDir", OutDir, 1)
}



# ______________________________________________________________________________________________----
# Plots ----
# _________________________________________________________________________________________________


#' @title wplot_save_this
#'
#' @description Save the currently active graphic device (for complicated plots).  Insert links to your markdown
#' report, set by "path_of_report". Name the file by naming the variable!
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param OverwritePrevPDF Overwrite previous PDF image (as name stored in plotnameLastPlot).
#' If FALSE, it creates a name from the date.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should
#'   work).
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by
#'   "path_of_report".
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#' @export
#' @examples wplot_save_this (plotname = date(), col = "gold1", w = 7
#' , mdlink = FALSE, ManualName = FALSE)

wplot_save_this <-
  function(plotname = ww.autoPlotName(),
           ...,
           OverwritePrevPDF = TRUE,
           w = unless.specified("b.defSize", 7),
           h = w,
           mdlink = FALSE,
           PNG = unless.specified("b.usepng", F)) {
    if (!OverwritePrevPDF) {plotname = make.names(date())}

    ww.dev.copy(
      PNG_ = PNG,
      fname_ = plotname,
      w_ = w,
      h_ = h
    )

    if (mdlink) {
      md.image.linker(fname_wo_ext = plotname)
    }
  }


#' wplot_save_pheatmap
#'
#' Save pheatmap object. Modified from:
#' https://stackoverflow.com/questions/43051525/how-to-draw-pheatmap-plot-to-screen-and-also-save-to-file
#' @param x The pheatmap object to save.
#' @param suffix Suffix to File name.
#' @param filename File name (saved as .pdf, inside working directory).
#' @param width width of the plot in inches.
#' @param height height of the plot in inches.
#' @export
#'
#' @examples test = matrix(rnorm(200), 20, 10);
#' colnames(test) = paste("Test", 1:10, sep = "");
#' rownames(test) = paste("Gene", 1:20, sep = "");
#' ph.test <- pheatmap::pheatmap(test);
#' wplot_save_pheatmap(ph.test)

wplot_save_pheatmap <-
  function(x,
           suffix = NULL,
           filename = kpp(substitute(x), suffix),
           width = 15,
           height = width) {
    stopifnot(!missing(x))
    filename <- ppp(filename, ".heatmap.pdf")
    pdf(file = filename,
        width = width,
        height = height)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
    print(kpps(getwd(), filename))
  }


#' wplot
#'
#' Create and save scatter plots as .pdf, in "OutDir". If mdlink = TRUE, it inserts a .pdf and a .png
#' link in the markdown report, set by "path_of_report". The .png version is not created, only the
#' link is put in place, not to overwrite previous versions. The .png version is not created, only
#' the link is put in place. You can add 2D error bars around the dots, or add lines (ablines) to
#' your plot, by setting "abline" argument to = FALSE (no line, default), "h" (horizontal, further
#' specified by a = y-offset), "v" (vertical, further specified by a = x-offset), "ab" (line with an
#' angle, further specified by a = offset, b = slope).
#'
#' @param df2col Input data frame to be plotted_2columns
#' @param col Color of the plot.
#' @param pch Define the symbol for each data point. A number (0-25) or any string between ""-s.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should
#'   work).
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param ylim manual Y-limits error bar
#' @param xlim manual X-limits error bar
#' @param errorbar Draw error bars if TRUE. Pass on the value in parameters "upper" and "lower".
#'   Refine the look by "w" and "arrow_lwd".
#' @param upper Size of the upper error bar.
#' @param lower Size of the lower error bar. By default, it equals the upper error bar.
#' @param left Size of the left error bar.
#' @param right Size of the right error bar. By default, it equals the left error bar.
#' @param arrow_lwd Line width for the error bar arrow. Line width for the error bar arrow.
#' @param col_errorbar Color of the error bar arrow.
#' @param abline Draw a line on the plot. Any value from: c( 'v', 'h', 'ab') for vertical,
#'   horizontal, and line with any slope. In each case you need to specify "a = ", specifying the
#'   X-position for vertical (v); the Y-position for horizontal (yh) lines and the intercept for
#'   lines with a slope (ab). In the latter case, 'b' specifes the slope.
#' @param a X-offset for vertical lines, Y-offset for horizontal, and inclined lines.
#' @param b Slope of an inclined line.
#' @param lty Linetype, defined by numbers 1-6.
#' @param lwd Line width. Set to 2 by default.
#' @param col_abline Color of the line.
#' @param equal.axes Span of axes is set to equal (maximum range in either X or Y).
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by
#'   "path_of_report".
#' @param panel_first Draw a backround grid, if set to "grid(NULL)"
#' @param width.whisker Width of the error bar whisker.
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#'
#' @export
#' @examples try.dev.off(); mydf = cbind("A" = rnorm(100), "B" = rpois(100, 8))
#' wplot (df2col = mydf, col = 1, pch = 18, w = 7,
#' mdlink = FALSE, errorbar = FALSE, upper = 0,
#' left = 0, right = left, width.whisker = 0.1, arrow_lwd = 1, abline = FALSE,
#' a = FALSE, b = FALSE, lty = 1, lwd = 1, col_abline = 1)

wplot <-
  function(df2col,
           col = 1,
           pch = 18,
           ...,
           panel_first = grid(NULL),
           plotname = substitute(df2col),
           errorbar = FALSE,
           upper = 0,
           lower = upper,
           left = 0,
           right = left,
           width.whisker = 0.1,
           arrow_lwd = 1,
           col_errorbar = 1,
           ylim = FALSE,
           xlim = FALSE,
           abline = c(FALSE, 'v', 'h', 'ab')[1],
           a = FALSE,
           b = FALSE,
           lty = 1,
           lwd = 1,
           col_abline = 1,
           equal.axes = FALSE,
           savefile = unless.specified("b.save.wplots"),
           mdlink = ww.set.mdlink(),
           w = unless.specified("b.defSize", 7),
           h = w,
           PNG = unless.specified("b.usepng", F)) {
    x = df2col[, 1]
    y = df2col[, 2]
    fname = kollapse(plotname, ".plot")
    if (errorbar) {
      ylim_ = range(c((y + upper + abs(0.1 * y)), (y - lower - abs(0.1 * y))), na.rm = TRUE)
      xlim_ = range(c((x + right + abs(0.1 * x)), (1.1 * x - left - abs(0.1 * x))), na.rm = TRUE)
    }
    else {
      ylim_ = range(y, na.rm = TRUE)
      xlim_ = range(x, na.rm = TRUE)
    }
    if (equal.axes)
      xlim_ = ylim_ = range(c(xlim_, ylim_))
    if (is.numeric(ylim) & length(ylim) == 2) {
      ylim_ = ylim
    } #overwrite if
    if (is.numeric(xlim) & length(xlim) == 2) {
      xlim_ = xlim
    }

    plot(
      df2col,
      ...,
      main = plotname,
      col = col,
      pch = pch,
      ylim = ylim_,
      xlim = xlim_,
      panel.first = panel_first
    )
    if (errorbar) {
      arrows(
        x0 = x,
        y0 = y + upper,
        x1 = x,
        y1 = y - lower,
        angle = 90,
        code = 3,
        length = width.whisker,
        lwd = arrow_lwd,
        col = col_errorbar
      )
      arrows(
        x0 = x + left,
        y0 = y,
        x1 = x - right,
        y1 = y,
        angle = 90,
        code = 3,
        length = width.whisker,
        lwd = arrow_lwd,
        col = col_errorbar
      )
    }
    if (abline == "h") {
      abline(
        h = a,
        lty = lty,
        lwd = lwd,
        col = col_abline
      )
    }
    if (abline == "v") {
      abline(
        v = a,
        lty = lty,
        lwd = lwd,
        col = col_abline
      )
    }
    if (abline == "ab") {
      abline(
        a = a,
        b = b,
        lty = lty,
        lwd = lwd,
        col = col_abline
      )
    }
    ww.assign_to_global("plotnameLastPlot", fname, 1)
    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
    if (mdlink & savefile) {
      md.image.linker(fname_wo_ext = fname)
    }
  }



#' wscatter.fill
#'
#' A scatterplot with color gradient and color legend. Modified from:
#' http://stackoverflow.com/questions/20127282/r-color-scatterplot-points-by-col-value-with-legend
#'
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should
#'   work).
#' @param xlab X axis label
#' @param ylab Y axis label
#' @param color Filling color of the symbols
#' @param xlim Manually set the range of canvas in X dimension
#' @param zlim  Manually set the range of colors numbers (Z dimension)
#' @param nlevels Number of steps in the color gradient
#' @param pch Define the symbol for each data point. A number (0-25) or any string between ""-s.
#' @param cex Size of the symbols
#' @param plotname The name of the file saved.
#' @param plot.title The title of the plot.
#' @param axes  Draw axes and box
#' @param plot.axes Draw axis ticks
#' @param key.title ...
#' @param key.axes ...
#' @param asp numeric, giving the aspect ratio y/x. See help('plot.window').
#' @param xaxs The style of axis interval calculation to be used for the X-axis. See help('par').
#' @param yaxs The style of axis interval calculation to be used for the X-axis. See help('par').
#' @param las numeric in {0, 1, 2, 3}; the style of axis labels. See help('par').
#' @param frame.plot No description.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not
#'   fit on the plot.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by
#'   "path_of_report".
#' @param df2col Input data, a 2 column dataframe
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#' @param ylim Defines the Y axis range. Replacement for the standard "ylim" argument.
#'
#' @export
#' @import stats
#' @examples try.dev.off(); mydf = cbind("A" = rnorm(100), "B" = rnorm(100))
#' wscatter.fill( df2col = mydf, color = rnorm(100), nlevels = 15, pch = 21,
#' xlab = "The X Dimension. Wooaaahh")



wscatter.fill <-
  function(df2col = cbind("A" = rnorm(100), "B" = rnorm(100)),
           ...,
           color,
           xlim = range(df2col[, 1]),
           ylim = range(df2col[, 2]),
           zlim = range(color),
           nlevels = 20,
           pch = 21,
           cex = 1,
           plotname = substitute(df2col),
           plot.title = plotname,
           plot.axes,
           key.title,
           key.axes,
           asp = NA,
           xaxs = "i",
           yaxs = "i",
           las = 1,
           axes = TRUE,
           frame.plot = axes,
           xlab,
           ylab,
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           incrBottMarginBy = 0,
           mdlink = ww.set.mdlink(),
           PNG = unless.specified("b.usepng", F)) {
    x = df2col[, 1]
    y = df2col[, 2]
    CNN = colnames(df2col)
    xlab = if (length(CNN) & missing(xlab))
      CNN[1]
    ylab = if (length(CNN) & missing(ylab))
      CNN[2]

    fname = kollapse(plotname, ".barplot")
    if (incrBottMarginBy) {
      .ParMarDefault <- par("mar")
      par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]))
    }   # Tune the margin

    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    WID <- (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(WID)))
    par(las = las)
    mar <- mar.orig
    mar[4L] <- mar[2L]
    mar[2L] <- 1
    par(mar = mar)

    # choose colors to interpolate
    levels <- seq(zlim[1], zlim[2], length.out = nlevels)
    col <- colorRampPalette(c("red", "yellow", "dark green"))(nlevels)
    colz <- col[cut(color, nlevels)]

    plot.new()
    plot.window(
      xlim = c(0, 1),
      ylim = range(levels),
      xaxs = "i",
      yaxs = "i"
    )

    rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border = col)
    if (missing(key.axes)) {
      if (axes) {
        axis(4)
      }
    }
    else
      key.axes
    box()
    if (!missing(key.title))
      key.title
    mar <- mar.orig
    mar[4L] <- 1
    par(mar = mar)

    # points
    xlb <- xlab # to avoid circular reference in the inside function argument
    ylb <- ylab
    plot(
      x,
      y,
      main = plot.title,
      type = "n",
      xaxt = 'n',
      yaxt = 'n',
      ...,
      xlim = xlim,
      ylim = ylim,
      bty = "n",
      xlab = xlb,
      ylab = ylb
    )
    points(
      x,
      y,
      bg = colz,
      xaxt = 'n',
      yaxt = 'n',
      xlab = "",
      ylab = "",
      bty = "n",
      pch = pch,
      ...
    )

    ## options to make mapping more customizable
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "",
              xlab = "",
              ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    } else {plot.axes}
    if (frame.plot) {box()}
    if (missing(plot.title)) { title(...) } else { plot.title }
    invisible()

    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
    if (incrBottMarginBy) {
      par("mar" = .ParMarDefault)
    }
    ww.assign_to_global("plotnameLastPlot", fname, 1)
    if (mdlink & savefile) {
      md.image.linker(fname_wo_ext = fname)
    }
  }


#' wbarplot
#'
#' Create and save bar plots as .pdf, in "OutDir". If mdlink = TRUE, it inserts a .pdf and a .png
#' link in the markdown report, set by "path_of_report". The .png version is not created, only the
#' link is put in place, not to overwrite previous versions.
#' @param variable The variable to plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should
#'   work).
#' @param col Color of the plot.
#' @param sub Subtitle below the plot.
#' @param plotname The name of the file saved.
#' @param main The title of the plot.
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param ylim Defines the Y axis range. Replacement for the standard "ylim" argument.
#' @param hline Draw a horizontal line at the value you pass on to it. Useful to display a
#'   threshold. Design the line by "lty", "lwd" & "lcol" parameters.
#' @param vline Draw a vertical line at the value you pass on to it. Useful to display a threshold.
#'   Design the line by "lty", "lwd" & "lcol" parameters.
#' @param filtercol Color bars below / above the threshold with red / green. Define the direction by
#'   -1 or 1. Takes effect if "hline" is defined.
#' @param lty Linetype, defined by numbers 1-6.
#' @param lwd Linewidth. Set to 2 by default.
#' @param lcol Color of the line.
#' @param errorbar Draw error bars if TRUE. Pass on the value in parameters "upper" and "lower".
#'   Refine the look by "w" and "arrow_lwd".
#' @param upper Size of the upper error bar.
#' @param lower Size of the lower error bar. By default, it equals the upper error bar.
#' @param arrow_width Width of the arrow head.
#' @param arrow_lwd Line width for the error bars.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not
#'   fit on the plot.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by
#'   "path_of_report".
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#' @export
#' @examples MyVec = 1:3; wbarplot (variable = MyVec, col = "gold1", sub = FALSE, w = 7, width = 1,
#' incrBottMarginBy = 0, mdlink = FALSE, tilted_text = FALSE, hline = FALSE, vline = FALSE,
#' filtercol = 1, lty = 1, lwd = 2, lcol = 2, errorbar = FALSE, upper = 0,
#' arrow_width = 0.1, arrow_lwd = 1)

wbarplot <-
  function(variable,
           ...,
           col = unless.specified("b.def.colors", "gold1"),
           sub = FALSE,
           plotname = substitute(variable),
           main = plotname,
           tilted_text = FALSE,
           ylim = NULL,
           hline = FALSE,
           vline = FALSE,
           filtercol = 1,
           lty = 1,
           lwd = 2,
           lcol = 2,
           errorbar = FALSE,
           upper = 0,
           lower = upper,
           arrow_width = 0.1,
           arrow_lwd = 1,
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           incrBottMarginBy = 0,
           mdlink = ww.set.mdlink(),
           PNG = unless.specified("b.usepng", F)) {
    isVec = is.vector(variable) | is.table(variable)
    isMat = is.matrix(variable) | is.data.frame(variable)

    NrBars = if (isVec)
      length(variable)
    else if (isMat)
      ncol(variable)
    else
      length(variable)

    BarNames = if (isVec)
      names(variable)
    else if (isMat)
      colnames(variable)
    else
      names(variable)

    fname = kollapse(plotname, ".barplot")
    if (incrBottMarginBy) {
      .ParMarDefault <- par("mar")
      par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]))
    }   # Tune the margin
    cexNsize = 0.8 / abs(log10(length(variable)))
    cexNsize = min(cexNsize, 1)
    if (sub == TRUE) {
      subtitle = paste("mean:", iround(mean(variable, na.rm = TRUE)),
                       "CV:", percentage_formatter(cv(variable)))
    } else if (sub == FALSE) {
      subtitle = ""
    } else {
      subtitle = sub
    }
    if (hline & filtercol == 1) {
      col = (variable >= hline) + 2
    }
    if (hline & filtercol == -1) {
      col = (variable <  hline) + 2
    }
    if (errorbar & is.null(ylim)) {
      ylim = range(c(
        0,
        (variable + upper + abs(0.1 * variable)),
        variable - lower - abs(0.1 * variable)
      ), na.rm = TRUE)
    } # else {  ylim = range(0, variable) }
    if (tilted_text) {
      xlb = rep(NA, NrBars)
    } else {
      xlb = BarNames
    }

    x = barplot(
      variable,
      ylim = ylim,
      ...,
      names.arg = xlb,
      main = main,
      sub = subtitle,
      col = col,
      las = 2,
      cex.names = cexNsize
    )
    if (hline) {
      abline(
        h = hline,
        lty = lty,
        lwd = lwd,
        col = lcol
      )
    }
    if (vline[1]) {
      abline(
        v = x[vline],
        lty = lty,
        lwd = lwd,
        col = lcol
      )
    }
    if (errorbar) {
      arrows(
        x,
        variable + upper,
        x,
        variable - lower,
        angle = 90,
        code = 3,
        length = arrow_width,
        lwd = arrow_lwd,
        ...
      )
    }
    if (tilted_text) {
      text(
        x = x - 0.25,
        y = 0,
        labels = BarNames,
        xpd = TRUE,
        srt = 45,
        cex = cexNsize,
        adj = c(1, 3)
      )
    }

    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
    if (incrBottMarginBy) {
      par("mar" = .ParMarDefault)
    }
    ww.assign_to_global("plotnameLastPlot", fname, 1)
    if (mdlink & savefile) {
      md.image.linker(fname_wo_ext = fname)
    }
  }



#' whist
#'
#' Create and save histograms as .pdf, in "OutDir". If mdlink = TRUE, it inserts a .pdf and a .png
#' link in the markdown report, set by "path_of_report". The .png version is not created, only the
#' link is put in place, not to overwrite previous versions. Name the file by naming the variable!
#' Cannot be used with dynamically called variables [e.g. call vectors within a loop]. "filtercol"
#' assumes  >= coloring!
#' @param variable The variable to plot.
#' @param breaks Number of bins.
#' @param plotname The name of the file.
#' @param main Title of the plot.
#' @param xlab X-axis label.
#' @param col Color of the plot.
#' @param vline Draw a vertical line at the value you pass on to it. Useful to display a threshold.
#'   Design the line by "lty", "lwd" & "lcol" parameters.
#' @param lty Linetype, defined by numbers 1-6.
#' @param lwd Line width. Set to 2 by default.
#' @param lcol Color of the line.
#' @param filtercol Color bars below / above the threshold with red / green. Define the direction by
#'   -1 or 1. Takes effect if "vline" is defined.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should
#'   work).
#' @param filter filtervalues
#' @param passequal Pass equal values
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by
#'   "path_of_report".
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#' @export
#' @examples MyGauss = rnorm(1000); whist (variable = MyGauss, col = "gold1", w = 7,
#' breaks = 20, mdlink = FALSE, hline = FALSE, vline = FALSE, lty = 2, lwd = 3,
#' lcol = 2, filtercol = 0)

whist <-
  function(variable,
           ...,
           breaks = 20,
           col = unless.specified("b.def.color", "gold1"),
           plotname = substitute(variable),
           main = kollapse("Histogram of ", substitute(variable)),
           xlab = substitute(variable),
           lty = 2,
           lwd = 3,
           lcol = 1,
           filtercol = 0,
           # hline = FALSE,
           vline = FALSE,
           filter = c(FALSE, "HighPass", "LowPass", "MidPass")[1],
           passequal = TRUE,
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           mdlink = ww.set.mdlink(),
           PNG = unless.specified("b.usepng")) {
    xtra = list(...)
    xlb <- xlab # to avoid circular reference in the inside function argument
    if (length(variable) > 0) {
      fname = kollapse(plotname, ".hist")
      if (!is.numeric(variable)) {
        variable = table(variable)
        cexNsize = 0.7 / abs(log10(length(variable)))
        cexNsize = min(cexNsize, 1)
        barplot(
          variable,
          ...,
          main = main,
          xlab = xlb,
          col = col,
          las = 2,
          cex.names = cexNsize,
          sub = paste(
            "mean:", iround(mean(variable, na.rm = TRUE)),
            "CV:", percentage_formatter(cv(variable))
          )
        )
      } else {
        histdata = hist(variable, breaks = breaks, plot = FALSE)
        BRK = histdata$breaks
        NrThr = length(vline)
        if (filtercol == 1  & NrThr == 1) {
          col = (BRK >= vline) + 2
        } else if (filtercol == 1  &     NrThr == 2) {
          col = (BRK >= vline[1] & BRK < vline[2]) + 2
        } else if (filtercol == -1 &     NrThr == 1) {
          col = (BRK < vline) + 2
        } else if (filtercol == -1 &     NrThr == 2) {
          col = (BRK < vline[1] | BRK >= vline[2]) + 2
        }
        hist(
          variable,
          ...,
          main = main,
          breaks = breaks,
          xlab = xlb,
          col = col,
          las = 2
        )
      }
      # if (hline) { abline(h = hline, lty = lty, lwd = lwd, col = lcol) }
      if (!missing(vline) & !length(xtra$xlim)) {
        PozOfvline = NULL

        for (l_ in 1:length(vline)) {
          PozOfvline[l_] = mean(histdata$mids[c(max(which(BRK < vline[l_])),
                                                min(which(BRK >= vline[l_])))])
        }
        abline(
          v = PozOfvline,
          lty = lty,
          lwd = lwd,
          col = lcol
        )
      }
      else if (vline & length(xtra$xlim)) {
        abline(
          v = vline,
          lty = lty,
          lwd = lwd,
          col = 1
        )
      }
      if (savefile) {
        ww.dev.copy(
          PNG_ = PNG,
          fname_ = fname,
          w_ = w,
          h_ = h
        )
      }
    } else {
      Stringendo::iprint(variable, " IS EMPTY")
    }
    ww.assign_to_global("plotnameLastPlot", fname, 1)
    if (mdlink & savefile) {
      md.image.linker(fname_wo_ext = fname)
    }

    if (!is.null(filter)) {
      passequal_ = passequal
      if (filter == "HighPass" & any(vline) ) {
        filter_HP(
          numeric_vector = variable,
          threshold = vline,
          passequal = passequal_,
          plot.hist = FALSE
        )
      } else if (filter == "LowPass" & any(vline) ) {
        filter_LP(
          numeric_vector = variable,
          threshold = vline,
          passequal = passequal_,
          plot.hist = FALSE
        )
      } else if (filter == "MidPass" & any(vline)  & (length(vline) == 2)) {
        filter_MidPass(
          numeric_vector = variable,
          HP_threshold = vline[1],
          LP_threshold = vline[2],
          plot.hist = FALSE
        )
      }
    }
  }




#' wboxplot
#'
#' Create and save box plots as .pdf, in "OutDir". If mdlink = TRUE, it inserts a .pdf and a .png
#' link in the markdown report, set by "path_of_report". The .png version is not created, only the
#' link is put in place, not to overwrite previous versions.
#' @param yourlist The variable to plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should
#'   work).
#' @param main Title of the plot and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param ylab Y axis label
#' @param col Color of the plot.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not
#'   fit on the plot.
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by
#'   "path_of_report".
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#' @export
#' @examples MyList = list(rnorm(100),rnorm(100)); wboxplot (yourlist = MyList,col = "gold1",
#' sub = FALSE, incrBottMarginBy = 0,
#'  tilted_text = FALSE, w = 7, mdlink = FALSE)

wboxplot <-
  function(yourlist,
           main = as.character(substitute(yourlist)),
           sub = FALSE,
           ylab = "",
           col = unless.specified("b.def.colors", "gold1"),
           incrBottMarginBy = 0,
           tilted_text = FALSE,
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           mdlink = ww.set.mdlink(),
           PNG = unless.specified("b.usepng"),
           ...) {
    fname = kollapse(main, ".boxplot")
    if (incrBottMarginBy) {
      .ParMarDefault <- par("mar")
      par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]))
    }   # Tune the margin
    if (tilted_text) {
      xlb = NA
    } else {
      xlb = names(yourlist)
    }
    plotname <-
      main # to avoid circular reference in the inside function argument
    boxplot(
      yourlist,
      ...,
      names = xlb,
      main = plotname,
      col = col,
      las = 2
    )
    mtext(ylab, side = 2, line = 2)
    if (tilted_text) {
      text(
        x = 1:length(yourlist),
        y = min(unlist(yourlist), na.rm = TRUE) - (max(nchar(
          names(yourlist)
        )) / 2),
        labels = names(yourlist),
        xpd = TRUE,
        srt = 45
      )
    }
    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
    ww.assign_to_global("plotnameLastPlot", fname, 1)
    if (incrBottMarginBy) {
      par("mar" = .ParMarDefault)
    }
    if (mdlink & savefile) {
      md.image.linker(fname_wo_ext = fname)
    }
  }



#' wpie
#'
#' Create and save pie charts as .pdf, in "OutDir". If mdlink = TRUE, it inserts a .pdf and a .png
#' link in the markdown report, set by "path_of_report". The .png version is not created, only the
#' link is put in place, not to overwrite previous versions.
#' @param NamedVector The variable to plot.
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should
#'   work).
#' @param percentage Display percentage instead of counts. TRUE by default.
#' @param both_pc_and_value Report both percentage AND number.
#' @param col Fill color. Defined by rich colours by default
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by
#'   "path_of_report".
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#' @export
#' @examples Cake = 1:3; names(Cake) = letters[1:3]; wpie (Cake, percentage = TRUE,
#' w = 7, mdlink = FALSE)

wpie <-
  function(NamedVector,
           percentage = TRUE,
           both_pc_and_value = FALSE,
           plotname = substitute(NamedVector),
           col = gplots::rich.colors(length(NamedVector)),
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           mdlink = ww.set.mdlink(),
           PNG = unless.specified("b.usepng", F),
           ...) {
    # if (!require("gplots")) {
    #   print("Please install gplots: install.packages('gplots')")
    # }
    fname = kollapse(plotname, ".pie")
    subt = kollapse("Total = ", sum(NamedVector), print = FALSE)
    if (percentage) {
      labs <-
        paste("(",
              names(NamedVector),
              ")",
              "\n",
              percentage_formatter(NamedVector / sum(NamedVector)),
              sep = "")
      if (both_pc_and_value) {
        labs <-
          paste(
            "(",
            names(NamedVector),
            ")",
            "\n",
            percentage_formatter(NamedVector / sum(NamedVector)),
            "\n",
            NamedVector,
            sep = ""
          )
      }
    } else {
      labs <- paste("(", names(NamedVector), ")", "\n", NamedVector, sep = "")
    }
    pie(
      NamedVector,
      ...,
      main = plotname,
      sub = subt,
      clockwise = TRUE,
      labels = labs,
      col = col
    )
    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
    if (mdlink & savefile) {
      md.image.linker(fname_wo_ext = fname)
    }
  }



#' wstripchart
#'
#' Create and save strip charts as .pdf, in "OutDir". If mdlink = TRUE, it inserts a .pdf and a .png
#' link in the markdown report, set by "path_of_report". The .png version is not created, only the
#' link is put in place, not to overwrite previous versions.
#'
#' @param yourlist Input list to be plotted.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should
#'   work).
#' @param main Title of the plot (main parameter) and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param ylab Y axis label
#' @param BoxPlotWithMean Display the mean instead of the median in a boxplot. This is non-standard
#'   use of a boxplot, report it.
#' @param border An optional vector of colors for the outlines of the boxplots. The values in border
#'   are recycled if the length of border is less than the number of plots.
#' @param pch Define the symbol for each data point. A number (0-25) or any string between ""-s.
#' @param pchlwd Define the outline width of the symbol for each data point.
#' @param pchcex Define the size of the symbol for each data point.
#' @param bg Background color.
#' @param col Color of the plot.
#' @param metod Method for displaying data points to avoid overlap; either"jitter" or "stack". See
#'   stripchart().
#' @param jitter The amount of horizontal scatter added to the individual data points (to avoid
#'   overlaps).
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not
#'   fit on the plot.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by
#'   "path_of_report".
#' @param cex.lab Cex for labels
#' @param colorbyColumn Color each box by a simple background color? TRUE by default.
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#'
#' @export
#' @examples try.dev.off(); my.ls = list(A = rnorm(10), B = rnorm(10), C = rnorm(10));
#' wstripchart (yourlist = my.ls)

wstripchart <-
  function(yourlist,
           main = as.character(substitute(yourlist)),
           sub = NULL,
           ylab = "",
           BoxPlotWithMean = FALSE,
           border = 1,
           incrBottMarginBy = 0,
           tilted_text = FALSE,
           metod = "jitter",
           jitter = 0.3,
           pch = 18,
           pchlwd = 1,
           cex.lab = 1,
           pchcex = 1.5,
           bg = "seagreen2",
           colorbyColumn = TRUE,
           col = if (colorbyColumn)
             1:length(yourlist)
           else
             1,
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           mdlink = ww.set.mdlink(),
           PNG = unless.specified("b.usepng", F),
           ...) {

    col_ <- col # to avoid circular reference in the inside function argument
    bg_ <- bg

    if (incrBottMarginBy) {
      .ParMarDefault <-
        par("mar")
      par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]))
    }   # Tune the margin
    cexNsize = 1 / abs(log10(length(yourlist)))
    cexNsize = min(cexNsize, 1)
    fname = kollapse(main, ".stripchart")
    a = boxplot(yourlist, plot = FALSE)
    if (colorbyColumn) {
      bg = NULL
    }
    if (BoxPlotWithMean) {
      a$stats[3, ] = unlist(lapply(yourlist, mean))
    }
    if (tilted_text) {
      xlb = FALSE
    } else {
      xlb = TRUE
    }
    plotname <-
      main # to avoid circular reference in the inside function argument
    bxp(
      a,
      xlab = "",
      show.names = xlb,
      ...,
      main = plotname,
      sub = sub,
      border = border,
      outpch = NA,
      las = 2,
      outline = TRUE,
      cex.axis = cexNsize,
      ylab = NA
    )
    stripchart(
      yourlist,
      vertical = TRUE,
      add = TRUE,
      method = metod,
      jitter = jitter,
      pch = pch,
      bg = bg_,
      col = col_,
      lwd = pchlwd,
      cex = pchcex
    )
    mtext(ylab,
          side = 2,
          line = 2,
          cex = cex.lab)
    if (tilted_text) {
      xx = min(unlist(yourlist), na.rm = TRUE)
      text(
        x = 1:length(yourlist),
        y = xx,
        labels = names(yourlist),
        xpd = TRUE,
        srt = 45,
        adj = c(1, 3)
      )
    }
    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
    if (incrBottMarginBy) {
      par("mar" = .ParMarDefault)
    }
    ww.assign_to_global("plotnameLastPlot", fname, 1)
    if (mdlink & savefile) {
      md.image.linker(fname_wo_ext = fname)
    }
  }


#' wstripchart_list
#'
#' Create and save stripcharts from a list as .pdf, in "OutDir". This version allows individual
#' coloring of each data point, by a color-list of the same dimension. If mdlink = TRUE, it inserts a
#' .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not
#' created, only the link is put in place, not to overwrite previous versions.
#' @param yourlist Input list to be plotted.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should
#'   work).
#' @param main Title of the plot (main parameter) and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param bg Background color.
#' @param col Color of the plot.
#' @param bxpcol Color of the boxplot outlines.
#' @param border An optional vector of colors for the outlines of the boxplots. The values in border
#'   are recycled if the length of border is less than the number of plots.
#' @param pch Define the symbol for each data point. A number (0-25) or any string between ""-s.
#' @param pchlwd Define the outline width of the symbol for each data point.
#' @param pchcex Define the size of the symbol for each data point.
#' @param metod Method for displaying data points to avoid overlap; either"jitter" or "stack". See
#'   stripchart().
#' @param jitter The amount of horizontal scatter added to the individual data points (to avoid
#'   overlaps).
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not
#'   fit on the plot.
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by
#'   "path_of_report".
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#' @export
#' @examples try.dev.off(); my.ls = list(A = rnorm(10), B = rnorm(10), C = rnorm(10));
#' wstripchart_list(yourlist = my.ls, sub = NULL, ylab = NULL, xlab = NULL,
#' border = 1, bxpcol = 0, pch = 23, pchlwd = 1, pchcex = 1.5, bg = 'chartreuse2', col = 1,
#' metod = jitter, jitter = 0.2, w = 7, incrBottMarginBy = 0, tilted_text = FALSE, mdlink = FALSE)


wstripchart_list <- function(yourlist,
                             ...,
                             main = as.character(substitute(yourlist)),
                             sub = NULL,
                             ylab = "",
                             xlab = "",
                             border = 1,
                             bxpcol = 0,
                             pch = 18,
                             pchlwd = 1,
                             pchcex = 1.5,
                             incrBottMarginBy = 0,
                             tilted_text = FALSE,
                             bg = "chartreuse2",
                             col = "black",
                             metod = "jitter",
                             jitter = 0.2,
                             savefile = unless.specified("b.save.wplots"),
                             w = unless.specified("b.defSize"),
                             h = w,
                             mdlink = ww.set.mdlink(),
                             PNG = unless.specified("b.usepng", F)) {
  fname = kollapse(main, ".stripchart")
  if (incrBottMarginBy) {
    .ParMarDefault <- par("mar")
    par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]))
  }   # Tune the margin
  cexNsize = 1 / abs(log10(length(list)))
  cexNsize = min(cexNsize, 1)
  if (tilted_text) {
    xlab = FALSE
  } else {
    xlab = TRUE
  }
  plotname <-
    main # to avoid circular reference in the inside function argument
  boxplot(
    yourlist,
    ...,
    show.names = xlab,
    main = plotname,
    sub = sub,
    border = border,
    outpch = NA,
    las = 2,
    ylab = NA,
    col = bxpcol,
    cex.axis = cexNsize
  )
  mtext(ylab, side = 2, line = 2)
  for (i in 1:length(yourlist)) {
    if (length(CodeAndRoll2::na.omit.strip(yourlist[[i]]))) {
      j = k = i
      if (length(1) < length(yourlist)) {
        j = 1
      }
      if (length(bg) < length(yourlist)) {
        k = 1
      }
      stripchart(
        CodeAndRoll2::na.omit.strip(yourlist[[i]]),
        at = i,
        add = TRUE
        ,
        vertical = TRUE,
        method = "jitter",
        jitter = jitter
        ,
        pch = pch,
        bg = bg[[k]],
        col = col[[j]],
        lwd = pchlwd,
        cex = pchcex
      )
    }
  } # for
  if (tilted_text) {
    xx = min(unlist(yourlist), na.rm = TRUE)
    text(
      x = 1:length(yourlist),
      y = xx,
      labels = names(yourlist)
      ,
      xpd = TRUE,
      srt = 45,
      adj = c(1, 3)
    )
  }
  if (savefile) {
    ww.dev.copy(
      PNG_ = PNG,
      fname_ = fname,
      w_ = w,
      h_ = h
    )
  }
  if (incrBottMarginBy) {
    par("mar" = .ParMarDefault)
  }
  ww.assign_to_global("plotnameLastPlot", fname, 1)
  if (mdlink & savefile) {
    md.image.linker(fname_wo_ext = fname)
  }
}




#' wvioplot_list
#'
#' Create and save violin plots as .pdf, in "OutDir". It requires (and calls) "vioplot" package.
#' If mdlink = TRUE, it inserts a .pdf and a .png link in the markdown report,
#' set by "path_of_report". The .png version is not created, only the link is put in place,
#' not to overwrite previous versions.
#'
#' @param yourlist Input list to be plotted.
#' @param ... Pass any other parameter of the corresponding
#' plotting function (most of them should work).
#' @param main Title of the plot (main parameter) and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param ylim Manual y axis limits
#' @param col Color of the plot.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot.
#' Use if labels do not fit on the plot.
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report,
#' set by "path_of_report".
#' @param yoffset Offset for X axis labels (in vertical / Y dimension).
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#' @importFrom vioplot vioplot
#' @importFrom sm sm.density
#'
#' @export
#' @examples try.dev.off(); my.ls = list(A = rnorm(10), B = rnorm(10), C = rnorm(10));
#' # wvioplot_list (yourlist = my.ls, xlab = names(yourlist), ylab = "", incrBottMarginBy = 0,
#' # w = 7, tilted_text = FALSE, mdlink = FALSE)


wvioplot_list <-
  function(yourlist,
           ...,
           main = as.character(substitute(yourlist)),
           sub = NULL,
           xlab = names(yourlist),
           ylab = "",
           ylim = FALSE,
           col = c(2:(length(yourlist) + 1)),
           incrBottMarginBy = 0,
           tilted_text = FALSE,
           yoffset = 0,
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           mdlink = ww.set.mdlink(),
           PNG = unless.specified("b.usepng", F)) {
    stopifnot(is.list(yourlist))
    # if (!require("vioplot")) {
    #   print("Please install vioplot: install.packages('vioplot')")
    # }
    if (incrBottMarginBy) {
      .ParMarDefault <- par("mar")
      par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]))
    }   # Tune the margin
    l_list = length(yourlist)
    fname = kollapse(main, ".vioplot")
    if (length(col) < l_list) {
      col = rep(col, l_list)
    }
    if (tilted_text) {
      xlab = NA
    } else {
      xlab = names(yourlist)
    }
    if (!(is.numeric(ylim) & length(ylim) == 2)) {
      ylim = range(unlist(yourlist), na.rm = TRUE)
    }

    plotname <-
      main # to avoid circular reference in the inside function argument
    ylb <- ylab
    ylimm <- ylim
    plot(
      0,
      0,
      type = "n",
      xlim = c(0.5, (l_list + 0.5)),
      ylim = ylimm,
      xaxt = "n",
      xlab = "",
      ylab = ylb,
      main = plotname,
      sub = sub
    )
    for (i in 1:l_list) {
      if (length(CodeAndRoll2::na.omit.strip(yourlist[[i]]))) {
        vioplot(
          CodeAndRoll2::na.omit.strip(yourlist[[i]]),
          ...,
          at = i,
          add = TRUE,
          col = col[i]
        )
      }
    }
    axis(
      side = 1,
      at = 1:l_list,
      labels = xlab,
      las = 2
    )
    if (tilted_text) {
      text(
        x = 1:length(yourlist),
        y = min(unlist(yourlist)) + yoffset
        ,
        labels = names(yourlist),
        xpd = TRUE,
        srt = 45
      )
    }
    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
    if (incrBottMarginBy) {
      par("mar" = .ParMarDefault)
    }
    ww.assign_to_global("plotnameLastPlot", fname, 1)
    if (mdlink & savefile) {
      md.image.linker(fname_wo_ext = fname)
    }
  }



#' wviostripchart_list
#'
#' Create and save violin plots as .pdf, in "OutDir". It requires (and calls) "vioplot" package.
#' If mdlink = TRUE, it inserts a .pdf and a .png link in the markdown report,
#' set by "path_of_report". The .png version is not created, only the link is put in place,
#' not to overwrite previous versions.
#' @param yourlist Input list to be plotted.
#' @param ... Pass any other parameter of the corresponding plotting
#' function (most of them should work).
#' @param main Title of the plot (main parameter) and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param pch Define the symbol for each data point. A number (0-25) or any string between ""-s.
#' @param viocoll Background color of each individual violing plot.
#' @param vioborder Border color of each individual violing plot.
#' @param bg Background color.
#' @param col Color of the plot.
#' @param metod Method for displaying data points to avoid overlap; either"jitter" or "stack".
#' See stripchart().
#' @param jitter The amount of horizontal scatter added to the individual
#' data points (to avoid overlaps).
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot.
#' Use if labels do not fit on the plot.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report,
#' set by "path_of_report".
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#'
#' @importFrom vioplot vioplot
#' @import sm
#' @export
#' @examples try.dev.off(); my.ls = list(A = rnorm(10), B = rnorm(10), C = rnorm(10));
#' # wviostripchart_list (yourlist = my.ls, pch = 23, viocoll = 0, vioborder = 1, sub = FALSE,
#' # bg = 0, col = "black", metod = "jitter", jitter = 0.1, w = 7, mdlink = FALSE)

wviostripchart_list <-
  function(yourlist,
           ...,
           pch = 20,
           viocoll = c(2:(length(yourlist) + 1)),
           vioborder = 1,
           bg = 1,
           col = 1,
           metod = "jitter",
           jitter = 0.25,
           main = as.character(substitute(yourlist)),
           sub = NULL,
           xlab = names(yourlist),
           ylab = "",
           incrBottMarginBy = 0,
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           mdlink = ww.set.mdlink(),
           PNG = unless.specified("b.usepng", F)) {
    fname = kollapse(main, ".VioStripchart")
    # if (!require("vioplot")) {
    #   print("Please install vioplot: install.packages('vioplot')")
    # }
    if (incrBottMarginBy) {
      .ParMarDefault <- par("mar")
      par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]))
    }   # Tune the margin
    l_list = length(yourlist)

    plotname <- main # to avoid circular reference in the inside function argument
    ylb <- ylab
    plot(
      0,
      0,
      type = "n",
      xlim = c(0.5, (l_list + 0.5)),
      ylim = range(unlist(yourlist), na.rm = TRUE),
      xaxt = "n",
      xlab = "",
      ylab = ylb,
      main = plotname,
      sub = sub
    )
    for (i in 1:l_list) {
      print(i)
      if (length(CodeAndRoll2::na.omit.strip(yourlist[[i]]))) {
        vioplot(
          CodeAndRoll2::na.omit.strip(yourlist[[i]]),
          ...,
          at = i,
          add = TRUE,
          col = viocoll[i],
          border = 1
        )
      } #if
      axis(
        side = 1,
        at = 1:l_list,
        labels = xlab,
        las = 2
      )
    }
    for (i in 1:length(yourlist)) {
      if (length(CodeAndRoll2::na.omit.strip(yourlist[[i]]))) {
        j = k = i
        if (length(col) < length(yourlist)) {
          j = 1
        }
        if (length(bg) < length(yourlist)) {
          k = 1
        }
        stripchart(
          CodeAndRoll2::na.omit.strip(yourlist[[i]]),
          at = i,
          add = TRUE,
          vertical = TRUE,
          method = metod,
          jitter = jitter,
          pch = pch,
          bg = bg[[k]],
          col = col[[j]]
        )
      } #if
    }
    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
    if (incrBottMarginBy) {
      par("mar" = .ParMarDefault)
    }
    ww.assign_to_global("plotnameLastPlot", fname, 1)
    if (mdlink & savefile) {
      md.image.linker(fname_wo_ext = fname)
    }
  }





#' wvenn
#'
#' Save venn diagrams. Unlike other ~vplot funcitons, this saves directly into a .png,
#' and it does not use the dev.copy2pdf() function.
#' @param yourlist The variable to plot.
#' @param imagetype Image format, png by default.
#' @param alpha Transparency, .5 by default.
#' @param fill Background color vec
#' @param subt Subtitle
#' @param ... Pass any other parameter of the corresponding venn.diagram()
#' function (most of them should work).
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report,
#'  set by "path_of_report".
#' @param plotname Manual plotname parameter
#' @param openFolder open current directory (=working if setup_MarkdownReports('setDir=T'))
#'
#' @export
#' @examples TwoSets = list("set1" = LETTERS[1:6], "set2" = LETTERS[3:9] )
#' wvenn (yourlist = TwoSets, imagetype = "png", alpha = 0.5, w = 7, mdlink = FALSE)

# @importFrom VennDiagram venn.diagram
wvenn <-
  function(yourlist,
           imagetype = "png",
           alpha = .5,
           fill = 1:length(yourlist),
           subt,
           ...,
           w = unless.specified("b.defSize", 7),
           h = w,
           mdlink = ww.set.mdlink(),
           plotname = substitute(yourlist),
           openFolder = T) {

    # if (!require("VennDiagram")) {
    #   print("Please install VennDiagram: install.packages('VennDiagram')")
    # }

    print(plotname)
    fname = kollapse(plotname, ".", imagetype, print = FALSE)

    LsLen = length(yourlist)
    if (length(names(yourlist)) < LsLen) {
      names(yourlist) = 1:LsLen
      print("List elements had no names.")
    }

    filename = kollapse(ww.set.OutDir(), fname, print = FALSE)

    if (missing(subt)) {
      subt = kollapse("Total = ", length(unique(unlist(yourlist)))
                      , " elements in total.", print = FALSE)
    } #if
    # print(filename)

    VennDiagram::venn.diagram(
      x = yourlist,
      imagetype = imagetype,
      filename = filename,
      main = plotname,
      ...,
      sub = subt,
      fill = fill,
      alpha = alpha,
      sub.cex = .75,
      main.cex = 2
    )
    # print(names(yourlist))

    if (mdlink) {
      llogit(ww.md.image.link.parser(fname))
      if (b.usepng == TRUE && b.png4Github == TRUE) {
        llogit(ww.md.image.link.parser(paste0("Reports/", fname)))
      }
    }
    if (openFolder) system("open .")
  }



# ______________________________________________________________________________________________
# Plots for cycling over data frame columns or rows ----
# _________________________________________________________________________________________________


#' @title wbarplot_dfCol
#'
#' @description wbarplot for a column of a data frame.
#'
#' @param df Input data frame to be plotted
#' @param ... Pass any other parameter of the corresponding
#' plotting function (most of them should work).
#' @param col Color of the plot.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param colName Which column to plot (by name).
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#'
#' @export
#' @examples try.dev.off(); df = cbind(a = rnorm(1:10), b = rnorm(10))
#' wbarplot_dfCol (df, colName = "a",  col = "gold1", w = 7)



wbarplot_dfCol <-
  function(df,
           ...,
           colName,
           col = unless.specified("b.def.colors", "gold1"),
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           PNG = unless.specified("b.usepng", F)) {
    stopifnot(colName %in% colnames(df))
    variable = unlist(df[, colName])
    stopifnot(length(variable) > 1)
    plotname = paste(substitute(df), "__", colName, sep = "")
    fname = ww.FnP_parser(plotname, "barplot.pdf")
    cexNsize = 0.7 / abs(log10(length(variable)))
    cexNsize = min(cexNsize, 1)
    barplot(
      variable,
      ...,
      main = plotname,
      col = col,
      las = 2,
      cex.names = cexNsize,
      sub = paste("mean:", iround(mean(variable, na.rm = TRUE))
                  , "CV:", percentage_formatter(cv(variable)))
    )
    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
  }

#' whist_dfCol
#'
#' Use this version of whist() if you iterate over columns  or rows of a data frame.
#' You can name the file by naming the variable.
#' Cannot be used with dynamically called variables (e.g. call vectors within a loop).
#'
#' @param df Input data frame to be plotted
#' @param col Color of the plot.
#' @param ... Pass any other parameter of the corresponding
#' plotting function (most of them should work).
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param colName Which column to plot (by name).
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#'
#' @export
#' @import stats
#'
#' @examples try.dev.off(); df = cbind(a = rnorm(1:10), b = rnorm(10))
#' whist_dfCol (df, colName="a", col = "gold", w = 7)

whist_dfCol <-
  function(df,
           colName,
           col = unless.specified("b.def.colors", "gold1"),
           ...,
           savefile = unless.specified("b.save.wplots"),
           w = unless.specified("b.defSize", 7),
           h = w,
           PNG = unless.specified("b.usepng", F)) {
    stopifnot(colName %in% colnames(df))
    variable = as.vector(unlist(df[, colName]))
    stopifnot(length(variable) > 1)
    plotname = paste(substitute(df), "__", colName, sep = "")
    fname = ww.FnP_parser(plotname, "hist.pdf")
    if (!is.numeric(variable)) {
      table_of_var = table(variable)
      cexNsize = 0.7 / abs(log10(length(table_of_var)))
      cexNsize = min(cexNsize, 1)
      barplot(
        table_of_var,
        ...,
        main = plotname,
        col = col,
        las = 2,
        cex.names = cexNsize,
        sub = paste(
          "mean:",iround(mean(table_of_var, na.rm = TRUE)),
          "| median:",iround(median(table_of_var, na.rm = TRUE)),
          "| mode:",iround(modus(table_of_var)),
          "| CV:",percentage_formatter(cv(table_of_var))))
    }
    else {
      zz = hist(variable, ..., plot = FALSE)
      hist(
        variable,
        ...,
        main = plotname,
        col = col,
        las = 2,
        sub = paste(
          "mean:",iround(mean(variable)),
          "| median:",iround(median(variable)),
          "| modus:",iround(modus(variable))))
    }
    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
  }

# ______________________________________________________________________________________________
# A4 pdfs for multi-plots ----
# _________________________________________________________________________________________________

#' @title pdfA4plot_on
#'
#' @description Create A4 PDFs to plot multiple subplots in one file
#' @param pname Title of the plot (main parameter) and also the name of the file.
#' @param ... Pass any other parameter of the corresponding
#' plotting function (most of them should work).
#' @param w Width of the saved pdf image, in inches. c("A4" = 8.27, "1col.nature" = 3.50,
#' "2col.nature" = 7.20, "1col.cell" = 3.35, "1.5col.cell" = 4.49, "2col.cell" = 6.85).
#' @param h Height of the saved pdf image, in inches.
#' @param rows Number of rows for subplots
#' @param cols Number of columns for subplots
#' @param one_file Allows multiple figures in one file, if true (default).
#' Set to FALSE to use with pheatmap / grid.base
#' @param mdlink Insert a .pdf and a .png image link in the markdown report,
#' set by "path_of_report".
#' @param title Manually set the title field of the PDF file
#' @export
#' @import graphics grDevices
#' @examples pdfA4plot_on(pname = "MyA4plots");  hist(rnorm(100)); hist(-rnorm(100))
#'  hist(10+rnorm(100)); pdfA4plot_off()


pdfA4plot_on <-
  function(pname = date(),
           ...,
           w = unless.specified("b.defSize.fullpage", 8.27),
           h = 11.69,
           rows = 4,
           cols = rows - 1,
           one_file = TRUE,
           mdlink = ww.set.mdlink(),
           title = ww.ttl_field(pname)) {
    fname = ww.FnP_parser(pname, "pdf")
    try.dev.off()
    ww.assign_to_global("b.mfrow_def", par("mfrow"), 1)
    ww.assign_to_global("b.bg_def", par("bg"), 1)
    ww.assign_to_global("b.save.wplots", FALSE, 1) # switch of "savefile" option
    pdf(
      fname,
      width = w,
      height = h,
      title = title,
      onefile = one_file
    )
    par(mfrow = c(rows, cols), bg = "white")
    Stringendo::iprint(
      " ----  Don't forget to call the pair of this function to finish
      plotting in the A4 pdf.: pdfA4plot_off ()"
    )
    if (mdlink) {
      md.image.linker(fname_wo_ext = pname)
    }
  }

#' pdfA4plot_on.layout
#'
#' Create A4 PDFs to plot multiple subplots in one file with custom numbers of columns in each row.
#' Fancy layout version of pdfA4plot_on()
#' @param pname Title of the plot (main parameter) and also the name of the file.
#' @param ... Pass any other parameter of the corresponding plotting function
#' (most of them should work).
#' @param layout_mat A matrix of plot layout. Default: rbind(1, c(2, 3), 4:5)
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param one_file Allows multiple figures in one file, if true (default).
#' Set to FALSE to use with pheatmap / grid.base
#' @param mdlink Insert a .pdf and a .png image link in the markdown report,
#' set by "path_of_report".
#' @param title Manually set the title field of the PDF file
#' @export
#' @import graphics grDevices
#'
#' @examples pdfA4plot_on.layout(pname = "MyA4_w_layout");  hist(rnorm(100)); hist(-rnorm(100))
#' hist(10+rnorm(100)); pdfA4plot_off()


pdfA4plot_on.layout <-
  function(pname = date(),
           ...,
           layout_mat = rbind(1, c(2, 3), 4:5),
           w = unless.specified("b.defSize.fullpage", 8.27),
           h = 11.69,
           one_file = TRUE,
           mdlink = ww.set.mdlink(),
           title = ww.ttl_field(pname)) {
    fname = ww.FnP_parser(pname, "pdf")
    try.dev.off()
    ww.assign_to_global("b.bg_def", par("bg"), 1)
    ww.assign_to_global("b.save.wplots", FALSE, 1) # switch of "savefile" option
    pdf(
      fname,
      width = w,
      height = h,
      title = title,
      onefile = one_file
    )
    layout(layout_mat)
    # par(mar = c(3, 3, 0, 0))
    print(layout_mat)
    Stringendo::iprint(
      " ----  Don't forget to call the pair of this function to finish
      plotting in the A4 pdf.: pdfA4plot_off ()"
    )
    if (mdlink) {
      md.image.linker(fname_wo_ext = pname)
    }
  }


#' pdfA4plot_off
#'
#' pair of the "pdfA4plot_on()" function; to finish plotting in the A4 pdf.
#' @export
#' @import graphics grDevices
#' @importFrom clipr write_clip
#' @examples pdfA4plot_on.layout(pname = "MyA4_w_layout");  hist(rnorm(100)); hist(-rnorm(100))
#' hist(10+rnorm(100)); pdfA4plot_off()

pdfA4plot_off <- function() {
  x = if (exists("b.mfrow_def"))
    b.mfrow_def
  else
    c(1, 1)
  y = if (exists("b.bg_def"))
    b.bg_def
  else
    "white"
  if (exists("b.save.wplots")) {
    ww.assign_to_global("b.save.wplots", TRUE, 1) # switch back mdlink to its original value
  }
  par(mfrow = x, bg = y)
  try.dev.off()
  # close pdf
  if (exists("OutDir")) { try(write_clip(OutDir), silent = TRUE) }
}


# ______________________________________________________________________________________________----
# Add-ons to exisiting plots ----
# _________________________________________________________________________________________________


#' @title error_bar
#'
#' @description Put error bars on top of your bar plots. This functionality is now integrated into
#' MarkdownReporter's wbarplot() function
#' @param x X-position on the plot.
#' @param y Y-position on the plot.
#' @param upper Size of the upper error bar.
#' @param lower Size of the lower error bar. By default, it equals the upper error bar.
#' @param width.whisker Width of the error bar whisker.
#' @param ... Pass any other argument to the arrows function.
#' arrows function (most of them should work).
#' @export
#' @examples plot (1); error_bar (x = 1, y = 1, upper = .1, width.whisker = 0.1)

error_bar <-
  function(x,
           y,
           upper,
           lower = upper,
           width.whisker = 0.1,
           ...) {
    stopifnot(length(x) == length(y) &  length(y) == length(lower) & length(lower) == length(upper))
    if (length(dim(y)) > 1) {
      arrows(
        as.vector(x),
        as.vector(y + upper),
        as.vector(x),
        as.vector(y - lower),
        angle = 90,
        code = 3,
        length = width.whisker,
        ...
      )
    }
    else {
      arrows(
        x,
        y + upper,
        x,
        y - lower,
        angle = 90,
        code = 3,
        length = width.whisker,
        ...
      )
    }
  }




#' wlegend
#'
#' Quickly add a legend to an existing plot, and save the plot immediately.
#' @param NamedColorVec Color of the boxes next to the text
#' @param poz Position of the legend (def: 4). Use numbers 1-4 to choose from
#' "topleft", "topright", "bottomright", "bottomleft".
#' @param legend Labels displayed (Text)
#' @param ... Additional parameters for legend()
#' @param cex font size
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param bty The type of box to be drawn around the legend.
#' The allowed values are "o" (the default) and "n".
#' @param title What should be the title of the legend? NULL by default
#' @param ttl.by.varname Should the title of the legend substituted from the NamedColorVec variable's name?
#' ALSE by default. Does not work if you pass on a list item like this: list$element
#' @param OverwritePrevPDF Save the plot immediately with the same name
#' the last wplot* function made (It is stored in plotnameLastPlot variable).
#' @param mdlink Insert a .pdf and a .png image link in the markdown report
#', set by "path_of_report".
#' @export
#' @examples try.dev.off(); x = cbind(a = rnorm(1:10), b = rnorm(10)); wplot(x)
#' LegendCols = 2:5; names(LegendCols) = LETTERS[1:4]
#' wlegend(NamedColorVec = LegendCols, poz = 1, w = 7, bty = "n", OverwritePrevPDF = TRUE)

wlegend <-
  function(NamedColorVec = NA,
           poz = 4,
           legend,
           cex = .75,
           bty = "n",
           ...,
           w = 7,
           h = w,
           title = NULL,
           ttl.by.varname = FALSE,
           OverwritePrevPDF = unless.specified("b.save.wplots"),
           mdlink = FALSE) {
    w_ <- w # to avoid circular reference in the inside function argument
    h_ <- h
    cex_ <- cex

    fNames = names(NamedColorVec)
    LF = length(NamedColorVec)
    LN = length(fNames)
    if (ttl.by.varname & is.null(title))
      title = substitute(NamedColorVec)
    Stringendo::stopif((LN != LF & missing(legend)),
           message = "The color vector (NamedColorVec) has less names than entries /
           the variable 'legend' is not provided.")
    # Stringendo::stopif( ( LF  != length(legend)), message = "Fill and legend are not equally long.")
    legend = if (LN == LF & missing(legend))
      fNames
    else
      legend
    pozz = translate(
      poz,
      oldvalues = 1:4,
      newvalues = c("topleft", "topright", "bottomright", "bottomleft")
    )
    legend(
      x = pozz,
      legend = legend,
      fill = NamedColorVec,
      title = title,
      ...,
      bty = bty,
      cex = cex_
    )
    if (OverwritePrevPDF) {
      wplot_save_this(
        plotname = ww.set.PlotName(),
        w = w_,
        h = h_,
        mdlink = mdlink
      )
    }
  }


#' wlegend.label
#'
#' Quickly add a "text only" legend without a filled color box. to an existing plot,
#' and save the plot immediately. Never inserts an mdlink.
#' @param legend Labels displayed (Text)
#' @param poz Position of the legend (def: 4). Use numbers 1-4 to choose from "topleft",
#'  "topright", "bottomright", "bottomleft".
#' @param ... Additional parameters for legend()
#' @param cex font size
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param bty The type of box to be drawn around the legend.
#' The allowed values are "o" (the default) and "n".
#' @param title What should be the title of the legend? NULL by default
#' @param ttl.by.varname Should the title of the legend substituted from the NamedColorVec variable's name?
#' FALSE by default. Does not work if you pass on a list item like this: list$element
#' @param OverwritePrevPDF Save the plot immediately with the same name
#' the last wplot* function made (It is stored in plotnameLastPlot variable).
#' @param mdlink Insert a .pdf and a .png image link in the markdown report,
#' set by "path_of_report".
#' @export
#' @examples x = cbind(a = rnorm(1:10), b = rnorm(10)); wplot(x);
#' wlegend.label(legend = "Hey", poz = 2,  w = 7, bty = "n", OverwritePrevPDF = TRUE)

wlegend.label <-
  function(legend = "...",
           poz = 1,
           cex = 1,
           bty = "n",
           ...,
           w = 7,
           h = w,
           title = NULL,
           ttl.by.varname = FALSE,
           OverwritePrevPDF = unless.specified("b.save.wplots"),
           mdlink = FALSE) {
    w_ <- w # to avoid circular reference in the inside function argument
    h_ <- h
    cex_ <- cex

    pozz = translate(
      poz,
      oldvalues = 1:4,
      newvalues = c("topleft", "topright", "bottomright", "bottomleft")
    )
    legend(
      x = pozz,
      legend = legend,
      title = title,
      ...,
      bty = bty,
      cex = cex_
    )
    if (OverwritePrevPDF) {
      wplot_save_this(
        plotname = plotnameLastPlot,
        w = w_,
        h = h_,
        mdlink = mdlink
      )
    }
  }


#' barplot_label
#'
#' Add extra labels to your bar plots at the top or the base.
#' @param barplotted_variable The variable that you barplotted previously.
#' @param labels Label text.
#' @param bottom Put labels at the bottom of the bars.
#' @param TopOffset Absolute offset from top.
#' @param relpos_bottom Relative offset from bottom.
#' @param OverwritePrevPDF Save the plot immediately with the same name the last
#' wplot* function made (It is stored in plotnameLastPlot variable). Never inserts an mdlink.
#' @param filename Filename to overwrite after errorbars are added to the current barplot.
#' @param PNG_ Set to true if you want to save the plot as PNG instead of the default PDF.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param ... Pass any other parameter of the corresponding
#' text function (most of them should work).
#' @import graphics
#' @export
#'
#' @examples barplot (1:10);
#' barplot_label(barplotted_variable = 1:10, labels = 11:2, filename = "myBarplot.pdf")



barplot_label <-
  function(barplotted_variable,
           labels = iround(barplotted_variable),
           bottom = FALSE,
           TopOffset = .5,
           relpos_bottom = 0.1,
           OverwritePrevPDF = unless.specified("b.save.wplots"),
           filename = plotnameLastPlot,
           PNG_ = unless.specified("b.usepng",F),
           w = 7,
           h = w,
           ...) {
    w_ = w
    h_ = h
    x = barplot(barplotted_variable, plot = FALSE)
    y = barplotted_variable
    # stopifnot(length(x) == length(y))
    if (bottom) {
      y = rep(relpos_bottom * max(y, na.rm = TRUE), length(x))
    }
    if (length(dim(x)) > 1) {
      text(x = as.vector(x),
           y = as.vector(y - TopOffset),
           labels = as.vector(labels),
           ...)
    }
    else if (length(dim(x)) == 1) {
      text(x, y, labels = labels, ...)
    }
    if (OverwritePrevPDF) {
      wplot_save_this(plotname = filename, mdlink = FALSE, PNG = PNG_, w = w_, h = h_, ...)
    }
  }

#'wLinRegression
#'
#' Add linear regression, and descriptors to line to your scatter plot.
#' Provide the same dataframe as you provided to wplot() before you called this function
#' @param DF  The same dataframe as you provided to wplot() before you called this function
#' @param coeff What coefficient to display? Either "all", "pearson", "spearman"
#' correlation values or "r2" for the Coefficient of Determination.
#' @param textlocation where to put the legend?
#' @param cex font size; 1 by default
#' @param OverwritePrevPDF Save the plot immediately with the same name the last
#' wplot* function made (It is stored in plotnameLastPlot variable). Never inserts an mdlink.
#' @param ...  Additional parameters for the line to display.
#' @export
#' @import stats
#' @examples try.dev.off(); x = cbind(a = rnorm(1:10), b = rnorm(10)); wplot(x)
#' # wLinRegression(x, coeff = c("pearson", "spearman", "r2")[3])

wLinRegression <-
  function(DF,
           coeff = c("pearson", "spearman", "r2")[3],
           textlocation = "topleft",
           cex = 1,
           OverwritePrevPDF = unless.specified("b.save.wplots"),
           ...) {
    regression <- lm(DF[, 2] ~ DF[, 1])
    abline(regression, ...)
    legendText = NULL
    if (coeff == "all")
      coeff = c("pearson", "spearman", "r2")
    if ("pearson" %in% coeff) {
      dispCoeff = iround(cor(DF[, 2], DF[, 1], method = "pearson"))
      legendText = c(legendText, paste0("Pears.: ", dispCoeff))
    }
    if ("spearman" %in% coeff) {
      dispCoeff = iround(cor(DF[, 2], DF[, 1], method = "spearman"))
      legendText = c(legendText, paste0("Spear.: ", dispCoeff))
    }
    if ("r2" %in% coeff) {
      r2 = iround(summary(regression)$r.squared)
      legendText = c(legendText, paste0("R^2: ", r2))
    }
    cexx <- cex

    if (length(coeff) == 1 & "r2" == coeff[1]) {
      legend(
        textlocation,
        legend = superscript_in_plots(
          prefix = "R",
          sup = "2",
          suffix = paste0(": ", r2)
        ),
        bty = "n",
        cex = cexx
      )
    } else {
      legend(textlocation,
             legend = legendText,
             bty = "n",
             cex = cexx)
    }
    if (OverwritePrevPDF) {
      wplot_save_this(plotname = plotnameLastPlot)
    }
  }



# ______________________________________________________________________________________________----
# Graphics and Internal functions ----
# _________________________________________________________________________________________________


#' subscript_in_plots
#'
#' Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
#' Create an expression with subscript for axis labels.
#' Parsed when provided to xlab or ylab of a function.
#' @param prefix String before the subscript.
#' @param subscr Subscripted text.
#' @param quantity String in brackets after the subscript, eg.: log2(read count).
#' @export
#' @examples plot (1, 1, xlab = subscript_in_plots(subscr = 10, quantity = "read count"),
#'  ylab = subscript_in_plots())

subscript_in_plots <-
  function(prefix = "log",
           subscr = 2,
           quantity = "arbitrary units") {
    formatted_string = bquote(.(prefix)[.(subscr)] * '(' * .(quantity) * ')')
  }


#' @title superscript_in_plots
#'
#' @description Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
#' Create an expression with superscript for axis labels.
#' Parsed when provided to xlab or ylab of a function.
#' @param prefix String before the superscript.
#' @param sup Superscripted text.
#' @param suffix String after the subscript.
#' @export
#' @examples plot (1, 1, main = superscript_in_plots())

superscript_in_plots <- function(prefix = 'n',
                                 sup = 'k',
                                 suffix = '') {
  formatted_string = bquote(.(prefix) ^ .(sup) * .(suffix))
}







#' ww.dev.copy
#'
#' Parser for dev.copy to save as PDF or PNG
#' @param PNG_ Set to true if you want to save the plot as PNG instead of the default PDF.
#' @param PNG_res default 100
#' @param w_ Width of the saved pdf image, in inches.
#' @param h_ Height of the saved pdf image, in inches.
#' @param fname_ File name
#' @export
#' @examples try.dev.off(); plot(1); # ww.dev.copy(PNG = FALSE, w_ = 7, h_ = 7, fname_ = "myNewplot")

ww.dev.copy <- function(PNG_ = FALSE,
                        PNG_res = 100,
                        w_,
                        h_,
                        fname_) {
  if (PNG_) {
    dev.copy(
      device = png,
      filename = ww.FnP_parser(fname_, "png"),
      res = PNG_res,
      width = w_ * 100,
      height = h_ * 100
    )
    try.dev.off()
  } else {
    dev.copy2pdf(
      file = ww.FnP_parser(fname_, "pdf"),
      width = w_,
      height = h_,
      title = ww.ttl_field(fname_)
    )
  }
}




