## MarkdownReports.R
# author: Abel Vertesy
# date: # 14 September 2017 (Monday) 12:38

# Table of Contents ------------------------------------
# - Setup
# - Plots
# - Plots for cycling over data frame columns or rows
# - A4 pdfs for multi-plots
# - Add-ons to exisiting plots
# - Graphics
# - Colors
# - Printing to the markdown file and to the screen
# - Writing markdown tables
# - Filtering Data
# - Generic
# - String Manipulation
# - Annotation parse / create / manipulate
# - Internal function
# - Legacy functions
# - Alternative versions


# Setup -------------------------------------------------------------------------------------------------

#' setup_MarkdownReports
#'
#' Setup the markdown report file and the output directory, create a sub directory in "OutDir". Its name is stamped with the script name and the modification time. Create the "path_of_report" variable used by all log-writing and ~wplot functions.
#' @param OutDir The output directory (absolute / full path).
#' @param b.scriptname Name of the script (file) generating the report. "b.scriptname" will be used as the default title for the report. It is assigned to the global environment and used in pdf's title field to denote which script generated the file.
#' @param title Manually set the title of the report.
#' @param append Set append to TRUE if you do not want to overwrite the previous report. Use continue_logging_markdown() if you return logging into an existing report. FALSE by default: rerunning the script overwrites the previous report. Archive reports manually into the timestamped subfolder within the OutDir.
#' @param b.defSize Default width of plot EXCEPT in pdfA4plot_on(), assuming h=w by default. c("def"= 7, "A4" = 8.27, "1col.nature" = 3.50, "2col.nature" = 7.20, "1col.cell" = 3.35, "1.5col.cell" = 4.49, "2col.cell" = 6.85)
#' @param b.defSize.fullpage Default width of plot in pdfA4plot_on()A global background variable used by pdfA4plot_on.
#' @param b.usepng A global background variable used by the plotting functions. If TRUE, a link to the .png versions of the saved plot will be created. The .png file itself is not created.
#' @param b.png4Github A global background variable used by the plotting functions. If TRUE (default), the link to the .png versions of the saved plot will be created in a GitHub compatible format.  That means, when you upload your markdown report and the .png images to your GitHub wiki under "Reports/" the links will correctly display the images online.
#' @param b.mdlink A global background variable used by the plotting functions. If TRUE (default), all saved (.pdf) plots will be linked into your report.
#' @param b.save.wplots A global background variable used by the plotting functions. If TRUE (default), plots will be saved to a .pdf file.
#' @param addTableOfContents write '[TOC]' below the header of the file, This is compiled to a proper Table Of Contents by, e.g. Typora.
#' @export
#' @examples setup_logging_markdown (scriptname =  , title =  , append = T, b.png4Github = T)

setup_MarkdownReports <- function (OutDir = getwd(), scriptname = basename(OutDir), title = "", setDir=T, append = F, addTableOfContents=F
                                  , b.defSize = c("def"= 7, "A4" = 8.27, "1col.nature" = 3.50, "2col.nature" = 7.20, "1col.cell" = 3.35, "1.5col.cell" = 4.49, "2col.cell" = 6.85)[1]
                                  , b.defSize.fullpage = 8.27, b.usepng = F, b.png4Github = T, b.mdlink = T, b.save.wplots = T) {
  if (!exists(OutDir)) {	dir.create(OutDir)	}
  if ( ! substrRight(OutDir, 1) == "/" )  OutDir = paste0(OutDir, "/") # add '/' if necessary

  assign("OutDir", OutDir, envir = .GlobalEnv)
  iprint("All files will be saved under 'OutDir': ", OutDir)
  path_of_report <- paste0(OutDir, scriptname, ".log.md")
  assign("path_of_report", path_of_report, envir = .GlobalEnv)
  iprint("MarkdownReport location is stored in 'path_of_report': ", path_of_report)

  if (nchar(title)) {	write(paste("# ", title), path_of_report, append = append)
  } else {			write(paste("# ", scriptname, "Report"), path_of_report, append = append) }
  write(paste0("		Modified: ", format(Sys.time(), "%d/%m/%Y | %H:%M | by: "), scriptname), path_of_report, append = T)

  if (addTableOfContents) write('[TOC]', path_of_report, append = T)
  BackupDir = kollapse(OutDir, "/", substr(scriptname, 1, nchar(scriptname)), "_", format(Sys.time(), "%Y_%m_%d-%Hh"), print = F)
  if (setDir) {	setwd(OutDir)}
  if (!exists(BackupDir)) {
    dir.create(BackupDir)
    assign("BackupDir", BackupDir, envir = .GlobalEnv)
  }
  assign("b.defSize", b.defSize, envir = .GlobalEnv)
  assign("b.defSize.fullpage", b.defSize.fullpage, envir = .GlobalEnv)
  assign("b.mdlink", b.mdlink, envir = .GlobalEnv)
  assign("b.save.wplots", b.save.wplots, envir = .GlobalEnv)
  assign("b.usepng", b.usepng, envir = .GlobalEnv)
  assign("b.png4Github", b.png4Github, envir = .GlobalEnv)
  assign("b.scriptname", scriptname, envir = .GlobalEnv)
}

# create_set_SubDir
#'
#' Create or set the output directory of the script, and set the "NewOutDir" variable that is used by all ~wplot functions.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @param makeOutDirOrig Change the "OutDirOrig" variable to the current OutDir (before setting it to a subdir).
#' @param setDir Change working directory to the newly defined subdirectory
#' @export
#' @examples create_set_NewOutDir (...)

create_set_SubDir <- function (..., makeOutDirOrig=T, setDir=T) {
  if ( ! substrRight(OutDir, 1) == "/" )  OutDir = paste0(OutDir, "/") # add '/' if necessary
  NewOutDir = kollapse(OutDir,  ..., print = F)
  if ( ! substrRight(NewOutDir, 1) == "/" )  NewOutDir = paste0(NewOutDir, "/") # add '/' if necessary
  NewOutDir = gsub(x=NewOutDir, pattern = '//', replacement = '/') # replace //
  iprint("All files will be saved under 'NewOutDir': ", NewOutDir)
  if (!exists(NewOutDir)) {	dir.create(NewOutDir)	}
  if (setDir) {	setwd(NewOutDir)}
  if (makeOutDirOrig) {
    if (exists("OutDirOrig")) iprint("OutDirOrig was defined as:", OutDirOrig)
    iprint("OutDirOrig will be:", OutDir)
    assign("OutDirOrig", OutDir, envir = .GlobalEnv)
  } #if
  assign("OutDir", NewOutDir, envir = .GlobalEnv)
}

#' continue_logging_markdown
#'
#' Continue writing to an existing report file.
#' @param fname Name of the report file.
#' @export
#' @examples continue_logging_markdown (fname =  )

continue_logging_markdown <- function (b.scriptname) {
  if (exists("OutDir")) {	path = OutDir } else {	path = getwd(); iprint("OutDir not defined !!! Saving in working directory.") }
  path_of_report <- kollapse(path, "/", b.scriptname, ".log.md", print = F)
  iprint("Writing report in:", path_of_report)
  assign("path_of_report", path_of_report, envir = .GlobalEnv)

  BackupDir = kollapse(OutDir, "/", substr(b.scriptname, 1, (nchar(b.scriptname) - 2)), format(Sys.time(), "%Y_%m_%d-%Hh"), print = F)
  if (!exists(BackupDir)) {
    dir.create(BackupDir)
    assign("BackupDir", BackupDir, envir = .GlobalEnv)
  }
}

#' create_set_OutDir
#'
#' Create or set the output directory of the script, and set the "OutDir" variable that is used by all ~wplot functions.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples create_set_OutDir (... =  )

create_set_OutDir <- function (..., setDir=T) {
  OutDir = kollapse(..., print = F)
  if ( ! substrRight(OutDir, 1) == "/" )  OutDir = paste0(OutDir, "/") # add '/' if necessary
  OutDir = gsub(x=OutDir, pattern = '//', replacement = '/')
  iprint("All files will be saved under 'OutDir': ", OutDir)
  if (!exists(OutDir)) {	dir.create(OutDir)	}
  if (setDir) {	setwd(OutDir)}
  assign("OutDir", OutDir, envir = .GlobalEnv)
}


# Plots ------------------------------------------------------------------------------------------

#' wplot_save_this
#'
#' Save the currently active graphic device (for complicated plots).  Insert links to your markdown report, set by "path_of_report". Name the file by naming the variable!
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples wplot_save_this (plotname = date(), col = gold1, ... =  , w = 7, h = w, mdlink = F, ManualName = FALSE)

wplot_save_this <- function (plotname = ww.autoPlotName(), ..., w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink()) {
  dev.copy2pdf(file = ww.FnP_parser(plotname, "pdf"), width = w, height = h, title =  ww.ttl_field(flname = plotname ) )
  if (mdlink) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = plotname) }
}

#' wplot
#'
#' Create and save scatter plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions. The .png version is not created, only the link is put in place. You can add 2D error bars around the dots, or add lines (ablines) to your plot, by setting "abline" argument to = F (no line, default), "h" (horizontal, further specified by a = y-offset), "v" (vertical, further specified by a = x-offset), "ab" (line with an angle, further specified by a = offset, b = slope).
#' @param df Input data frame to be plotted_2columns
#' @param col Color of the plot.
#' @param pch Define the symbol for each data point. A number [0-25] or any string between ""-s.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param panel.first Draw a background grid similar as in ggplot2. Set to "panel.first=grid()" to draw an auto-fitted grid, (nx=NULL, ny=0) to draw vertical only, etc.
#' @param errorbar Draw error bars if TRUE. Pass on the value in parameters "upper" and "lower". Refine the look by "w" and "arrow_lwd".
#' @param upper Size of the upper error bar.
#' @param lower Size of the lower error bar. By default, it equals the upper error bar.
#' @param left Size of the left error bar.
#' @param right Size of the right error bar. By default, it equals the left error bar.
#' @param arrow_lwd Line width for the error bar arrow. Line width for the error bar arrow.
#' @param col_errorbar Color of the error bar arrow.
#' @param abline Draw a line on the plot. Any value from: c( 'v', 'h', 'ab') for vertical, horizontal, and line with any slope. In each case you need to specify "a=", specifying the X-position for vertical (v); the Y-position for horizontal (yh) lines and the intercept for lines with a slope (ab). In the latter case, 'b' specifes the slope.
#' @param a X-offset for vertical lines, Y-offset for horizontal, and inclined lines.
#' @param b Slope of an inclined line.
#' @param lty Linetype, defined by numbers 1-6.
#' @param lwd Line width. Set to 2 by default.
#' @param col_abline Color of the line.
#' @param equal.axes Span of axes is set to equal (maximum range in either X or Y).
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param ylimm manual Y-limits error bar
#' @param xlimm manual X-limits error bar
#' @export
#' @examples wplot (df_2columns =  , col = 1, pch = 18, ... =  , w = 7, h = w, plotname = substitute(df_2columns), mdlink = F, errorbar = F, upper = 0, lower = upper, left = 0, right = left, width = 0.1, arrow_lwd = 1, abline = F, a = F, b = F, lty = 1, lwd = 1, col_abline = 1)

wplot <- function (df_2columns, col = 1, pch = 18, ..., plotname = substitute(df_2columns), panel.first=grid(F),
                  errorbar = F, upper = 0, lower = upper, left = 0, right = left, width = 0.1, arrow_lwd = 1, col_errorbar = 1, ylimm=F, xlimm=F,
                  abline = c( F, 'v', 'h', 'ab')[1], a = F, b = F, lty = 1, lwd = 1, col_abline = 1, equal.axes =F,
                  savefile = UnlessSpec("b.save.wplots"), mdlink = ww.set.mdlink(), w = UnlessSpec("b.defSize", 7), h = w) {
  x = df_2columns[, 1]
  y = df_2columns[, 2]
  fname = kollapse(plotname, ".plot")
  if (errorbar) {
    ylim = range(c( (y + upper + abs(0.1 * y)), (y - lower - abs(0.1 * y))))
    xlim = range(c( (x + right + abs(0.1 * x)), (1.1 * x - left - abs(0.1 * x))))
  }
  else {
    ylim = range(y)
    xlim = range(x)
  }
  if (equal.axes) xlim = ylim = range(c(xlim,ylim))
  if (is.numeric(ylimm) & length(ylimm)==2) { ylim = ylimm } #overwrite if
  if (is.numeric(xlimm) & length(xlimm)==2) { xlim = xlimm }

  plot(df_2columns, ..., main = plotname, col = col, pch = pch, ylim = ylim, xlim = xlim)
  if (errorbar) {
    arrows(x0 = x, y0 = y + upper, x1 = x, y1 = y - lower, angle = 90, code = 3, length = width, lwd = arrow_lwd, col = col_errorbar)
    arrows(x0 = x + left, y0 = y, x1 = x - right, y1 = y, angle = 90, code = 3, length = width, lwd = arrow_lwd, col = col_errorbar)
  }
  if (abline == "h") {	abline(h = a, lty = lty, lwd = lwd, col = col_abline)	}
  if (abline == "v") {	abline(v = a, lty = lty, lwd = lwd, col = col_abline)	}
  if (abline == "ab") {	abline(a = a, b = b, lty = lty, lwd = lwd, col = col_abline)	}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname)) }
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}



#' wscatter.fill
#'
#' A scatterplot with color gradient and color legend. Modified from: http://stackoverflow.com/questions/20127282/r-color-scatterplot-points-by-col-value-with-legend
#' @param x X variable
#' @param y Y variable
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param color Filling color of the symbols
#' @param xlim Manually set the range of canvas in X dimension
#' @param ylimManually set the range of canvas in Y dimension
#' @param zlim  Manually set the range of colors numbers (Z dimension)
#' @param nlevels Number of steps in the color gradient
#' @param pch Define the symbol for each data point. A number [0-25] or any string between ""-s.
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
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not fit on the plot.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples wscatter.fill(x=rnorm(100), y=rnorm(100), color=rnorm(100), nlevels=15, pch = 21, xlab="The X Dimension. Wooaaahh")

wscatter.fill <- function (df2col = cbind("A"=rnorm(100), "B"=rnorm(100)), ..., color, xlim=range(df2col[, 1]), ylim=range(df2col[, 2]), zlim=range(color), nlevels = 20, pch=21, cex=1,
                           plotname = substitute(df2col), plot.title = plotname,
                           plot.axes, key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1,
                           axes = TRUE, frame.plot = axes, xlb, ylb,
                           savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, incrBottMarginBy = 0, mdlink = ww.set.mdlink() ) {
  x = df2col[, 1]
  y = df2col[, 2]
  CNN = colnames(df2col)
  xlb = if(length(CNN) & missing(xlb)) CNN[1]
  ylb = if(length(CNN) & missing(ylb)) CNN[2]

  fname = kollapse(plotname, ".barplot")
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin

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
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", yaxs = "i")

  rect(0, levels[-length(levels)], 1, levels[-1L], col=col, border=col)
  if (missing(key.axes)) { if (axes){axis(4)} }
  else key.axes
  box()
  if (!missing(key.title)) key.title
  mar <- mar.orig
  mar[4L] <- 1
  par(mar = mar)

  # points
  plot(x, y, main =plot.title, type = "n", xaxt='n', yaxt='n', ..., xlim=xlim, ylim=ylim, bty="n", xlab=xlb, ylab=ylb)
  points(x, y, bg = colz, xaxt='n', yaxt='n', xlab="", ylab="", bty="n", pch=pch, ...)

  ## options to make mapping more customizable
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) box()
  if (missing(plot.title)) title(...)
  else plot.title
  invisible()

  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname)) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname)	}
}


#' wbarplot
#'
#' Create and save bar plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.
#' @param variable The variable to plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param col Color of the plot.
#' @param sub Subtitle below the plot.
#' @param plotname The name of the file saved.
#' @param main The title of the plot.
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param ylimits Defines the Y axis range. Replacement for the standard "ylim" argument.
#' @param hline Draw a horizontal line at the value you pass on to it. Useful to display a threshold. Design the line by "lty", "lwd" & "lcol" parameters.
#' @param vline Draw a vertical line at the value you pass on to it. Useful to display a threshold. Design the line by "lty", "lwd" & "lcol" parameters.
#' @param filtercol Color bars below / above the threshold with red / green. Define the direction by -1 or 1. Takes effect if "hline" is defined.
#' @param lty Linetype, defined by numbers 1-6.
#' @param lwd Linewidth. Set to 2 by default.
#' @param lcol Color of the line.
#' @param errorbar Draw error bars if TRUE. Pass on the value in parameters "upper" and "lower". Refine the look by "w" and "arrow_lwd".
#' @param upper Size of the upper error bar.
#' @param lower Size of the lower error bar. By default, it equals the upper error bar.
#' @param arrow_width Width of the arrow head.
#' @param arrow_lwd Line width for the error bars.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not fit on the plot.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples wbarplot (variable =  , ... =  , col = gold1, sub = F, plotname = substitute(variable), main = substitute(variable), w = 7, h = w, incrBottMarginBy = 0, mdlink = F, tilted_text = F, hline = F, vline = F, filtercol = 1, lty = 1, lwd = 2, lcol = 2, errorbar = F, upper = 0, lower = upper, arrow_width = 0.1, arrow_lwd = 1)

wbarplot <- function (variable, ..., col = "gold1", sub = F, plotname = substitute(variable), main = plotname, tilted_text = F, ylimits = NULL,
                     hline = F, vline = F, filtercol = 1, lty = 1, lwd = 2, lcol = 2,
                     errorbar = F, upper = 0, lower = upper, arrow_width = 0.1, arrow_lwd = 1,
                     savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, incrBottMarginBy = 0, mdlink = ww.set.mdlink()) {
  isVec = is.vector(variable) | is.table(variable)
  isMat = is.matrix(variable) | is.data.frame(variable)
  NrBars = if (isVec) l(variable) else if ( isMat ) ncol(variable) else l(variable)
  BarNames = if (isVec) names(variable) else if ( isMat ) colnames(variable) else names(variable)

  fname = kollapse(plotname, ".barplot")
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  cexNsize = 0.8/abs(log10(length(variable)))
  cexNsize = min(cexNsize, 1)
  if (sub == T) {	subtitle = paste("mean:", iround(mean(variable, na.rm = T)), "CV:", percentage_formatter(cv(variable)))	} else if (sub == F) { subtitle = "" } else { subtitle = sub }
  if (hline & filtercol == 1) {  col = (variable >= hline) + 2	}
  if (hline & filtercol == -1) { col = (variable <  hline) + 2	}
  if (errorbar & is.null(ylimits)) {	ylimits = range(c(0, (variable + upper + abs(0.1 * variable)), variable - lower - abs(0.1 * variable)), na.rm = T) } # else {	ylimits = range(0, variable)	}
  if (tilted_text) {	xlb = rep(NA, NrBars)	}	else {		xlb = BarNames	}

  x = barplot(variable, ylim = ylimits, ..., names.arg = xlb, main = main, sub = subtitle, col = col, las = 2, cex.names = cexNsize)
  if (hline) { abline(h = hline, lty = lty, lwd = lwd, col = lcol)	}
  if (vline[1]) { abline(v = x[vline], lty = lty, lwd = lwd, col = lcol)	}
  if (errorbar) {	arrows(x, variable + upper, x, variable - lower, angle = 90, code = 3, length = arrow_width, lwd = arrow_lwd, ...)	}
  if (tilted_text) {
    text(x = x - 0.25, y = 0, labels = BarNames, xpd = TRUE, srt = 45, cex = cexNsize, adj = c(1, 3))
  }

  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname)) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}



#' whist
#'
#' Create and save histograms as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions. Name the file by naming the variable! Cannot be used with dynamically called variables [e.g. call vectors within a loop]. "filtercol" assumes  >= coloring!
#' @param variable The variable to plot.
#' @param breaks Number of bins.
#' @param col Color of the plot.
#' @param plotname The name of the file.
#' @param main Title of the plot.
#' @param xlb X-axis label.
#' @param vline Draw a vertical line at the value you pass on to it. Useful to display a threshold. Design the line by "lty", "lwd" & "lcol" parameters.
#' @param lty Linetype, defined by numbers 1-6.
#' @param lwd Line width. Set to 2 by default.
#' @param lcol Color of the line.
#' @param filtercol Color bars below / above the threshold with red / green. Define the direction by -1 or 1. Takes effect if "vline" is defined.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @export
#' @examples whist (variable =  , col = gold1, w = 7, h = w, plotname = substitute(variable), breaks = 20, main = kollapse("Histogram of ", substitute(variable)), xlb = substitute(variable), mdlink = F, hline = F, vline = F, lty = 2, lwd = 3, lcol = 2, filtercol = 0, ... =  )

whist <- function (variable, breaks = 20, col = "gold1", plotname = substitute(variable), main = kollapse("Histogram of ", substitute(variable)), xlb = substitute(variable),
                  hline = F, vline = F, lty = 2, lwd = 3, lcol = 1, filtercol = 0,
                  savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink(), ...) {
  xtra = list(...)
  if (length(variable) > 0) {
    fname = kollapse(plotname, ".hist")
    if (!is.numeric(variable)) {
      variable = table(variable)
      cexNsize = 0.7/abs(log10(length(variable)))
      cexNsize = min(cexNsize, 1)
      barplot(variable, ..., main = main, xlab = xlb, col = col, las = 2, cex.names = cexNsize,
              sub = paste("mean:", iround(mean(variable, na.rm = T)), "CV:", percentage_formatter(cv(variable))))
    } else {
      histdata = hist(variable, breaks = breaks, plot = F)
      BRK = histdata$breaks
      NrThr = length(vline)
      if (filtercol == 1  & NrThr ==1) { col =(BRK >= vline) + 2
      } else if (filtercol == 1  & NrThr ==2) { col =(BRK >= vline[1] & BRK < vline[2]) + 2
      } else if (filtercol == -1 & NrThr ==1) { col =(BRK < vline) + 2
      } else if (filtercol == -1 & NrThr ==2) { col =(BRK < vline[1] | BRK >= vline[2]) + 2 }
      hist(variable, ..., main = main, breaks = breaks, xlab = xlb, col = col, las = 2)
    }
    # if (hline) { abline(h = hline, lty = lty, lwd = lwd, col = lcol) }
    if (!missing(vline) & !length(xtra$xlim)) { PozOfvline = NULL;
    for (l in 1:length(vline)) {
      PozOfvline[l] = mean(histdata$mids[c(max(which(BRK < vline[l])), min(which(BRK >= vline[l])))])
    }
    abline(v = PozOfvline, lty = lty, lwd = lwd, col = lcol)
    }
    else if (vline & length(xtra$xlim)) { abline(v = vline, lty = lty, lwd = lwd, col = 1)	}
    if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname)) }
  } else { iprint(variable, " IS EMPTY")	}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}

#' whist.back2back
#'
#' Two back-to-back histograms from a list. The X-axis is only correct if  breaks1 ==breaks2. Undeveloped function, contains graphical bugs, no support for this function.
#' @param ListOf2 List of 2 numeric vectors
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param breaks1 break parameter for histogram function for the 2st list element.
#' @param breaks2 break parameter for histogram function for the 2st list element.
#' @param colorz  Color of the 2 histograms
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param plotname The name of the file saved.
#' @param main_ The title of the plot.
#' @param ylab Y-axis label
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not fit on the plot.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples whist.back2back(ListOf2 = list("A"  = rnorm(100), "B"=rnorm(100)))

whist.back2back <- function(ListOf2 = list("A"  = rnorm(100), "B"=rnorm(100)), breaks1 = 20, breaks2 = breaks1, colorz = c("green", "blue"), ...,
                           plotname = substitute(variable), main_ = plotname, ylab ="Frequency",
                           savefile = UnlessSpec("b.save.wplots"), incrBottMarginBy = 0, w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink()) {

  fname = kollapse(plotname, ".hist.btb")
  lsNm = if (!is.null(names(ListOf2))) names(ListOf2)  else 1:2

  lng = length(ListOf2)
  if (lng != 2) { iprint("length(List): ", lng, " First two elements used" ) } #if
  h1 = hist(ListOf2[[1]], plot=FALSE, breaks = breaks1)
  h2 = hist(ListOf2[[2]], plot=FALSE, breaks = breaks2)
  h2$counts = - h2$counts
  hmax = max(h1$counts, na.rm =T)
  hmin = min(h2$counts, na.rm =T)
  xlimm =range(unlist(ListOf2), na.rm =T)
  xlimm = c(1, max(l(h2$counts), l(h1$counts))+3)

  print(xlimm)
  X = c(h1$breaks, h2$breaks)
  barplot(h1$counts, ylim=c(hmin, hmax), xlim = xlimm, col=colorz[1], names.arg =h1$breaks[-1], las=3 , main = main_, ylab=ylab, ...)
  barplot(h2$counts, col=colorz[2], add=T)
  condition = F

  legend("topright", lsNm[1], bty="n")
  legend("bottomright", lsNm[2], bty="n")

  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname)) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname)	}
}


#' wboxplot
#'
#' Create and save box plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.
#' @param yalist The variable to plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param col Color of the plot.
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not fit on the plot.
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples wboxplot (variable =  , ... =  , col = gold1, plotname = as.character(substitute(variable)), sub = FALSE, incrBottMarginBy = 0, tilted_text = F, w = 7, h = w, mdlink = F)

wboxplot <- function (yalist, ..., col = "gold1", plotname = as.character(substitute(yalist)), ylb="", sub = FALSE, incrBottMarginBy = 0, 	tilted_text = F,
                     savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink()) {
  fname = kollapse(plotname, ".boxplot")
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  if (tilted_text) { 	xlb = NA } else {	xlb = names(yalist) }
  boxplot(yalist, ..., names = xlb, main = plotname, col = col, las = 2)
  mtext(ylb, side = 2, line = 2)
  if (tilted_text) {
    text(x = 1:length(yalist), y = min(unlist(yalist), na.rm = T)-(max(nchar(names(yalist)))/2), labels = names(yalist), xpd = TRUE, srt = 45)
  }
  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname)) }
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}


#' wpie
#'
#' Create and save pie charts as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.
#' @param variable The variable to plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param percentage Display percentage instead of counts. TRUE by default.
#' @param both_pc_and_value Report both percentage AND number.
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param col Fill color. Defined by rich colours by default
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples wpie (variable =  , ... =  , percentage = TRUE, plotname = substitute(variable), w = 7, h = w, mdlink = F)

wpie <- function (variable, ..., percentage = TRUE, both_pc_and_value=F, plotname = substitute(variable), col = gplots::rich.colors(length(variable)), savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink()) {
  if (!require("gplots")) { print("Please install gplots: install.packages('gplots')") }
  fname = kollapse(plotname, ".pie")
	subt = kollapse("Total = ", sum(variable), print = F)
	if (percentage) {	labs <- paste("(", names(variable), ")", "\n", percentage_formatter(variable/sum(variable)), sep = "")
	if (both_pc_and_value) { labs <- paste("(", names(variable), ")", "\n", percentage_formatter(variable/sum(variable)), "\n", variable , sep = "")}
	} else {	labs <- paste("(", names(variable), ")", "\n", variable, sep = "")	}
	pie(variable, ..., main = plotname, sub = subt, clockwise = T, labels = labs, col = col )
	if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname)) }
	if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}


#' wstripchart
#'
#' Create and save strip charts as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.
#' @param yalist Input list to be plotted.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param border An optional vector of colors for the outlines of the boxplots. The values in border are recycled if the length of border is less than the number of plots.
#' @param BoxPlotWithMean Display the mean instead of the median in a boxplot. This is non-standard use of a boxplot, report it.
#' @param pch Define the symbol for each data point. A number [0-25] or any string between ""-s.
#' @param pchlwd Define the outline width of the symbol for each data point.
#' @param pchcex Define the size of the symbol for each data point.
#' @param bg Background color.
#' @param col Color of the plot.
#' @param metod Method for displaying data points to avoid overlap; either"jitter" or "stack". See stripchart().
#' @param jitter The amount of horizontal scatter added to the individual data points (to avoid overlaps).
#' @param col Color of the plot.orbyColumn
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not fit on the plot.
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples wstripchart (yalist =  , ... =  , plotname = as.character(substitute(yalist)), sub = FALSE, border = 1, BoxPlotWithMean = F, pch = 23, pchlwd = 1, pchcex = 1.5, bg = chartreuse2, col = black, metod = jitter, jitter = 0.2, colorbyColumn = F, w = 7, h = w, incrBottMarginBy = 0, tilted_text = F, mdlink = F)

wstripchart <- function (yalist, ..., plotname = as.character(substitute(yalist)), sub = NULL,
                        border = 1, incrBottMarginBy = 0, tilted_text = F, BoxPlotWithMean = F, metod = "jitter", jitter = 0.3,
                        pch = 18, pchlwd = 1, cex.lab=1, pchcex = 1.5, bg = "seagreen2", colorbyColumn = T, col = if(colorbyColumn) 1:l(yalist) else 1, ylb="",
                        savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink()) {
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  cexNsize = 1/abs(log10(length(yalist)))
  cexNsize = min(cexNsize, 1)
  fname = kollapse(plotname, ".stripchart")
  a = boxplot(yalist, plot = F)
  if (colorbyColumn) {bg=NULL }
  if (BoxPlotWithMean) {	a$stats[3, ] = unlist(lapply(yalist, mean))	}
  if (tilted_text) {	xlb = F } else { xlb = T }
  bxp(a, xlab = "", show.names = xlb, ..., main = plotname, sub = sub, border = border, outpch = NA, las = 2,
      outline = T, cex.axis = cexNsize, ylab=NA)
  stripchart(yalist, vertical = TRUE, add = TRUE, method = metod, jitter = jitter, pch = pch, bg = bg,
             col = col, lwd = pchlwd, cex = pchcex)
  mtext(ylb, side = 2, line = 2, cex = cex.lab)
  if (tilted_text) {
    xx= min(unlist(yalist), na.rm = T)
    text(x = 1:length(yalist), y=xx, labels = names(yalist), xpd = TRUE, srt = 45, adj = c(1, 3))
  }
  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname) ) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}



#' wstripchart_list
#'
#' Create and save stripcharts from a list as .pdf, in "OutDir". This version allows individual coloring of each data point, by a color-list of the same dimension. If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.
#' @param yalist Input list to be plotted.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param ylb Y-axis label.
#' @param xlb X-axis label.
#' @param border An optional vector of colors for the outlines of the boxplots. The values in border are recycled if the length of border is less than the number of plots.
#' @param bxpcol Color of the boxplot outlines.
#' @param pch Define the symbol for each data point. A number [0-25] or any string between ""-s.
#' @param pchlwd Define the outline width of the symbol for each data point.
#' @param pchcex Define the size of the symbol for each data point.
#' @param bg Background color.
#' @param col Color of the plot.l
#' @param metod Method for displaying data points to avoid overlap; either"jitter" or "stack". See stripchart().
#' @param jitter The amount of horizontal scatter added to the individual data points (to avoid overlaps).
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not fit on the plot.
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples wstripchart_list (yalist =  , ... =  , plotname = as.character(substitute(yalist)), sub = FALSE, ylb = NULL, xlab = NULL, border = 1, bxpcol = 0, pch = 23, pchlwd = 1, pchcex = 1.5, bg = chartreuse2, coll = black, metod = jitter, jitter = 0.2, w = 7, h = w, incrBottMarginBy = 0, tilted_text = F, mdlink = F)

wstripchart_list <- function ( yalist, ..., 	border = 1, bxpcol = 0, pch = 18, pchlwd = 1, pchcex = 1.5, bg = "chartreuse2", coll = "black", metod = "jitter", jitter = 0.2,
                              plotname = as.character(substitute(yalist)), sub = NULL, ylb = "", xlab = "", incrBottMarginBy = 0, tilted_text = F,
                              savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink()) {
  fname = kollapse(plotname, ".stripchart")
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  cexNsize = 1/abs(log10(length(list)))
  cexNsize = min(cexNsize, 1)
  if (tilted_text) {	xlb = F	} else {	xlb = T	}

  boxplot(yalist, ..., show.names = xlb, main = plotname, sub = sub, border = border, outpch = NA, las = 2, ylab =NA,
          col = bxpcol, cex.axis = cexNsize)
  mtext(ylb, side = 2, line = 2)
  for (i in 1:length(yalist)) {
    if( l(na.omit.strip(yalist[[i]])) ){
      j = k = i
      if (length(coll) < length(yalist)) { j = 1 }
      if (length(bg) < length(yalist)) {	k = 1	}
      stripchart(na.omit(yalist[[i]]), at = i, add = T, vertical = T, method = metod, jitter = jitter,
                 pch = pch, bg = bg[[k]], col = coll[[j]], lwd = pchlwd, cex = pchcex)
    }
  } # for
  if (tilted_text) {
    xx= min(unlist(yalist), na.rm = T)
    # yy = (max(nchar(names(yalist)))/2)
    text(x = 1:length(yalist), y = xx, labels = names(yalist), xpd = TRUE, srt = 45, adj = c(1, 3))
  }
  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname)) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}


#' wvioplot_list
#'
#' Create and save violin plots as .pdf, in "OutDir". It requires (and calls) "vioplot" package. If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.
#' @param yalist Input list to be plotted.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param col Color of the plot.l
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not fit on the plot.
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param xlb X-axis label.
#' @param ylb Y-axis label.
#' @param tilted_text Use 45 degree x-labels if TRUE. Useful for long, but not too many labels.
#' @param tilted_text Manual tuning of the Y-postion of the tilted text labels
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param ylimm Manual y axis limits
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples wvioplot_list (yalist =  , ... =  , xlb = names(yalist), ylb =  , coll = c(1:length(yalist)), incrBottMarginBy = 0, w = 7, h = w, plotname = as.character(substitute(yalist)), tilted_text = F, mdlink = F)

wvioplot_list <- function (yalist, ..., coll = c(2:(length(yalist)+1)),
                          plotname = as.character(substitute(yalist)), sub = NULL, xlb = names(yalist), ylb = "", ylimm=F,
                          incrBottMarginBy = 0, tilted_text = F, yoffset=0, savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink()) {
  if (!require("vioplot")) { print("Please install vioplot: install.packages('vioplot')") }
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  l_list = length(yalist)
  fname = kollapse(plotname, ".vioplot")
  if (length(coll) < l_list) { coll = rep(coll, l_list) }
  if (tilted_text) {	xlb = NA } else { xlb = names(yalist) }
  if (! (is.numeric(ylimm) & length(ylimm)==2)) { ylimm = range(unlist(yalist), na.rm = T)}
  plot(0, 0, type = "n", xlim = c(0.5, (l_list + 0.5)), ylim = ylimm, xaxt = "n", xlab = "",
       ylab = ylb, main = plotname, sub = sub)
  for (i in 1:l_list) {
    if( l(na.omit.strip(yalist[[i]])) ){
      vioplot::vioplot(na.omit(yalist[[i]]), ..., at = i, add = T, col = coll[i])
    }
  }
  axis(side = 1, at = 1:l_list, labels = xlb, las = 2)
  if (tilted_text) {
    text(x = 1:length(yalist), y = min(unlist(yalist))+yoffset, labels = names(yalist), xpd = TRUE, srt = 45)
  }
  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname) ) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}



#' wviostripchart_list
#'
#' Create and save violin plots as .pdf, in "OutDir". It requires (and calls) "vioplot" package. If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.
#' @param yalist Input list to be plotted.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param pch Define the symbol for each data point. A number [0-25] or any string between ""-s.
#' @param viocoll Background color of each individual violing plot.
#' @param vioborder Border color of each individual violing plot.
#' @param bg Background color.
#' @param col Color of the plot.l
#' @param metod Method for displaying data points to avoid overlap; either"jitter" or "stack". See stripchart().
#' @param jitter The amount of horizontal scatter added to the individual data points (to avoid overlaps).
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param sub Subtitle below the plot.
#' @param ylb Y-axis label.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not fit on the plot.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @export
#' @examples wviostripchart_list (yalist =  , ... =  , pch = 23, viocoll = 0, vioborder = 1, ylb =  , plotname = as.character(substitute(yalist)), sub = F, bg = 0, coll = black, metod = jitter, jitter = 0.1, w = 7, h = w, incrBottMarginBy = 0, mdlink = F)

wviostripchart_list <- function (yalist, ..., pch = 23, viocoll = 0, vioborder = 1, bg = 0, coll = "black", metod = "jitter", jitter = 0.1,
                                plotname = as.character(substitute(yalist)), sub = NULL, ylb = "", incrBottMarginBy = 0,
                                savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink()) {
  fname = kollapse(plotname, ".VioStripchart")
  if (!require("vioplot")) { print("Please install vioplot: install.packages('vioplot')") }
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  l_list = length(yalist)
  plot(0, 0, type = "n", xlim = c(0.5, (l_list + 0.5)), ylim = range(unlist(yalist), na.rm = T), xaxt = "n", xlab = "",
       ylab = ylb, main = plotname, sub = sub)
  for (i in 1:l_list) {
    print(i)
    if( l(na.omit.strip(yalist[[i]])) ){
      vioplot::vioplot(na.omit(yalist[[i]]), ..., at = i, add = T, col = viocoll[i], border = 1)
    } #if
  }
  for (i in 1:length(yalist)) {
    if( l(na.omit.strip(yalist[[i]])) ){
      j = k = i
      if (length(coll) < length(yalist)) {	j = 1	}
      if (length(bg) < length(yalist)) { k = 1 }
      stripchart(na.omit(yalist[[i]]), at = i, add = T, vertical = T, method = metod, jitter = jitter,
                 pch = pch, bg = bg[[k]], col = coll[[j]])
    } #if
  }
  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "pdf"), width = w, height = h, title = ww.ttl_field(fname)) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink & savefile) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}


#' wvenn
#'
#' Save venn diagrams. Unlike other ~vplot funcitons, this saves directly into a .png, and it does not use the dev.copy2pdf() function.
#' @param yalist The variable to plot.
#' @param imagetype Image format, png by default.
#' @param alpha Transparency, .5 by default.
#' @param fill Background color vec
#' @param subt Subtitle
#' @param ... Pass any other parameter of the corresponding venn.diagram() function (most of them should work).
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param plotname Manual plotname parameter
#' @param LogFile Allow logfiles.
#' @export
#' @examples wvenn (yalist =  , imagetype = png, alpha = 0.5, ... =  , w = 7, h = w, mdlink = F)

wvenn <- function (yalist, imagetype = "png", alpha = .5, fill = 1:length(yalist), subt, ..., w = UnlessSpec("b.defSize", 7), h = w, mdlink = ww.set.mdlink(), plotname = substitute(yalist), LogFile=F) {
  if (!require("VennDiagram")) { print("Please install VennDiagram: install.packages('VennDiagram')") }
  fname = kollapse(plotname, ".", imagetype, print = F)
  LsLen = length(yalist)
  if(length(names(yalist)) < LsLen) { names(yalist) =1:LsLen; print("List elements had no names.") }
  print(names(yalist))

  filename = kollapse(OutDir, "/", fname, print = F)
  if (missing(subt)) { subt = kollapse("Total = ", length(unique(unlist(yalist))), " elements in total.", print = F)  } #if
  if (!LogFile) futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger") # suppress unless wamted
  venn.diagram(x = yalist, imagetype = imagetype, filename = filename, main = plotname, ... ,
               sub = subt, fill = fill, alpha = alpha, sub.cex = .75, main.cex = 2)
  if (mdlink) {
    llogit(ww.MarkDown_ImgLink_formatter(fname))
    if (b.usepng == T && b.png4Github == T) { llogit(ww.MarkDown_ImgLink_formatter(paste0("Reports/", fname) ) )	}
  }
}


# Plots for cycling over data frame columns or rows ------------------------------------------------------------------------------------------

#' wbarplot_dfCol
#'
#' wbarplot for a column of a data frame.
#' @param df Input data frame to be plotted
#' @param col Color of the plot.Name
#' @param col Color of the plot.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @export
#' @examples wbarplot_dfCol (df =  , colName =  , col = gold1, w = 7, h = w, ... =  )

wbarplot_dfCol <- function (df, colName, col = "gold1", savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w, ...) {
  stopifnot(colName %in% colnames(df))
  variable = unlist(df[, colName])
  stopifnot(length(variable) > 1)
  plotname = paste(substitute(df), "__", colName, sep = "")
  FullPath = ww.FnP_parser(plotname, "barplot.pdf")
  cexNsize = 0.7/abs(log10(length(variable)))
  cexNsize = min(cexNsize, 1)
  barplot(variable, ..., main = plotname, col = col, las = 2, cex.names = cexNsize, sub = paste("mean:",
                                                                                                iround(mean(variable, na.rm = T)), "CV:", percentage_formatter(cv(variable))))
  if (savefile) { dev.copy2pdf(file = FullPath, width = w, height = h, title = ww.ttl_field(fname))	}
}

#' whist_dfCol
#'
#' Use this version of whist() if you iterate over columns  or rows of a data frame. You can name the file by naming the variable. Cannot be used with dynamically called variables [e.g. call vectors within a loop]
#' @param df Input data frame to be plotted
#' @param col Color of the plot.Name
#' @param col Color of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @export
#' @examples whist_dfCol (df =  , colName =  , col = gold, ... =  , w = 7, h = w)

whist_dfCol <- function (df, colName, col = "gold", ..., savefile = UnlessSpec("b.save.wplots"), w = UnlessSpec("b.defSize", 7), h = w) {
  stopifnot(colName %in% colnames(df))
  variable = as.vector(unlist(df[, colName]))
  stopifnot(length(variable) > 1)
  plotname = paste(substitute(df), "__", colName, sep = "")
  fname = ww.FnP_parser(plotname, "hist.pdf")
  if (!is.numeric(variable)) {
    table_of_var = table(variable)
    cexNsize = 0.7/abs(log10(length(table_of_var)))
    cexNsize = min(cexNsize, 1)
    barplot(table_of_var, ..., main = plotname, col = col, las = 2, cex.names = cexNsize,
            sub = paste("mean:", iround(mean(table_of_var, na.rm = T)),
                        "| median:", iround(mean(table_of_var, na.rm = T)),
                        "| mode:", iround(modus(table_of_var, na.rm = T)),
                        "| CV:", percentage_formatter(cv(table_of_var))))
  }
  else {
    zz = hist(variable, ..., plot = F)
    hist(variable, ..., main = plotname, col = col, las = 2, sub = paste("mean:", iround(mean(variable)),
                                                                         "| median:", iround(median(variable)),
                                                                         "| modus:", iround(modus(variable)))    )
  }
  if (savefile) { dev.copy2pdf(file = fname, width = w, height = h, title = ww.ttl_field(fname)) }
}

# A4 pdfs for multi-plots -------------------------------------------------------------------------------------------------

#' pdfA4plot_on
#'
#' Create A4 PDFs to plot multiple subplots in one file
#' @param pname Title of the plot (main parameter) and also the name of the file.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param w Width of the saved pdf image, in inches. c("A4" = 8.27, "1col.nature" = 3.50, "2col.nature" = 7.20, "1col.cell" = 3.35, "1.5col.cell" = 4.49, "2col.cell" = 6.85).
#' @param h Height of the saved pdf image, in inches.
#' @param rows Number of rows for subplots
#' @param cols Number of columns for subplots
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param title Manually set the title field of the PDF file
#' @export
#' @examples pdfA4plot_on();  hist(rnorm(100)); hist(-rnorm(100)); hist(10+rnorm(100)); pdfA4plot_off()

pdfA4plot_on <- function (pname = date(), ..., w = UnlessSpec("b.defSize.fullpage", 8.27), h = 11.69, rows = 4, cols = rows-1, mdlink = ww.set.mdlink(),
                         title = ww.ttl_field(pname)) { # Print (multiple) plots to an (A4) pdf.
  fname = ww.FnP_parser(pname, "pdf")
  try.dev.off()
  assign("b.mfrow_def", par("mfrow"), fname, envir = .GlobalEnv)
  assign("b.bg_def", par("bg"), fname, envir = .GlobalEnv)
  assign("b.save.wplots", F, envir = .GlobalEnv) # switch of "savefile" option
  pdf(fname, width=w, height=h, title = title)
  par(mfrow = c(rows, cols), bg ="white")
  iprint(" ----  Don't forget to call the pair of this function to finish plotting in the A4 pdf.: pdfA4plot_off ()")
  if (mdlink) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = pname) }
}


#' pdfA4plot_on.layout
#'
#' Create A4 PDFs to plot multiple subplots in one file with custom numbers of columns in each row
#' @param pname Title of the plot (main parameter) and also the name of the file.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param layout_mat A matrix of plot layout. Default: rbind(1, c(2, 3), 4:5)
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param title Manually set the title field of the PDF file
#' @export
#' @examples pdfA4plot_on.layout();  hist(rnorm(100)); hist(-rnorm(100)); hist(10+rnorm(100)); pdfA4plot_off()

pdfA4plot_on.layout <- function (pname = date(), ..., w = UnlessSpec("b.defSize.fullpage", 8.27), h = 11.69, layout_mat = rbind(1, c(2, 3), 4:5), mdlink = ww.set.mdlink(),
                                title = ww.ttl_field(pname)) { # Fancy layout version. Print (multiple) plots to an (A4) pdf.
  fname = ww.FnP_parser(pname, "pdf")
  try.dev.off()
  assign("b.bg_def", par("bg"), fname, envir = .GlobalEnv)
  assign("b.save.wplots", F, envir = .GlobalEnv) # switch of "savefile" option
  pdf(fname, width=w, height=h, title = title)
  layout(layout_mat)
  # par(mar = c(3, 3, 0, 0))
  print(layout_mat)
  iprint(" ----  Don't forget to call the pair of this function to finish plotting in the A4 pdf.: pdfA4plot_off ()")
  if (mdlink) { ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = pname) }
}


#' pdfA4plot_off
#'
#' pair of the "pdfA4plot_on()" function; to finish plotting in the A4 pdf.
#' @export
#' @examples pdfA4plot_on.layout();  hist(rnorm(100)); hist(-rnorm(100)); hist(10+rnorm(100)); pdfA4plot_off()

pdfA4plot_off <- function () {
  x =  if (exists("b.mfrow_def")) b.mfrow_def else c(1, 1)
  y =  if (exists("b.bg_def")) b.bg_def else "white"
  if (exists("b.save.wplots")) assign("b.save.wplots", T, envir = .GlobalEnv) # switch back mdlink to its original value
  par(mfrow = x, bg = y)
  try(dev.off()) # close pdf
  if(exists("OutDir")) {oo()}
}



# Add-ons to exisiting plots -------------------------------------------------------------------------------------------------

#' error_bar
#'
#' Put error bars on top of your bar plots. This functionality is now integrated into MarkdownReporter's wbarplot() function
#' @param x X-position on the plot.
#' @param y Y-position on the plot.
#' @param upper Size of the upper error bar.
#' @param lower Size of the lower error bar. By default, it equals the upper error bar.
#' @param width Width of the erro bar whisker.
#' @param ... Pass any other parameter of the corresponding arrows function (most of them should work).
#' @export
#' @examples error_bar (x =  , y =  , upper =  , lower = upper, width  = 0.1, ... =  )

error_bar <- function (x, y, upper, lower = upper, width  = 0.1, ...) {
  stopifnot(length(x) == length(y) & length(y) == length(lower) & length(lower) == length(upper))
  if (length(dim(y)) > 1) {
    arrows(as.vector(x), as.vector(y + upper), as.vector(x), as.vector(y - lower), angle = 90, code = 3,
           length = width , ...)
  }
  else {
    arrows(x, y + upper, x, y - lower, angle = 90, code = 3, length = width , ...)
  }
}

#' wlegend
#'
#' Quickly add a legend to an existing plot, and save the plot immediately. Never inserts an mdlink.
#' @param fill_ Color of the boxes next to the text
#' @param poz Position of the legend (def: 4). Use numbers 1-4 to choose from "topleft", "topright", "bottomright", "bottomleft".
#' @param legend Labels displayed (Text)
#' @param ... Additional parameters for legend()
#' @param w_ Width of the saved pdf image, in inches.
#' @param h_ Height of the saved pdf image, in inches.
#' @param bty The type of box to be drawn around the legend. The allowed values are "o" (the default) and "n".
#' @param OverwritePrevPDF Save the plot immediately with the same name the last wplot* function made (It is stored in plotnameLastPlot variable).
#' @export
#' @examples function(fill_ = NULL, poz=4, legend = names(fill_), ..., w_=7, h_=w_, bty = "n", OverwritePrevPDF =T)

wlegend <- function(fill_ = NA, poz=4, legend, bty = "n", ..., w_=7, h_=w_, title=NULL, OverwritePrevPDF = UnlessSpec("b.save.wplots")) { # Add a legend, and save the plot immediately
  fNames = names(fill_)
  LF = length(fill_)
  LN = length(fNames)
  stopif( ( LN != LF & missing(legend) ), message = "The color vector (fill_) has less names than entries / the variable 'legend' is not provided.")
  # stopif( ( LF  != length(legend)), message = "Fill and legend are not equally long.")
  legend = if( LN == LF & missing(legend) ) fNames else legend
  pozz = translate(poz, oldvalues = 1:4, newvalues = c("topleft", "topright", "bottomright", "bottomleft"))
  legend(x=pozz, legend=legend, fill=fill_, title=title, ..., bty=bty)
  if (OverwritePrevPDF) {   wplot_save_this(plotname = plotnameLastPlot, w= w_, h = h_, mdlink = F)  }
}

#' barplot_label
#'
#' Add extra labels to your bar plots at the top or the base.
#' @param barplotted_variable The variable that you barplotted previously.
#' @param labels Label text.
#' @param bottom Put labels at the bottom of the bars.
#' @param TopOffset Absolute offset from top.
#' @param relpos_bottom Relative offset from bottom.
#' @param OverwritePrevPDF Save the plot immediately with the same name the last wplot* function made (It is stored in plotnameLastPlot variable). Never inserts an mdlink.
#' @param ... Pass any other parameter of the corresponding text function (most of them should work).
#' @export
#' @examples barplot (1:10); barplot_label (1:10, labels =11:2  , bottom = F, TopOffset = 0.5, relpos_bottom = 0.1, ... =  )

barplot_label <- function (barplotted_variable, labels, bottom = F, TopOffset = .5, relpos_bottom = 0.1, OverwritePrevPDF =UnlessSpec("b.save.wplots"), ...) {
  x = barplot(barplotted_variable, plot = F)
  y = barplotted_variable
  # stopifnot(length(x) == length(y))
  if (bottom) {
    y = rep(relpos_bottom * max(y, na.rm = T), length(x))
  }
  if (length(dim(x)) > 1) {
    text(as.vector(x), as.vector(y -TopOffset), labels = as.vector(labels), ...)
  }
  else if (length(dim(x)) == 1) {
    text((x), (y), labels = (labels), ...)
  }
  if (OverwritePrevPDF) {   wplot_save_this(plotname = plotnameLastPlot, mdlink = F, ...)  }
}


#'wLinRegression
#'
#' Add linear regression, and descriptors to line to your scatter plot. Provide the same dataframe as you provided to wplot() before you called this function
#' @param DF  The same dataframe as you provided to wplot() before you called this function
#' @param coeff What coefficient to display? Either "all", "pearson", "spearman" correlation values or "r2" for the Coefficient of Determination.
#' @param textlocation where to put the legend?
#' @param cexx font size; 1 by default
#' @param savefile Shall it call wplot_save_this(plotname = plotnameLastPlot) ?
#' @param ...  Additional parameters for the line to display.
#' @export
#' @examples x = cbind(a=rnorm(1:10), b=rnorm(10)); wplot(x); wLinRegression(x, coeff = c("pearson", "spearman", "r2"))

wLinRegression <- function(DF, coeff = c("pearson", "spearman", "r2")[3], textlocation = "topleft", savefile =T, cexx =1, ...) { # Add linear regression, and descriptors to line to your scatter plot. Provide the same dataframe as you provided to wplot() before you called this function
  regression <- lm(DF[, 2] ~ DF[, 1])
  abline(regression, ...)
  legendText = NULL
  condition = F
  if (coeff =="all") coeff = c("pearson", "spearman", "r2")
  if ( "pearson" %in% coeff) {    dispCoeff = iround(cor(DF[, 2], DF[, 1], method = "pearson"))
  legendText  =  c(legendText, paste0("Pears.: ", dispCoeff))  }
  if ("spearman" %in% coeff) {    dispCoeff = iround(cor(DF[, 2], DF[, 1], method = "spearman"))
  legendText = c(legendText, paste0("Spear.: ", dispCoeff))  }
  if ("r2" %in% coeff) {          r2 = iround(summary(regression)$r.squared)
  legendText = c(legendText, paste0("R^2: ", r2))  }
  # print(legendText)
  if (length(coeff)==1 & "r2" == coeff[1]) {  legend(textlocation, legend = superscript_in_plots(prefix = "R", sup = "2", suffix = paste0(": ", r2)) , bty="n", cex = cexx)
  } else {                                    legend(textlocation, legend = legendText , bty="n", cex = cexx) }
  if(savefile){   wplot_save_this(plotname = plotnameLastPlot) }
}

# Graphics ------------------------------------------------------------------------------------------------

#' try.dev.off
#'
#' Tries to close R graphical devices without raising an error.
#' @export
#' @examples try.dev.off ()

try.dev.off <- function () { try(dev.off(), silent = T) }

#' subscript_in_plots
#'
#' Create an expression with subscript for axis labels. Parsed when provided to xlab or ylab of a function.
#' @param prefix String before the subscript.
#' @param subscr Subscripted text.
#' @param quantity String in brackets after the subscript, eg.: log2(read count).
#' @export
#' @examples plot (1, 1, xlab =subscript_in_plots(subscr = 10, quantity = "read count"), ylab =subscript_in_plots())

subscript_in_plots <- function(prefix="log", subscr=2, quantity="arbitrary units") { # Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
  formatted_string = bquote(.(prefix)[.(subscr)]*'('*.(quantity)*')')
}


#' superscript_in_plots
#'
#' Create an expression with superscript for axis labels. Parsed when provided to xlab or ylab of a function.
#' @param prefix String before the superscript.
#' @param sup Superscripted text.
#' @param suffix String after the subscript.
#' @export
#' @examples plot (1, 1, main =superscript_in_plots())

superscript_in_plots <- function(prefix='n', sup='k', suffix='') { # Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
  formatted_string = bquote(.(prefix)^.(sup)*.(suffix))
}



# Colors -------------------------------------------------------------------------------------------------

#' wcolorize
#'
#' Generate color palettes. Input: a vector with categories, can be numbers or strings. Handles repeating values. Output: color vector of equal length as input. Optionally it can ouput a list where an extra element lists the categories (simply using unique would remove the names). See example.
#' @param vector A vector with categories, can be numbers or strings
#' @param UseRColorBrewer Use RColorBrewer palettes. Either FALSE or one of the legit RColorBrewer palettes, e.g.: "Set1". See its help.
#' @param ReturnCategoriesToo Return unique Categories. See example.
#' @param show Show generated color palette
#' @param set Color palette for base ("heat.colors", "terrain.colors", "topo.colors", "rainbow"), or gplots::rich.colors.
#' @param randomize Randomize colors
#' @export
#' @examples wcolorize (vector=c(1,1,1:6), ReturnCategoriesToo=T, show=T)

wcolorize  <- function(vector=c(1,1,1:6), RColorBrewerSet=F, ReturnCategoriesToo=F, show=F, randomize=F, set = c(F, "rich", "heat.colors", "terrain.colors", "topo.colors", "rainbow")[1]) {
  NrCol = l(unique(vector))
  condition = F
  COLZ = as.factor.numeric(vector) # if basic numbers
  if(randomize) {COLZ = sample(COLZ)} # if randomise
  if (RColorBrewerSet != F) {
    COLZ = RColorBrewer::brewer.pal(NrCol, name = RColorBrewerSet)[as.factor.numeric(vector)]
  } else {
    COLZ = if
    (set == "rainbow") {          rainbow(NrCol)[COLZ]} else if
    (set == "heat.colors") {        heat.colors(NrCol)[COLZ]} else if
    (set == "terrain.colors") {     terrain.colors(NrCol)[COLZ]} else if
    (set == "topo.colors") {        topo.colors(NrCol)[COLZ]} else if
    (set == "rich") {               gplots::rich.colors(NrCol)[COLZ]} else
      as.factor.numeric(vector) # if basic numbers
  }#if
  COLZ=as.vector(COLZ)
  names(COLZ) = vector
  CATEG = unique.wNames(COLZ)
  if(show) Color_Check(CATEG)
  if (ReturnCategoriesToo) {COLZ = list("vec" = COLZ, "categ" = CATEG)}
  return(COLZ)
}



#' val2col
#'
#' This function converts a vector of values("yourdata") to a vector of color levels. One must define the number of colors. The limits of the color scale("zlim") or the break points for the color changes("breaks") can also be defined. when breaks and zlim are defined, breaks overrides zlim. Source: http://menugget.blogspot.nl/2011/09/converting-values-to-color-levels.html
#' @param yourdata The data, to what the colors will be scaled to.
#' @param zlim Limits.
#' @param col Color of the plot.
#' @param breaks Number of bins.
#' @param rename The returned color vector will be named with its previous values
#' @export
#' @examples val2col (yourdata =  , zlim =  , col = rev(heat.colors(12)), breaks =  )


### CONTAINS A QUICK FIX FOR THE NUMBER OF COLOR LEVELS. See #59 on GitHub ###
val2col <- function (yourdata, zlim, col = rev(heat.colors( max(12, 3*l(unique(yourdata)))) ), breaks, rename=F) {

  if (!missing(breaks)) {
    if (length(breaks) != (length(col) + 1)) {
      stop("must have one more break than color")
    }
  }
  if (missing(breaks) & !missing(zlim)) {
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  if (missing(breaks) & missing(zlim)) {
    zlim <- range(yourdata, na.rm = TRUE)
    zlim[2] <- zlim[2] + c(zlim[2] - zlim[1]) * (0.001)
    zlim[1] <- zlim[1] - c(zlim[2] - zlim[1]) * (0.001)
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  colorlevels <- col[((as.vector(yourdata) - breaks[1])/(range(breaks)[2] - range(breaks)[1])) * (length(breaks) - 1) + 1]
  if (length(names(yourdata))) {
    names(colorlevels) = yourdata
  }

  if (rename) { names(colorlevels) = yourdata	} # works on vectors only
  colorlevels
}


# Printing to the markdown file and to the screen ------------------------------------------------------------------------------------------------

#' iprint
#'
#' A more intelligent printing function that collapses any variable passed to it by white spaces.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples iprint ("Hello ", "you ", 3, ", " , 11, " year old kids.")

iprint <- function (...) {
  argument_list <- c(...)
  print(paste(argument_list, collapse = " "))
}

any_print = iprint # for compatibility

#' llprint
#'
#' Collapse by white spaces a sentence from any variable passed on to the function. Print the sentence to the screen and write it to your markdown report file, if the "path_of_report" variable is defined.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples llprint (... =  )

llprint <- function (...) {
  argument_list <- c(...)
  LogEntry = print(paste(argument_list, collapse = " "))
  if (exists("path_of_report")) {	write(kollapse("\n", LogEntry, print = F), path_of_report, append = T)	}
  else {	print("NOT LOGGED: Log path and filename is not defined in path_of_report")	}
}


#' llogit
#'
#' Collapse by white spaces a sentence from any variable passed on to the function. llogit() writes it to your markdown report file, if the "path_of_report" variable is defined. It does not print the sentence to the screen.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples llogit (... =  )

llogit <- function (...) {
  argument_list <- c(...)
  LogEntry = paste(argument_list, collapse = " ")
  LogEntry = gsub("^ +| +$", "", LogEntry)
  if (!exists("path_of_report")) { print("Log path and filename is not defined in path_of_report") }
  write(kollapse("\n", LogEntry, print = F), path_of_report, append = T)
}


#' md.write.as.list
#'
#' Writes a vector as a (numbered) list into the report file.
#' @param vector Vecot to be wirtten as a list
#' @param h Level of header above tl list.
#' @param numbered TRUE = Numbered list, FALSE = unordered list is written
#' @param ... Additional parameters
#' @export
#' @examples md.write.as.list()

md.write.as.list <- function (vector=1:3, h=4, numbered =F, ...) {
  LogEntry = kollapse(rep("#",h)," ", substitute(vector), print = F)
  if (!exists("path_of_report")) { print("Log path and filename is not defined in path_of_report") }
  write(kollapse("\n", LogEntry, print = F), path_of_report, ..., append = T)
  LV = length(vector)
  LN = if (numbered) p0(" ", 1:LV, ". ", vector) else p0(" - ", vector)
  for (i in 1:LV ) write(LN[i], path_of_report, append = T)
}


#' llwrite_list
#'
#' Print a list object from R, one element per line, into your markdown report
#' @param yalist your list
#' @param printName print header level 4: the name of the list or a custom string
#' @export
#' @examples llwrite_list(your_list)

llwrite_list <- function(yalist, printName="self") {
  if (printName == "self")  llprint("####", substitute(yalist))  else if (printName == F) { ""} else { llprint("####", printName) }  #  else do not print
  for (e in 1:l(yalist)) {
    if (is.null( names(yalist) )) { llprint("#####", names(yalist)[e]) } else { llprint("#####", e)}
    print(yalist[e]); llogit("`", yalist[e], "`")
  }
}


#' md.import
#'
#' Import and concatenated an external markdown or text file to the report
#' @param from.file File to be appended at the (current) last line of the report
#' @param to.file The report file. Defined as "path_of_report" by default, which is set by the "setup_MarkdownReports" function.
#' @export
#' @examples md.import("~/Downloads/yourfile.txt")

md.import <- function(from.file, to.file = path_of_report) {
  linez = readLines(md.filename)
  if (!exists("path_of_report")) { print("Log path and filename is not defined in path_of_report") } else iprint(length(linez), "lines from",basename(from.file) ,"are concatenated to:", basename(path_of_report))
  for(LogEntry in linez) {
    write(LogEntry, path_of_report, append = T)
  }
}

# Writing markdown tables ------------------------------------------------------------------------------------------------

#' md.LogSettingsFromList
#'
#' Log the parameters & settings used in the script and stored in a list, in a table format in the report.
#' @param parameterlist List of Paramters
#' @param maxlen Maximum length of entries in a parameter list element
#' @export
#' @examples md.LogSettingsFromList(parameterlist = list("min"=4, "method"="pearson", "max"=10))

md.LogSettingsFromList <- function (parameterlist=p, maxlen =20) {
  LZ = unlapply(parameterlist, l) # collapse paramters with multiple entires
  LNG = names(which(LZ>1))
  for (i in LNG ) {
    if (l(parameterlist[[LNG]]) > maxlen) parameterlist[[LNG]] = parameterlist[[LNG]][1:maxlen]
    parameterlist[[LNG]] = paste(parameterlist[[LNG]], collapse = ", ")
  } #for
  DF = t(as.data.frame(parameterlist))
  colnames(DF) = "Value"
  MarkDown_Table_writer_DF_RowColNames(DF, title_of_table = "Script Parameters and Settings")
}


#' MarkDown_Table_writer_DF_RowColNames
#'
#' Take an R data frame with row- and column- names, parse a markdown table from it, and write it to the markdown report, set by "path_of_report".
#' @param df Input data frame to be plotted
#' @param FullPath Full path to the file.
#' @param percentify Format numbers [0, 1] to percentages 0-100.
#' @param title_of_table Title above the table (in the markdown report).
#' @param print2screen Print the markdown formatted table to the sceen.
#' @param WriteOut Write the table into a TSV file.
#' @export
#' @examples MarkDown_Table_writer_DF_RowColNames (df =  , FullPath = path_of_report, percentify = F, title_of_table = NA)

MarkDown_Table_writer_DF_RowColNames <- function (df, FullPath = path_of_report, percentify = F, title_of_table = NA, print2screen=F, WriteOut =F) {
  if (is.na(title_of_table)) {
    t = paste0(substitute(df), collapse = " ")
  }
  else {
    t = title_of_table
  }
  title_of_table = paste("\n#### ", t)
  write(title_of_table, path_of_report, append = T)
  h = paste(colnames(df), collapse = " \t| ")
  h = paste("\n| |", h, " |", collapse = "")
  ncolz = dim(df)[2] + 1
  nrows = dim(df)[1]
  rn = rownames(df)
  sep = kollapse(rep("| ---", ncolz), " |", print = F)
  if (exists("path_of_report")) {
    write(h, path_of_report, append = T)
    write(sep, path_of_report, append = T)
    for (r in 1:nrows) {
      if (is.numeric(unlist(df[r, ]))) {
        b = iround(df[r, ])
        if (percentify) {
          b = percentage_formatter(b)
        }
      }
      else {
        b = df[r, ]
      }
      b = paste(b, collapse = " \t| ")
      b = paste("|", rn[r], "\t|", b, " |", collapse = "")
      write(b, path_of_report, append = T)
    }
  }
  else {
    print("NOT LOGGED: Log path and filename is not defined in path_of_report")
  }
  if (WriteOut) { write.simple.tsv(NamedVector) }
  if (print2screen) { print(b) }
}


#' MarkDown_Table_writer_NamedVector
#'
#' Take an R vector with names, parse a markdown table from it, and write it to the markdown report, set by "path_of_report".
#' @param NamedVector A vector for the table body, with names as table header.
#' @param FullPath Full path to the file.
#' @param percentify Format numbers [0, 1] to percentages 0-100.
#' @param title_of_table Title above the table (in the markdown report).
#' @param print2screen Print the markdown formatted table to the sceen.
#' @param WriteOut Write the table into a TSV file.
#' @export
#' @examples MarkDown_Table_writer_NamedVector (NamedVector =  , FullPath = path_of_report, percentify = F, title_of_table = NA)

MarkDown_Table_writer_NamedVector <- function (NamedVector, FullPath = path_of_report, percentify = F, title_of_table = NA, print2screen=F, WriteOut = FALSE) {
  if (is.na(title_of_table)) {
    t = paste0(substitute(NamedVector), collapse = " ")
  }	else {		t = title_of_table	}
  title_of_table = paste("\n#### ", t)
  write(title_of_table, path_of_report, append = T)
  if (!is.table(NamedVector)) {
    if (is.list(NamedVector) & any(lapply(NamedVector, l)>1)) { print("This complex list cannot be parsed to a table.") }
    if (is.numeric(NamedVector)) {			NamedVector = iround(NamedVector)		}
  }
  h = paste(names(NamedVector), collapse = " \t| ")
  h = paste("\n| ", h, " |", collapse = "")
  ncolz = length(NamedVector)
  sep = kollapse(rep("| ---", ncolz), " |", print = F)
  if (exists("path_of_report")) {
    write(h, path_of_report, append = T)
    write(sep, path_of_report, append = T)
    if (percentify & is.numeric(NamedVector)) {
      NamedVector = percentage_formatter(NamedVector)
    }
    b = paste(NamedVector, collapse = " \t| ")
    b = paste("|", b, " |", collapse = "")
    write(b, path_of_report, append = T)
  }
  else {
    print("NOT LOGGED: Log path and filename is not defined in path_of_report")
  }
  if (WriteOut) { write.simple.tsv(NamedVector) }
  if (print2screen) { print(b) }
}


#' md.import.table
#'
#' Import a table (.csv, or tab seprated values, .tsv file) and write it in markdown format to the report.
#' @param from.file.table  The *.tsv file to be appended  as table at the (current) last line of the report.
#' @param title_of_table Title above the table (as header 4, in the markdown report).
#' @param has.rownames If the first column contains (unique!) rownames.
#' @param has.colnames If the first line of the file contains the header, or the column names.
#' @param field.sep Field separator in table file. Tab's by default.
#' @param to.file The report file. Defined as "path_of_report" by default, which is set by the "setup_MarkdownReports" function.
#' @export
#'
#' @examples md.import.table("~/Downloads/yourfile.tsv")

md.import.table <- function(from.file.table, title_of_table, has.rownames=T, has.colnames=T, field.sep = "\t", to.file = path_of_report) {
  TTL = if(missing(title_of_table)) basename(from.file.table) else title_of_table
  importedtable = if (has.rownames) { read.table( from.file.table , stringsAsFactors=FALSE, sep="\t", header=has.colnames, row.names = 1 )
  } else if (!has.rownames) {          read.table( from.file.table , stringsAsFactors=FALSE, sep="\t", header=has.colnames) }
  MarkDown_Table_writer_DF_RowColNames(importedtable, title_of_table = TTL)
  iprint("The follwoing table is included in the markdown report:")
  return(importedtable)
}


# Filtering Data ------------------------------------------------------------------------------------------------------------------------

#' filter_HP
#'
#' Filter values that fall between above high-pass-threshold (X >).
#' @param numeric_vector Values to be filtered.
#' @param threshold A numeric value above which "numeric_vector" passes.
#' @param passequal Pass if a value is larger, or equal than the threshold. FALSE by default.
#' @param prepend Text prepended to the results.
#' @param return_survival_ratio Return a number with the survival ratio (TRUE), or a logical index vector of the survivors (FALSE).
#' @export
#' @examples filter_HP (numeric_vector =  , threshold =  , prepend =  , return_survival_ratio = F)

filter_HP <- function(numeric_vector, threshold, passequal = F, prepend ="", return_survival_ratio=F, na_rm = T) { # Filter values that fall between above high-pass-threshold (X >).
  survivors <- if (passequal) { numeric_vector >= threshold } else { numeric_vector > threshold }
  pc = percentage_formatter(sum(survivors, na.rm = na_rm)/length(survivors))
  conclusion = kollapse(prepend, pc, " or ", sum(survivors, na.rm = na_rm), " of ", length(numeric_vector), " entries in ", substitute (numeric_vector), " fall above a threshold value of: ", iround(threshold))
  if (file.exists(path_of_report) ) {	llogit (conclusion)} else { print  ("NOT LOGGED") }
  if (return_survival_ratio) {return (sum(survivors, na.rm = na_rm)/length(survivors))} else if (!return_survival_ratio) { return (survivors) }
}


#' filter_LP
#'
#' Filter values that fall below the low-pass threshold (X <).
#' @param numeric_vector Values to be filtered.
#' @param threshold A numeric value below which "numeric_vector" passes.
#' @param passequal Pass if a value is smaller, or equal than the threshold. FALSE by default.
#' @param prepend Text prepended to the results.
#' @param return_survival_ratio Return a number with the survival ratio (TRUE), or a logical index vector of the survivors (FALSE).
#' @export
#' @examples filter_LP (numeric_vector =  , threshold =  , prepend =  , return_survival_ratio = F)

filter_LP <- function(numeric_vector, threshold, passequal = F, prepend ="", return_survival_ratio=F, na_rm = T) { # Filter values that fall below the low-pass threshold (X <).
  survivors <- if (passequal) { numeric_vector <= threshold } else { numeric_vector < threshold }
  pc = percentage_formatter(sum(survivors, na.rm = na_rm)/length(survivors))
  conclusion = kollapse(prepend, pc, " or ", sum(survivors, na.rm = na_rm), " of ", length(numeric_vector), " entries in ", substitute (numeric_vector), " fall below a threshold value of: ", iround(threshold))
  if (file.exists(path_of_report) ) {	llogit (conclusion, )	} else { print  ("NOT LOGGED") }
  if (return_survival_ratio) {return (sum(survivors, na.rm = na_rm)/length(survivors))} else if (!return_survival_ratio) { return (survivors) }
}


#' filter_MidPass
#'
#' Filter values that fall above high-pass-threshold !(X >=)! and below the low-pass threshold (X <).
#' @param numeric_vector Values to be filtered.
#' @param HP_threshold Lower threshold value. (>=)
#' @param LP_threshold Upper threshold value. (<)
#' @param prepend Text prepended to the results.
#' @param return_survival_ratio Return a number with the survival ratio (TRUE), or a logical index vector of the survivors (FALSE).
#' @param EdgePass If TRUE, it reverses the filter: everything passes except between the two thresholds.
#' @export
#' @examples filter_MidPass (numeric_vector =  , HP_threshold =  , LP_threshold =  , prepend =  , return_survival_ratio = FALSE, EdgePass = F)

filter_MidPass <- function(numeric_vector, HP_threshold, LP_threshold, prepend ="", return_survival_ratio=FALSE, EdgePass = F, na_rm = T) { # Filter values that fall above high-pass-threshold !(X >=)! and below the low-pass threshold (X <).
  survivors = ( numeric_vector >= HP_threshold & numeric_vector < LP_threshold); keyword = "between"; relation = " <= x < "
  if (EdgePass) {survivors = ( numeric_vector < HP_threshold | numeric_vector >= LP_threshold); keyword = "outside"; relation = " >= x OR x > " }
  pc = percentage_formatter(sum(survivors, na.rm = na_rm)/length(survivors))
  conclusion = kollapse(prepend, pc, " or ", sum(survivors, na.rm = na_rm), " of ", length(numeric_vector), " entries in ", substitute (numeric_vector), " fall ", keyword, " the thresholds: ", iround(HP_threshold), relation, iround(LP_threshold) )
  if (file.exists(path_of_report) ) {	llogit (conclusion)	} else { print  ("NOT LOGGED") }
  if (return_survival_ratio) {return (sum(survivors, na.rm = na_rm)/length(survivors))} else if (!return_survival_ratio) { return (survivors) }
}



# Generic ------------------------------------------------------------------------------------------

#' iround
#'
#' Rounds a value to the significant amount of digits. Its a wrapper for signif().
#' @param x Unrounded number.
#' @param digitz Number of digits to keep. 3 by default.
#' @export
#' @examples iround (x =  , digitz = 3)

iround <- function (x, digitz = 3) {
  signif(x, digits = digitz)
}



# String Manipulation ------------------------------------------------------------------------------------------------

#' kollapse
#'
#' Collapses values and strings to one string (without a white space). It also prints the results (good for a quick check)
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @param print Print the results to the terminal. TRUE by default.
#' @param print collapseby collapse elements into a string separated by this character
#' @export
#' @examples kollapse("Hello ", LETTERS[24], ", the winning numbers are ", c(1, 3, 5, 65, 11), " . Yay!")

kollapse <- function (..., collapseby = "", print = T) {
  if (print == T) {
    print(paste0(c(...), collapse = collapseby))
  }
  paste0(c(...), collapse = collapseby)
}

#' substrRight
#'
#' Take the right substring of a string
#' @param x a character vector.
#' @param n integer. The number of elements on the right to be kept.
#' @export
#' @examples substrRight  ("Not cool", n=4)

substrRight <- function (x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#' percentage_formatter
#'
#' Parse a string of 0-100% from a number between 0 and 1.
#' @param x A vector of numbers between [0, 1]/
#' @param digitz Number of digits to keep. 3 by default.
#' @export
#' @examples percentage_formatter (x =  , digitz = 3)

percentage_formatter <- function (x, digitz = 3) {
  a = paste(100 * iround(x, digitz), "%", sep = " ")
  a[a == "NaN %"] = NaN
  a[a == "NA %"] = NA
  return(a)
}


# Annotation parse / create / manipulate --------------------------------------------------------------------------------

#' getCategories
#'
#' Extract unique entries with a corresponding name.
#' @param named_categ_vec A vector of categories with names. "Uniqueness" in the vector and its name should be the same!!!
#' @export
#' @examples function(named_categ_vec)

getCategories <- function(named_categ_vec) { named_categ_vec[unique(names(named_categ_vec))] }

#' parFlags
#'
#' Create a string from the names of the (boolean) parameters (T or F) of true values. Use it for Suffixing plot names with the parameters that were used for that plot.
#' @param ... Paramter variables
#' @param pasteflg Boolean: paste the parameters-flags together?
#' @param collapsechar Separating character between each parameters-flag
#' @export
#' @examples pearson = T; filtered =T; normalized = F; MyPlotname = parFlags(prefix = "MyPlot" , pearson, filtered, normalized ); MyPlotname

parFlags <- function(prefix="", ..., pasteflg=T, collapsechar =".") {
  namez=as.character(as.list(match.call())[-(1:2)])
  val = c(...)
  names(val) =namez
  flg = which_names(val)
  flg= if (pasteflg) paste0(prefix, collapsechar, paste0(flg, collapse = collapsechar))
  return(flg)
}

#' UnlessSpec
#'
#' Return value X (TRUE by default) unless the variable is defined. If defined, it returns the variable.
#' @param NameOfaVariable Name of a possibly defined variable to be tested.
#' @param def Default return value
#' @export
#' @examples UnlessSpec("xsadasf32", 2); Num=22; UnlessSpec("Num", 1); UnlessSpec("c", 333)

UnlessSpec <- function(NameOfaVariable, def=TRUE) {  if( exists(NameOfaVariable) ) get(NameOfaVariable) else def }


# Internal function ------------------------------------------------------------------------------------------------

#' ww.FnP_parser
#'
#' Internal Function. Parses the full path from the filename & location of the file.
#' @param fname Name of the file
#' @param ext_wo_dot File extension without separating dot.
#' @export
#' @examples ww.FnP_parser(fname = 'myplot', ext_wo_dot = "jpg")

ww.FnP_parser <- function(fname, ext_wo_dot) {
  if ( exists('OutDir') ) {path = OutDir} else {path = paste0(getwd(), "/") ; iprint ("OutDir not defined !!!")}
  FnP = if (methods::hasArg(ext_wo_dot) ) kollapse (path, fname, ".", ext_wo_dot) else FnP = kollapse (path, "/", fname)
}

#' ww.set.mdlink
#'
#' Internal function. Sets inserting a markdown link to the image (created by the wplot* function that calls this function) only if 'path_of_report' is defined and 'b.mdlink' is defined as TRUE.
#' @param NameOfaVariable Name of a possibly defined variable to be tested.
#' @param def Default return value
#' @export
#' @examples ww.set.mdlink() # It is an internal function, not intended for manual use.

ww.set.mdlink <- function(NameOfaVariable="b.mdlink", def=FALSE) {  if( exists('path_of_report') && exists(NameOfaVariable) ) get(NameOfaVariable) else def }


#' ww.MarkDown_ImgLink_formatter
#'
#' Format a markdown image reference (link) from the file path to the file. It can parse the file path, if you pass it in separate variables and strings. E.g. ww.MarkDown_ImgLink_formatter(Directory, "MyImage.png").
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples ww.MarkDown_ImgLink_formatter (... =  )

ww.MarkDown_ImgLink_formatter <- function (...) {
  FullPath = kollapse(..., print = F)
  splt = strsplit(FullPath, "/")
  fn = splt[[1]][length(splt[[1]])]
  kollapse("![", fn, "]", "(", FullPath, ")", print = F)
}


#' ww.MarkDown_Img_Logger_PDF_and_PNG
#'
#' Format a markdown image reference (link) to a .pdf and .png versions of graph, and insert both links to the markdown report, set by "path_of_report". If the "b.png4Github" variable is set, the .png-link is set up such, that you can upload the whole report with the .png image into your GitHub repo's wiki, under "Reports"/OutDir/ (Reports is a literal string, OutDir is the last/deepest directory name in the "OutDir" variable. See create_set_OutDir() function.). This function is called by the ~wplot functions.
#' @param fname_wo_ext Name of the image file where markdown links going to point to.
#' @export
#' @examples ww.MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext =  )

ww.MarkDown_Img_Logger_PDF_and_PNG <- function (fname_wo_ext) {
  splt = strsplit(fname_wo_ext, "/")
  fn = splt[[1]][length(splt[[1]])]
  llogit(kollapse("![]", "(", fname_wo_ext, ".pdf)", print = F))
  if (UnlessSpec("b.usepng")) {
    if (UnlessSpec("b.png4Github")) {
      dirnm = strsplit(OutDir, split = "/")[[1]]
      dirnm = dirnm[length(dirnm)]
      llogit(kollapse("![]", "(Reports/", dirnm, "/", fname_wo_ext, ".png)", print = F))
    }	else {
      llogit(kollapse("![", fn, "]", "(", fname_wo_ext, ".png)", print = F))
    }
  } # if b.usepng
}

#' ww.ttl_field
#'
#' Internal function. Creates the string written into the PDF files "Title' (metadata) field.
#' @param flname Name of the plot
#' @export
#' @examples ww.ttl_field()

ww.ttl_field <- function (flname = fname ) { paste0(basename(flname), " by ", if (exists("b.scriptname")) b.scriptname else "Rscript") }


#' ww.autoPlotName
#'
#' Internal function. Creates automatic plot and file-names.
#' @param name Manually name your plot
#' @export
#' @examples ww.autoPlotName()

ww.autoPlotName <- function (name=NULL) {
  if (is.null(name)) {    filename = if (exists("plotnameLastPlot")) {plotnameLastPlot} else {make.names(date())}
  } else {                filename = name}
  return(filename)
}

# Legacy functions ------------------------------------------------------------------------------------------

#' setup_logging_markdown (deprecated, use with create_set_OutDir, will be removed from V3)
#'
#' Setup the markdown report file, create a sub directory in "OutDir". Its name is stamped with the script name and the modification time. Create the "path_of_report" variable used by all log-writing and ~wplot functions.
#' @param fname Name of the report file.
#' @param title Title of the report.
#' @param append Set append to TRUE if you do not want to overwrite the previous report. Use continue_logging_markdown() if you return logging into an existing report.
#' @param b.png4Github A global variable, defined by this and used by the other functions. If TRUE (default), any link to the .png versions of images will be created in a GitHub compatible format. That means, when you upload your markdown report and the .png images to your GitHub wiki under "Reports/" the links will correctly display the images online.
#' @export
#' @examples setup_logging_markdown (fname =  , title =  , append = T, b.png4Github = T)

setup_logging_markdown <- function (fname, title = "", append = T, b.png4Github = T) {
  if (exists("OutDir")) {		path = OutDir
  } else {					path = getwd(); iprint("OutDir not defined !!!")	}
  path_of_report <- kollapse(path, "/", fname, ".log.md")

  if (nchar(title)) {	write(paste("# ", title), path_of_report, append = append)
  } else {			write(paste("# ", fname, "Report"), path_of_report, append = append) }
  write(kollapse("		Modified: ", format(Sys.time(), "%d/%m/%Y | %H:%M | by: "), fname), path_of_report, append = T)
  BackupDir = kollapse(OutDir, "/", substr(fname, 1, nchar(fname)), "_", format(Sys.time(), "%Y_%m_%d-%Hh"), print = F)
  if (!exists(BackupDir)) {
    dir.create(BackupDir)
    assign("BackupDir", BackupDir, envir = .GlobalEnv)
  }
  assign("path_of_report", path_of_report, envir = .GlobalEnv)
  assign("b.png4Github", b.png4Github, envir = .GlobalEnv)
}

#' log_settings_MarkDown (OLD)
#'
#' Log the parameters & settings used in the script in a table format.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples log_settings_MarkDown (... =  )

log_settings_MarkDown <- function (...) {
  call <- match.call()
  namez = sapply(as.list(call[-1]), deparse)
  value = c(...)
  value = as.data.frame(value)
  rownames(value) = namez
  MarkDown_Table_writer_DF_RowColNames((value), title_of_table = "Settings")
}



# Alternative versions -------------


# #' subscript_in_plots2
# #' Like subscript_in_plots, but uses substitute() instead bquote()
# #'
# #' @param prefix String before the subscript. Use for boxplots, stripchart and co. Uses substitue() instead of bquote()
# #' @param subscr Subscripted text.
# #' @param quantity String in brackets after the subscript, eg.: log2(read count).
# #' @export
# #'
# #' @examples plot (1, 1, xlab =subscript_in_plots2(subscr = 10, quantity = "read count"), ylab =subscript_in_plots())

# subscript_in_plots2 <- function(prefix="log", subscr=2, quantity="arbitrary units") { # Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
#   formatted_string = substitute(paste(prefix[subscr], "(", quantity, ")") , list(subscr=subscr, prefix=prefix, quantity=quantity))
# }
# #' superscript_in_plots2
# #' Like superscript_in_plots, but uses substitute() instead bquote()
# #'
# #' @param prefix String before the superscript.
# #' @param sup Superscripted text.
# #' @param suffix String after the subscript.
# #' @export
# #'
# #' @examples plot (1, 1, main =superscript_in_plots2())

# superscript_in_plots2 <- function(prefix='n', sup='k', suffix='') { # Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
#   formatted_string = substitute(paste(prefix^sup, quantity) , list(prefix=prefix, sup=sup, suffix=suffix))
# }

# wlegend <- function(fill_ = NULL, poz=4, legend = names(fill_), bty = "n", ..., w_=7, h_=w_, OverwritePrevPDF =T) { # Add a legend, and save the plot immediately
#   stopif(is.null(names(fill_) & 1), message = "The color vector (fill_) has no name, and the variable 'legend' is not provided.")
#   pozz = translate(poz, oldvalues = 1:4, newvalues = c("topleft", "topright", "bottomright", "bottomleft"))
#   legend(x=pozz, legend=legend, fill=fill_, ..., bty=bty)
#   if (OverwritePrevPDF) {   wplot_save_this(plotname = plotnameLastPlot, w= w_, h = h_)  }
# }
