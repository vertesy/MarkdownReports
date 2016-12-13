## MarkdownReports.R
# author: Abel Vertesy
# date: 01-09-2016

## Aliases
try.dev.off <- function () { try(dev.off(), silent = T) }


#' kollapse
#'
#' Collapses values and strings to one string (without a white space). It also prints the results (good for a quick check)
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @param print Print the results to the terminal. TRUE by default.
#' @examples kollapse("Hello ",LETTERS[24], ", the winning numbers are ", c(1,3,5,65,11), " . Yay!")
#' @export

kollapse <-function (..., print = T) {
	if (print == T) {
		print(paste0(c(...), collapse = ""))
	}
	paste0(c(...), collapse = "")
}


#' any_print
#'
#' A more flexible printing function that collapses any variable passed to it by white spaces.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @examples any_print ("Hello ", "you ", 3, ", " , 11, " year old kids.")
#' @export

any_print <-function (...) {
	argument_list <- c(...)
	print(paste(argument_list, collapse = " "))
}


#' iround
#'
#' Rounds a value to the significant amount of digits. Its a wrapper for signif().
#' @param x Unrounded number.
#' @param digitz Number of digits to keep. 3 by default.
#' @examples iround (x =  , digitz = 3)
#' @export

iround <-function (x, digitz = 3) {
	signif(x, digits = digitz)
}


#' percentage_formatter
#'
#' Parse a string of 0-100% from a number between 0 and 1.
#' @param x A vector of numbers between [0,1]/
#' @param digitz Number of digits to keep. 3 by default.
#' @examples percentage_formatter (x =  , digitz = 3)
#' @export

percentage_formatter <-function (x, digitz = 3) {
	a = paste(100 * iround(x, digitz), "%", sep = " ")
	a[a == "NaN %"] = NaN
	a[a == "NA %"] = NA
	return(a)
}



#' setup_MarkdownReports
#'
#' Setup the markdown report file and the output directory, create a sub directory in "OutDir". Its name is stamped with the script name and the modification time. Create the "path_of_report" variable used by all log-writing and ~wplot functions.
#' @param OutDir The output directory (absolute / full path).
#' @param scriptname Name of the script (file) generating the report. "scriptname" will be used as the default title for the report. It is assigned to the global environment and used in pdf's title field to denote which script generated the file.
#' @param title Manually set the title of the report.
#' @param append Set append to TRUE if you do not want to overwrite the previous report. Use continue_logging_markdown() if you return logging into an existing report. FALSE by default: rerunning the script overwrites the previous report. Archive reports manually into the timestamped subfolder within the OutDir.
#' @param png4Github A global variable, defined by this and used by the plotting functions. If TRUE (default), any link to the .png versions of images will be created in a GitHub compatible format. That means, when you upload your markdown report and the .png images to your GitHub wiki under "Reports/" the links will correctly display the images online.
#' @examples setup_logging_markdown (scriptname =  , title =  , append = T, png4Github = T)
#' @export

setup_MarkdownReports <-function (OutDir = getwd(), scriptname = basename(OutDir), title = "", setDir=T, append = F, png4Github = T) {
	if (!exists(OutDir)) {	dir.create(OutDir)	}
	assign("OutDir", OutDir, envir = .GlobalEnv)
	any_print("All files will be saved under 'OutDir': ", OutDir)
	path_of_report <- paste0(OutDir, "/", scriptname, ".log.md")
	assign("path_of_report", path_of_report, envir = .GlobalEnv)
	any_print("MarkdownReport location is stored in 'path_of_report': ", path_of_report)

	if (nchar(title)) {	write(paste("# ", title), path_of_report, append = append)
	} else {			write(paste("# ", scriptname, "Report"), path_of_report, append = append) }
	write(paste0("		Modified: ", format(Sys.time(), "%d/%m/%Y | %H:%M | by: "), scriptname), path_of_report, append = T)
	BackupDir = kollapse(OutDir, "/", substr(scriptname, 1, nchar(scriptname)), "_", format(Sys.time(), "%Y_%m_%d-%Hh"), print = F)
	if (setDir) {	setwd(OutDir)}
	if (!exists(BackupDir)) {
		dir.create(BackupDir)
		assign("BackupDir", BackupDir, envir = .GlobalEnv)
	}
	assign("png4Github", png4Github, envir = .GlobalEnv)
	assign("scriptname", scriptname, envir = .GlobalEnv)
}

#' create_set_OutDir (deprecated, use with setup_logging_markdown, will be removed from V3)
#'
#' Create or set the output directory of the script, and set the "OutDir" variable that is used by all ~wplot functions.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @examples create_set_OutDir (... =  )
#' @export

create_set_OutDir <-function (..., setDir=T) {
	OutDir = kollapse(..., print = F)
	any_print("All files will be saved under 'OutDir': ", OutDir)
	if (!exists(OutDir)) {	dir.create(OutDir)	}
	if (setDir) {	setwd(OutDir)}
	assign("OutDir", OutDir, envir = .GlobalEnv)
}


#' setup_logging_markdown (deprecated, use with create_set_OutDir, will be removed from V3)
#'
#' Setup the markdown report file, create a sub directory in "OutDir". Its name is stamped with the script name and the modification time. Create the "path_of_report" variable used by all log-writing and ~wplot functions.
#' @param fname Name of the report file.
#' @param title Title of the report.
#' @param append Set append to TRUE if you do not want to overwrite the previous report. Use continue_logging_markdown() if you return logging into an existing report.
#' @param png4Github A global variable, defined by this and used by the other functions. If TRUE (default), any link to the .png versions of images will be created in a GitHub compatible format. That means, when you upload your markdown report and the .png images to your GitHub wiki under "Reports/" the links will correctly display the images online.
#' @examples setup_logging_markdown (fname =  , title =  , append = T, png4Github = T)
#' @export

setup_logging_markdown <-function (fname, title = "", append = T, png4Github = T) {
	if (exists("OutDir")) {		path = OutDir
	} else {					path = getwd(); any_print("OutDir not defined !!!")	}
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
	assign("png4Github", png4Github, envir = .GlobalEnv)
}

#' continue_logging_markdown
#'
#' Continue writing to an existing report file.
#' @param fname Name of the report file.
#' @examples continue_logging_markdown (fname =  )
#' @export

continue_logging_markdown <-function (scriptname) {
  if (exists("OutDir")) {	path = OutDir } else {	path = getwd(); any_print("OutDir not defined !!! Saving in working directory.") }
  path_of_report <- kollapse(path, "/", scriptname, ".log.md", print = F)
  any_print("Writing report in:", path_of_report)
  assign("path_of_report", path_of_report, envir = .GlobalEnv)

  BackupDir = kollapse(OutDir, "/", substr(scriptname, 1, (nchar(scriptname) - 2)), format(Sys.time(), "%Y_%m_%d-%Hh"), print = F)
  if (!exists(BackupDir)) {
    dir.create(BackupDir)
    assign("BackupDir", BackupDir, envir = .GlobalEnv)
  }
}


#' log_settings_MarkDown
#'
#' Log the parameters & settings used in the script in a table format.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @examples log_settings_MarkDown (... =  )
#' @export

log_settings_MarkDown <-function (...) {
	call <- match.call()
	namez = sapply(as.list(call[-1]), deparse)
	value = c(...)
	value = as.data.frame(value)
	rownames(value) = namez
	MarkDown_Table_writer_DF_RowColNames((value), title_of_table = "Settings")
}

#' llprint
#'
#' Collapse by white spaces a sentence from any variable passed on to the function. Print the sentence to the screen and write it to your markdown report file, if the "path_of_report" variable is defined.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @examples llprint (... =  )
#' @export

llprint <-function (...) {
	argument_list <- c(...)
	LogEntry = print(paste(argument_list, collapse = " "))
	if (exists("path_of_report")) {	write(kollapse("\n", LogEntry, print = F), path_of_report, append = T)	}
	else {	print("NOT LOGGED: Log path and filename is not defined in path_of_report")	}
}


#' llogit
#'
#' Collapse by white spaces a sentence from any variable passed on to the function. llogit() writes it to your markdown report file, if the "path_of_report" variable is defined. It does not print the sentence to the screen.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @examples llogit (... =  )
#' @export

llogit <-function (...) {
	argument_list <- c(...)
	LogEntry = paste(argument_list, collapse = " ")
	LogEntry = gsub("^ +| +$", "", LogEntry)
	if (!exists("path_of_report")) { print("Log path and filename is not defined in path_of_report") }
	write(kollapse("\n", LogEntry, print = F), path_of_report, append = T)
}


#' MarkDown_ImgLink_formatter
#'
#' Format a markdown image reference (link) from the file path to the file. It can parse the file path, if you pass it in separate variables and strings. E.g. MarkDown_ImgLink_formatter(Directory, "MyImage.png").
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @examples MarkDown_ImgLink_formatter (... =  )
#' @export

MarkDown_ImgLink_formatter <-function (...) {
	FullPath = kollapse(..., print = F)
	splt = strsplit(FullPath, "/")
	fn = splt[[1]][length(splt[[1]])]
	kollapse("![", fn, "]", "(", FullPath, ")", print = F)
}


#' MarkDown_Img_Logger_PDF_and_PNG
#'
#' Format a markdown image reference (link) to a .pdf and .png versions of graph, and insert both links to the markdown report, set by "path_of_report". If the "png4Github" variable is set, the .png-link is set up such, that you can upload the whole report with the .png image into your GitHub repo's wiki, under "Reports"/OutDir/ (Reports is a literal string, OutDir is the last/deepest directory name in the "OutDir" variable. See create_set_OutDir() function.). This function is called by the ~wplot functions.
#' @param fname_wo_ext Name of the image file where markdown links going to point to.
#' @examples MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext =  )
#' @export

MarkDown_Img_Logger_PDF_and_PNG <-function (fname_wo_ext) {
	splt = strsplit(fname_wo_ext, "/")
	fn = splt[[1]][length(splt[[1]])]
	llogit(kollapse("![]", "(", fname_wo_ext, ".pdf)", print = F))
	if (exists("png4Github") & png4Github == T) {
		dirnm = strsplit(OutDir, split = "/")[[1]]
		dirnm = dirnm[length(dirnm)]
		llogit(kollapse("![]", "(Reports/", dirnm, "/", fname_wo_ext, ".png)", print = F))
	}
	else {
		llogit(kollapse("![", fn, "]", "(", fname_wo_ext, ".png)", print = F))
	}
}


#' MarkDown_Table_writer_DF_RowColNames
#'
#' Take an R data frame with row- and column- names, parse a markdown table from it, and write it to the markdown report, set by "path_of_report".
#' @param df Input data frame to be plotted
#' @param FullPath Full path to the file.
#' @param percentify Format numbers [0,1] to percentages 0-100.
#' @param title_of_table Title above the table (in the markdown report).
#' @examples MarkDown_Table_writer_DF_RowColNames (df =  , FullPath = path_of_report, percentify = F, title_of_table = NA)
#' @export

MarkDown_Table_writer_DF_RowColNames <-function (df, FullPath = path_of_report, percentify = F, title_of_table = NA) {
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
}


#' MarkDown_Table_writer_NamedVector
#'
#' Take an R vector with names, parse a markdown table from it, and write it to the markdown report, set by "path_of_report".
#' @param NamedVector A vector for the table body, with names as table header.
#' @param FullPath Full path to the file.
#' @param percentify Format numbers [0,1] to percentages 0-100.
#' @param title_of_table Title above the table (in the markdown report).
#' @examples MarkDown_Table_writer_NamedVector (NamedVector =  , FullPath = path_of_report, percentify = F, title_of_table = NA)
#' @export

MarkDown_Table_writer_NamedVector <-function (NamedVector, FullPath = path_of_report, percentify = F, title_of_table = NA) {
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
}


#' wplot
#'
#' Create and save scatter plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions. The .png version is not created, only the link is put in place. You can add 2D error bars around the dots, or add lines (ablines) to your plot, by setting "abline" argument to = F (no line, default), "h" (horizontal, further specified by a = y-offset), "v" (vertical, further specified by a = x-offset), "ab" (line with an angle, further specified by a = offset, b = slope).
#' @param df Input data frame to be plotted_2columns
#' @param col Color of the plot.
#' @param pch Define the symbol for each data point. A number [0-25] or any string between ""-s.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param errorbar Draw error bars if TRUE. Pass on the value in parameters "upper" and "lower". Refine the look by "w" and "arrow_lwd".
#' @param upper Size of the upper error bar.
#' @param lower Size of the lower error bar. By default, it equals the upper error bar.
#' @param left Size of the left error bar.
#' @param right Size of the right error bar. By default, it equals the left error bar.
#' @param w Width of the saved pdf image, in inches.idth
#' @param arrow_lwd Line width for the error bar arrow. Line width for the error bar arrow.
#' @param col_errorbar Color of the error bar arrow.
#' @param abline Draw a line on the plot. FALSE by default. Use parameters "a" and "b" to draw horizontal, vertical or inclined lines.
#' @param a X-offset for vertical lines, Y-offset for horizontal, and inclined lines.
#' @param b Slope of an inclined line.
#' @param lty Linetype, defined by numbers 1-6.
#' @param lwd Line width. Set to 2 by default.
#' @param col_abline Color of the line.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param ylimm manual Y-limits error bar
#' @param xlimm manual X-limits error bar
#' @examples wplot (df_2columns =  , col = 1, pch = 18, ... =  , w = 7, h = 7, plotname = substitute(df_2columns), mdlink = F, errorbar = F, upper = 0, lower = upper, left = 0, right = left, width = 0.1, arrow_lwd = 1, abline = F, a = F, b = F, lty = 1, lwd = 1, col_abline = 1)
#' @export

wplot <-function (df_2columns, col = 1, pch = 18, ...,plotname = substitute(df_2columns),
                  errorbar = F, upper = 0, lower = upper, left = 0, right = left, width = 0.1, arrow_lwd = 1, col_errorbar = 1, ylimm=F, xlimm=F,
                  abline = F, a = F, b = F, lty = 1, lwd = 1, col_abline = 1,
                  savefile = T, mdlink = F,  w = 7, h = 7) {
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
  if (savefile) { dev.copy2pdf(file = FnP_parser(fname, "pdf"), width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
  if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}

#' wplot_save_this
#'
#' Save the currently active graphic device (for complicated plots).  Insert links to your markdown report, set by "path_of_report". Name the file by naming the variable!
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @examples wplot_save_this (plotname = date(), col = gold1, ... =  , w = 7, h = 7, mdlink = FALSE, ManualName = FALSE)
#' @export

wplot_save_this <-function (plotname = date(), ..., w = 7, h = 7, mdlink = FALSE) {
	dev.copy2pdf(file = FnP_parser(plotname, "pdf"), width = w, height = h, title = paste0(plotname, " by ", if (exists("scriptname")) scriptname else "Rscript"))
	if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = plotname) }
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
#' @examples whist (variable =  , col = gold1, w = 7, h = 7, plotname = substitute(variable), breaks = 20, main = kollapse("Histogram of ", substitute(variable)), xlb = substitute(variable), mdlink = FALSE, hline = F, vline = F, lty = 2, lwd = 3, lcol = 2, filtercol = 0, ... =  )
#' @export

whist <-function (variable, breaks = 20, col = "gold1", plotname = substitute(variable), main = kollapse("Histogram of ", 	substitute(variable)), xlb = substitute(variable),
				  hline = F, vline = F, lty = 2, lwd = 3, lcol = 1, filtercol = 0,
				  savefile = T, w = 7, h = 7, mdlink = FALSE, ...) {
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
			if (filtercol == 1) {	col = (histdata$breaks >= vline) + 2
			} else if (filtercol == -1) {	col = (histdata$breaks < vline) + 2	}

			hist(variable, ..., main = main, breaks = breaks, xlab = xlb, col = col, las = 2)
		}
		# if (hline) { abline(h = hline, lty = lty, lwd = lwd, col = lcol) }
		if (vline & !length(xtra$xlim)) { PozOfvline = NULL;
			for (l in 1:length(vline)) {
				PozOfvline[l] = mean(histdata$mids[c(max(which(histdata$breaks < vline[l])), min(which(histdata$breaks >= vline[l])))])
			}
			abline(v = PozOfvline, lty = lty, lwd = lwd, col = lcol)
		}
		else if (vline & length(xtra$xlim)) { abline(v = vline, lty = lty, lwd = lwd, col = 1)	}
		if (savefile) { dev.copy2pdf(file = FnP_parser(fname, "pdf"), width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
	} else { any_print(variable, " IS EMPTY")	}
	assign("plotnameLastPlot", fname, envir = .GlobalEnv)
	if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
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
#' @examples wbarplot (variable =  , ... =  , col = gold1, sub = F, plotname = substitute(variable), main = substitute(variable), w = 7, h = 7, incrBottMarginBy = 0, mdlink = F, tilted_text = F, hline = F, vline = F, filtercol = 1, lty = 1, lwd = 2, lcol = 2, errorbar = F, upper = 0, lower = upper, arrow_width = 0.1, arrow_lwd = 1)
#' @export

wbarplot <-function (variable, ..., col = "gold1", sub = F, plotname = substitute(variable), main = substitute(variable), tilted_text = F, ylimits = NULL,
                     hline = F, vline = F, filtercol = 1, lty = 1, lwd = 2, lcol = 2,
                     errorbar = F, upper = 0, lower = upper, arrow_width = 0.1, arrow_lwd = 1,
                     savefile = T, w = 7, h = 7, incrBottMarginBy = 0, mdlink = F) {
  NrBars = if (is.vector(variable)) l(variable) else if (is.matrix(variable) |   is.data.frame(variable)) ncol(variable)
  BarNames = if (is.vector(variable)) names(variable) else if (is.matrix(variable) |   is.data.frame(variable)) colnames(variable)

  fname = kollapse(plotname, ".barplot")
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  cexNsize = 0.8/abs(log10(length(variable)))
  cexNsize = min(cexNsize, 1)
  if (sub == T) {	subtitle = paste("mean:", iround(mean(variable, na.rm = T)), "CV:", percentage_formatter(cv(variable)))	} else if (sub == F) { subtitle = "" } else { subtitle = sub }
  if (hline & filtercol == 1) { col = (variable >= hline) + 2	}
  if (hline & filtercol == -1) { col = (variable < hline) + 2	}
  if (errorbar & is.null(ylimits)) {	ylimits = range(c(0, (variable + upper + abs(0.1 * variable)), variable - lower - abs(0.1 * variable)), na.rm = T) } # else {	ylimits = range(0, variable)	}
  if (tilted_text) {	xlb = rep(NA, NrBars)	}	else {		xlb = BarNames	}

  x = barplot(variable, ylim = ylimits, ..., names.arg = xlb, main = main, sub = subtitle, col = col, las = 2, cex.names = cexNsize)
  if (hline) { abline(h = hline, lty = lty, lwd = lwd, col = lcol)	}
  if (vline[1]) { abline(v = x[vline], lty = lty, lwd = lwd, col = lcol)	}
  if (errorbar) {	arrows(x, variable + upper, x, variable - lower, angle = 90, code = 3, length = arrow_width, lwd = arrow_lwd, ...)	}
  if (tilted_text) {
    text(x = x - 0.25, y = 0, labels = BarNames, xpd = TRUE, srt = 45, cex = cexNsize, adj = c(1,3))
  }

  if (savefile) { dev.copy2pdf(file = FnP_parser(fname, "pdf"), width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname)	}
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
#' @examples wboxplot (variable =  , ... =  , col = gold1, plotname = as.character(substitute(variable)), sub = FALSE, incrBottMarginBy = 0, tilted_text = F, w = 7, h = 7, mdlink = F)
#' @export

wboxplot <-function (yalist, ..., col = "gold1", plotname = as.character(substitute(yalist)), ylb="", sub = FALSE, incrBottMarginBy = 0, 	tilted_text = F,
                     savefile = T, w = 7, h = 7, mdlink = F) {
  fname = kollapse(plotname, ".boxplot")
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  if (tilted_text) { 	xlb = NA } else {	xlb = names(yalist) }
  boxplot(yalist, ..., names = xlb, main = plotname, col = col, las = 2)
  mtext(ylb, side = 2, line = 2)
  if (tilted_text) {
    text(x = 1:length(yalist), y = min(unlist(yalist), na.rm = T)-(max(nchar(names(yalist)))/2), labels = names(yalist), xpd = TRUE, srt = 45)
  }
  if (savefile) { dev.copy2pdf(file = FnP_parser(fname, "pdf"), width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
}


#' wpie
#'
#' Create and save pie charts as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.
#' @param variable The variable to plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @param percentage Display percentage instead of counts. TRUE by default.
#' @param both_pc_and_value Report both percentage AND number. 
#' @param plotname Title of the plot (main parameter) and also the name of the file.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @examples wpie (variable =  , ... =  , percentage = TRUE, plotname = substitute(variable), w = 7, h = 7, mdlink = F)
#' @export

wpie <-function (variable, ..., percentage = TRUE, both_pc_and_value=F, plotname = substitute(variable), savefile = T, w = 7, h = 7, mdlink = F) {
	fname = kollapse(plotname, ".pie")
	subt = kollapse("Total = ", sum(variable), print = F)
	if (percentage) {	labs <- paste("(", names(variable), ")", "\n", percentage_formatter(variable/sum(variable)), sep = "")
	if (both_pc_and_value) { labs <- paste("(", names(variable), ")", "\n", percentage_formatter(variable/sum(variable)),"\n", variable , sep = "")}
	} else {	labs <- paste("(", names(variable), ")", "\n", variable, sep = "")	}
	pie(variable, ..., main = plotname, sub = subt, clockwise = T, labels = labs, col = rainbow(length(variable)))
	if (savefile) { dev.copy2pdf(file = FnP_parser(fname, "pdf"), width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
	if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
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
#' @examples wstripchart (yalist =  , ... =  , plotname = as.character(substitute(yalist)), sub = FALSE, border = 1, BoxPlotWithMean = F, pch = 23, pchlwd = 1, pchcex = 1.5, bg = chartreuse2, col = black, metod = jitter, jitter = 0.2, colorbyColumn = F, w = 7, h = 7, incrBottMarginBy = 0, tilted_text = F, mdlink = F)
#' @export

wstripchart <-function (yalist, ..., plotname = as.character(substitute(yalist)), sub = NULL,
                        border = 1, incrBottMarginBy = 0, tilted_text = F, BoxPlotWithMean = F, metod = "jitter", jitter = 0.2,
                        pch = 23, pchlwd = 1, pchcex = 1.5, bg = "chartreuse2", col = "black", colorbyColumn = F, ylb="",
                        savefile = T, w = 7, h = 7, mdlink = F) {
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  cexNsize = 1/abs(log10(length(yalist)))
  cexNsize = min(cexNsize, 1)
  fname = kollapse(plotname, ".stripchart")
  a = boxplot(yalist, plot = F)
  if (colorbyColumn) {	pchlwd = 5; pchcex = 0.5 }
  if (BoxPlotWithMean) {	a$stats[3, ] = unlist(lapply(yalist, mean))	}
  if (tilted_text) {	xlb = F } else { xlb = T }
  bxp(a, xlab = "", show.names = xlb, ..., main = plotname, sub = sub, border = border, outpch = NA, las = 2,
      outline = T, cex.axis = cexNsize, ylab=NA)
  stripchart(yalist, vertical = TRUE, add = TRUE, method = metod, jitter = jitter, pch = pch, bg = bg,
             col = col, lwd = pchlwd, cex = pchcex)
  mtext(ylb, side = 2, line = 2)
  if (tilted_text) {
    xx= min(unlist(yalist), na.rm = T)
    # yy = (max(nchar(names(yalist)))/2)
    text(x = 1:length(yalist), y=xx, labels = names(yalist), xpd = TRUE, srt = 45, adj = c(1,3))
  }
  if (savefile) { dev.copy2pdf(file = FnP_parser(fname, "pdf"), width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
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
#' @examples wstripchart_list (yalist =  , ... =  , plotname = as.character(substitute(yalist)), sub = FALSE, ylb = NULL, xlab = NULL, border = 1, bxpcol = 0, pch = 23, pchlwd = 1, pchcex = 1.5, bg = chartreuse2, coll = black, metod = jitter, jitter = 0.2, w = 7, h = 7, incrBottMarginBy = 0, tilted_text = F, mdlink = F)
#' @export

wstripchart_list <-function ( yalist, ...,	border = 1, bxpcol = 0, pch = 23, pchlwd = 1, pchcex = 1.5, bg = "chartreuse2", coll = "black", metod = "jitter", jitter = 0.2,
                              plotname = as.character(substitute(yalist)), sub = NULL, ylb = "", xlab = "",  incrBottMarginBy = 0, tilted_text = F,
                              savefile = T, w = 7, h = 7, mdlink = F) {
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
    text(x = 1:length(yalist), y = xx, labels = names(yalist), xpd = TRUE, srt = 45, adj = c(1,3))
  }
  if (savefile) { dev.copy2pdf(file = FnP_parser(fname, "pdf"), width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
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
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param ylimm Manual y axis limits
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @examples wvioplot_list (yalist =  , ... =  , xlb = names(yalist), ylb =  , coll = c(1:length(yalist)), incrBottMarginBy = 0, w = 7, h = 7, plotname = as.character(substitute(yalist)), tilted_text = F, mdlink = F)
#' @export

wvioplot_list <-function (yalist, ..., coll = c(2:(length(yalist)+1)),
                          plotname = as.character(substitute(yalist)), sub = NULL, xlb = names(yalist), ylb = "", ylimm=F,
                          incrBottMarginBy = 0, tilted_text = F, savefile = T, w = 7, h = 7, mdlink = F) {
  if (!require("vioplot")) { print("Please install vioplot: install.packages('vioplot')") }
  if (incrBottMarginBy) { .ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) } 	# Tune the margin
  l_list = length(yalist)
  fname = kollapse(plotname, ".vioplot")
  if (length(coll) < l_list) { coll = rep(coll, l_list) }
  if (tilted_text) {	xlb = NA } else { xlb = names(yalist) }
  if (! (is.numeric(ylimm) & length(ylimm)==2)) { ylimm = range(unlist(yalist),na.rm = T)}
  plot(0, 0, type = "n", xlim = c(0.5, (l_list + 0.5)), ylim = ylimm, xaxt = "n", xlab = "",
       ylab = ylb, main = plotname, sub = sub)
  for (i in 1:l_list) {
    if( l(na.omit.strip(yalist[[i]])) ){
      vioplot::vioplot(na.omit(yalist[[i]]), ..., at = i, add = T, col = coll[i])
    }
  }
  axis(side = 1, at = 1:l_list, labels = xlb, las = 2)
  if (tilted_text) {
    text(x = 1:length(yalist), y = min(unlist(yalist))-(max(nchar(names(yalist)))/2), labels = names(yalist), xpd = TRUE, srt = 45)
  }
  if (savefile) { dev.copy2pdf(file = FnP_parser(fname, "pdf"), width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
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
#' @examples wviostripchart_list (yalist =  , ... =  , pch = 23, viocoll = 0, vioborder = 1, ylb =  , plotname = as.character(substitute(yalist)), sub = F, bg = 0, coll = black, metod = jitter, jitter = 0.1, w = 7, h = 7, incrBottMarginBy = 0, mdlink = F)
#' @export

wviostripchart_list <-function (yalist, ..., pch = 23, viocoll = 0, vioborder = 1, bg = 0, coll = "black", metod = "jitter", jitter = 0.1,
                                plotname = as.character(substitute(yalist)), sub = NULL, ylb = "", incrBottMarginBy = 0,
                                savefile = T, w = 7, h = 7, mdlink = F) {
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
  if (savefile) { dev.copy2pdf(file = FnP_parser(fname, "pdf"), width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}
  assign("plotnameLastPlot", fname, envir = .GlobalEnv)
  if (mdlink) { MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname) }
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
#' @examples whist_dfCol (df =  , colName =  , col = gold, ... =  , w = 7, h = 7)
#' @export

whist_dfCol <-function (df, colName, col = "gold", ..., savefile = T, w = 7, h = 7) {
  stopifnot(colName %in% colnames(df))
  variable = as.vector(unlist(df[, colName]))
  stopifnot(length(variable) > 1)
  plotname = paste(substitute(df), "__", colName, sep = "")
  fname = FnP_parser(plotname, "hist.pdf")
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
  if (savefile) { dev.copy2pdf(file = fname, width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript")) }
}


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
#' @examples wbarplot_dfCol (df =  , colName =  , col = gold1, w = 7, h = 7, ... =  )
#' @export

wbarplot_dfCol <-function (df, colName, col = "gold1", savefile = T, w = 7, h = 7, ...) {
	stopifnot(colName %in% colnames(df))
	variable = unlist(df[, colName])
	stopifnot(length(variable) > 1)
	plotname = paste(substitute(df), "__", colName, sep = "")
	FullPath = FnP_parser(plotname, "barplot.pdf")
	cexNsize = 0.7/abs(log10(length(variable)))
	cexNsize = min(cexNsize, 1)
	barplot(variable, ..., main = plotname, col = col, las = 2, cex.names = cexNsize, sub = paste("mean:",
																								  iround(mean(variable, na.rm = T)), "CV:", percentage_formatter(cv(variable))))
	if (savefile) { dev.copy2pdf(file = FullPath, width = w, height = h, title = paste0(basename(fname), " by ", if (exists("scriptname")) scriptname else "Rscript"))	}
}


#' val2col
#'
#' This function converts a vector of values("yourdata") to a vector of color levels. One must define the number of colors. The limits of the color scale("zlim") or the break points for the color changes("breaks") can also be defined. when breaks and zlim are defined, breaks overrides zlim. Source: http://menugget.blogspot.nl/2011/09/converting-values-to-color-levels.html
#' @param yourdata The data, to what the colors will be scaled to.
#' @param zlim Limits.
#' @param col Color of the plot.
#' @param breaks Number of bins.
#' @examples val2col (yourdata =  , zlim =  , col = rev(heat.colors(12)), breaks =  )
#' @export


### CONTAINS A QUICK FIX FOR THE NUMBER OF COLOR LEVELS. See #59 on GitHub ###
val2col <-function (yourdata, zlim, col = rev(heat.colors( max(12,3*l(unique(yourdata)))) ), breaks) {

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
	colorlevels
}

#' wvenn
#'
#' Save venn diagrams. Unlike other ~vplot funcitons, this saves directly into a .png, and it does not use the dev.copy2pdf() function.
#' @param yalist The variable to plot.
#' @param imagetype Image format, png by default.
#' @param alpha Transparency, .5 by default.
#' @param ... Pass any other parameter of the corresponding venn.diagram() function (most of them should work).
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @examples wvenn (yalist =  , imagetype = png, alpha = 0.5, ... =  , w = 7, h = 7, mdlink = F)
#' @export

wvenn <- function (yalist, imagetype = "png", alpha = .5, ..., w = 7, h = 7, mdlink = F) {
	if (!require("VennDiagram")) { print("Please install VennDiagram: install.packages('VennDiagram')") }
	fill = 1:length(yalist)
	fname = kollapse(substitute(yalist), ".", imagetype, print = F)
	filename = kollapse(OutDir,"/", fname, print = F)
	subt = kollapse("Total = ", length(unique(unlist(yalist))), " elements in total.", print = F)
	venn.diagram(x = yalist, imagetype = imagetype, filename = filename, main = substitute(yalist),
				 , ... , sub = subt, fill = fill, alpha = alpha, sub.cex = .75, main.cex = 2)
	if (mdlink) {
		llogit(MarkDown_ImgLink_formatter(fname))
		if (exists("png4Github") & png4Github == T) { llogit(MarkDown_ImgLink_formatter(paste0("Reports/", fname) ) )	}
	}
}


#' error_bar
#'
#' Put error bars on top of your bar plots. This functionality is now integrated into MarkdownReporter's wbarplot() function
#' @param x X-position on the plot.
#' @param y Y-position on the plot.
#' @param upper Size of the upper error bar.
#' @param lower Size of the lower error bar. By default, it equals the upper error bar.
#' @param width Width of the erro bar whisker.
#' @param ... Pass any other parameter of the corresponding arrows function (most of them should work).
#' @examples error_bar (x =  , y =  , upper =  , lower = upper, width  = 0.1, ... =  )
#' @export

error_bar <-function (x, y, upper, lower = upper, width  = 0.1, ...) {
	stopifnot(length(x) == length(y) & length(y) == length(lower) & length(lower) == length(upper))
	if (length(dim(y)) > 1) {
		arrows(as.vector(x), as.vector(y + upper), as.vector(x), as.vector(y - lower), angle = 90, code = 3,
			   length = width , ...)
	}
	else {
		arrows(x, y + upper, x, y - lower, angle = 90, code = 3, length = width , ...)
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
#' @param OverwritePrevPDF Save the plot immediately with the same name the last wplot* function made (It is stored in plotnameLastPlot variable).
#' @param ... Pass any other parameter of the corresponding text function (most of them should work).
#' @examples barplot (1:10); barplot_label (1:10, labels =11:2  , bottom = F, TopOffset = 0.5, relpos_bottom = 0.1, ... =  )
#' @export

barplot_label <-function (barplotted_variable, labels, bottom = F, TopOffset = .5, relpos_bottom = 0.1, OverwritePrevPDF = T, ...) {
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
  if (OverwritePrevPDF) {   wplot_save_this(plotname = plotnameLastPlot)  }
}


#' subscript_in_plots
#'
#' @param prefix String before the subscript.
#' @param subscr Subscripted text.
#' @param quantity String in brackets after the subscript, eg.: log2(read count).
#' @export
#'
#' @examples plot (1, 1, xlab =subscript_in_plots(subscr = 10,quantity = "read count"), ylab =subscript_in_plots())

subscript_in_plots <- function(prefix="log", subscr=2, quantity="arbitrary units") { # Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
  formatted_string = bquote(.(prefix)[.(subscr)]*'('*.(quantity)*')')
}


#' superscript_in_plots
#'
#' @param prefix String before the superscript.
#' @param sup Superscripted text.
#' @param suffix String after the subscript.
#' @export
#'
#' @examples plot (1, 1, main =superscript_in_plots())

superscript_in_plots <- function(prefix='n', sup='k', suffix='') { # Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
  formatted_string = bquote(.(prefix)^.(sup)*.(suffix))
}


# #' subscript_in_plots2
# #'
# #' @param prefix String before the subscript. Use for boxplots, stripchart and co. Uses substitue() instead of bquote()
# #' @param subscr Subscripted text.
# #' @param quantity String in brackets after the subscript, eg.: log2(read count).
# #' @export
# #'
# #' @examples plot (1, 1, xlab =subscript_in_plots2(subscr = 10,quantity = "read count"), ylab =subscript_in_plots())

# subscript_in_plots2 <- function(prefix="log", subscr=2, quantity="arbitrary units") { # Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
#   formatted_string = substitute(paste(prefix[subscr],"(",quantity,")") , list(subscr=subscr, prefix=prefix, quantity=quantity))
# }
# #' superscript_in_plots2
# #'
# #' @param prefix String before the superscript.
# #' @param sup Superscripted text.
# #' @param suffix String after the subscript.
# #' @export
# #'
# #' @examples plot (1, 1, main =superscript_in_plots2())

# superscript_in_plots2 <- function(prefix='n', sup='k', suffix='') { # Returns a formatted string that you feed to main, xlab or ylab parameters of a plot
#   formatted_string = substitute(paste(prefix^sup,quantity) , list(prefix=prefix, sup=sup, suffix=suffix))
# }



#' filter_HP
#'
#' Filter values that fall between above high-pass-threshold (X >).
#' @param numeric_vector Values to be filtered.
#' @param threshold A numeric value above which "numeric_vector" passes.
#' @param passequal Pass if a value is larger, or equal than the threshold. FALSE by default.
#' @param prepend Text prepended to the results.
#' @param return_survival_ratio Return a number with the survival ratio (TRUE), or a logical index vector of the survivors (FALSE).
#' @examples filter_HP (numeric_vector =  , threshold =  , prepend =  , return_survival_ratio = F)
#' @export

filter_HP <- function(numeric_vector, threshold, passequal = F, prepend ="", return_survival_ratio=F) { # Filter values that fall between above high-pass-threshold (X >).
  survivors <- if (passequal) { numeric_vector >= threshold } else { numeric_vector > threshold }
  pc = percentage_formatter(sum(survivors)/length(survivors))
  conclusion = kollapse(prepend, pc, " or ", sum(survivors), " of ",length(numeric_vector)," entries in ", substitute (numeric_vector)," fall above a threshold value of: ", iround(threshold))
  if (file.exists(path_of_report) ) {	llogit (conclusion)} else { print  ("NOT LOGGED") }
  if (return_survival_ratio) {return (sum(survivors)/length(survivors))} else if (!return_survival_ratio) { return (survivors) }
}


#' filter_LP
#'
#' Filter values that fall below the low-pass threshold (X <).
#' @param numeric_vector Values to be filtered.
#' @param threshold A numeric value below which "numeric_vector" passes.
#' @param passequal Pass if a value is smaller, or equal than the threshold. FALSE by default.
#' @param prepend Text prepended to the results.
#' @param return_survival_ratio Return a number with the survival ratio (TRUE), or a logical index vector of the survivors (FALSE).
#' @examples filter_LP (numeric_vector =  , threshold =  , prepend =  , return_survival_ratio = F)
#' @export

filter_LP <- function(numeric_vector, threshold, passequal = F, prepend ="", return_survival_ratio=F) { # Filter values that fall below the low-pass threshold (X <).
  survivors <- if (passequal) { numeric_vector <= threshold } else { numeric_vector < threshold }
	pc = percentage_formatter(sum(survivors)/length(survivors))
	conclusion = kollapse(prepend, pc, " or ", sum(survivors), " of ",length(numeric_vector)," entries in ", substitute (numeric_vector)," fall below a threshold value of: ", iround(threshold))
	if (file.exists(path_of_report) ) {	llogit (conclusion)	} else { print  ("NOT LOGGED") }
	if (return_survival_ratio) {return (sum(survivors)/length(survivors))} else if (!return_survival_ratio) { return (survivors) }
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
#' @examples filter_MidPass (numeric_vector =  , HP_threshold =  , LP_threshold =  , prepend =  , return_survival_ratio = FALSE, EdgePass = F)
#' @export

filter_MidPass <- function(numeric_vector, HP_threshold, LP_threshold, prepend ="", return_survival_ratio=FALSE, EdgePass = F) { # Filter values that fall above high-pass-threshold !(X >=)! and below the low-pass threshold (X <).
	survivors = ( numeric_vector >= HP_threshold & numeric_vector < LP_threshold); keyword = "between"; relation = " <= x < "
	if (EdgePass) {survivors = ( numeric_vector < HP_threshold | numeric_vector >= LP_threshold); keyword = "outside"; relation = " >= x OR x > " }
	pc = percentage_formatter(sum(survivors)/length(survivors))
	conclusion = kollapse(prepend, pc, " or ", sum(survivors), " of ",length(numeric_vector)," entries in ", substitute (numeric_vector)," fall ", keyword, " the thresholds: ", iround(HP_threshold), relation, iround(LP_threshold) )
	if (file.exists(path_of_report) ) {	llprint (conclusion)	} else { print  ("NOT LOGGED") }
	if (return_survival_ratio) {return (sum(survivors)/length(survivors))} else if (!return_survival_ratio) { return (survivors) }
}



#' Print a list, one element per line,  into your markdown report
#'
#' @param your list
#' @examples llwrite_list(your_list)
#' @export 
#' 
llwrite_list <- function(yalist) {
  for (e in 1:l(yalist)) {
    if (is.null( names(yalist) )) { llprint("#####",names(yalist)[e]) } else { llprint("#####", e)}
    print(yalist[e]); llogit("`", yalist[e], "`")
  }
}




#' Add a legend, and save the plot immediately
#'
#' @param x location of legend
#' @param legend Text
#' @param fill Color
#' @param bty Background of legend, transparent by default
#' @param OverwritePrevPDF Save the plot immediately with the same name the last wplot* function made (It is stored in plotnameLastPlot variable).
#' @param ... Pass any other parameter of the corresponding text function (most of them should work).
#' @examples wlegend(...)
#' @export 
#' 

wlegend <- function(x="bottomleft", legend, fill = NULL, ..., bty = "n", OverwritePrevPDF =T) { # Add a legend, and save the plot immediately
  legend(x=x,legend=legend,fill=fill, ..., bty=bty)
  if (OverwritePrevPDF) {   wplot_save_this(plotname = plotnameLastPlot)  }
}