######################################################################
# Funcitons to Generate Markdown (html) Reports with R
######################################################################
# source ("/Users/abelvertesy/MarkDownLogs/MarkDownLogger_functions.R")
# Use MOU or alternatives to view and edit your reports

# Auxiliary fun -------------------------------------------------------------------------------------------------
kollapse <- function(...,print =T) { # parses (and prints) flexibly anything you pass on to it into a string
	if (print==T) {print (paste(c(...), sep="",collapse="")) }
	paste(c(...), sep="",collapse="")
}

any_print <- function(...) { # more flexible printing fun
	argument_list <- c(...)
	print (
		paste( argument_list, collapse=" ")
	)
} # any_print (1,2,"macska")

iround  <- function (x, digz = 3) {signif (x, digits=digz)}

percentage_formatter <- function(x, digitz=3) {
	a = paste (100*iround(x, digitz),"%", sep = " ")
	a[a == "NaN %"] = NaN; 	a[a == "NA %"] = NA
	return(a)
}

# Setup Logging -------------------------------------------------------------------------------------------------
create_set_OutDir <- function (...) {
	OutDir = kollapse(..., print=F)
	print (OutDir)
	if ( !exists (OutDir) ) {dir.create(OutDir)}
	assign ("OutDir", OutDir, envir = .GlobalEnv)
}

setup_logging_markdown <- function  (fname, append=T) { # setup_logging file, path and modification date
	if ( exists('OutDir') ) { path = OutDir } else { path = getwd() ; any_print ("OutDir not defined !!!") }
	Log_PnF <- kollapse (path,'/',fname,'.log.md')
	write (kollapse("# ", fname," report"), Log_PnF , append=append)
	write (kollapse("                   Modified: ",date() ), Log_PnF , append=append)
	assign ("Log_PnF",Log_PnF, envir = .GlobalEnv)
	OutImg = kollapse(OutDir,"/",substr(fname, 1, (nchar(fname)-2)),format(Sys.time(), "%Y_%m_%d-%Hh"), print=F)
	if ( !exists (OutImg) ) {dir.create(OutImg); assign ("OutImg", OutImg, envir = .GlobalEnv)}
}

continue_logging_markdown  <- function  (fname) { # continue writing to an existing markdown file
	if ( exists('OutDir') ) { path = OutDir } else { path = getwd() ; any_print ("OutDir not defined !!!") }
	Log_PnF <- kollapse (path,'/',fname,'.log.md', print = F)
	return (Log_PnF)
	OutImg = kollapse(OutDir,"/",substr(fname, 1, (nchar(fname)-2)),format(Sys.time(), "%Y_%m_%d-%Hh"), print=F)
	if ( !exists (OutImg) ) {dir.create(OutImg); assign ("OutImg", OutImg, envir = .GlobalEnv)}
	return (OutImg)
}

# Log into your markdown files -------------------------------------------------------------------------------------------------
log_settings_MarkDown <- function(...) { # log your parameter settings into a tabular format
	call <- match.call();
	namez = sapply(as.list(call[-1]), deparse)
	value = c(...)
	value = as.data.frame(value)
	rownames (value) = namez
	MarkDown_Table_writer_DF_RowColNames((value), title_of_table = "Settings")
}

llprint <- function  (...) { # log to markdown file and print to screen
	argument_list <- c(...)
	LogEntry = print ( 		paste( argument_list, collapse=" ")  	)
	if (exists("Log_PnF") ) {		write ( kollapse ("\n", LogEntry, print=F), Log_PnF, append=T) 	}
	else {		print("NOT LOGGED: Log path and filename is not defined in Log_PnF")	} # if cannot print
}

llogit <- function  (...) { # log to markdown file, do not print
	argument_list <- c(...)
	LogEntry = paste( argument_list, collapse=" ") 			# collapse by space
	LogEntry = gsub('^ +| +$', "", LogEntry) 				# remove trailing spaces
	if (!exists("Log_PnF") ) {print("Log path and filename is not defined in Log_PnF")}
	write ( kollapse ("\n", LogEntry, print=F), Log_PnF, append=T)
}

MarkDown_ImgLink_formatter <-  function (...) { # insert a link to a pdf image
	FnP =kollapse(..., print=F)
	splt = strsplit(FnP,"/"); fn = splt[[1]][l(splt[[1]])] # Split and select the trailing file name
	kollapse ('![', fn, ']', '(', FnP,')',  print=F)
}

MarkDown_Img_Logger_PDF_and_PNG <-  function (fname_wo_ext) { # insert 2 links, one for PDF, one for PNG version of the same image (png files are needed for web or email sharing!!!)
	splt = strsplit(fname_wo_ext,"/"); fn = splt[[1]][l(splt[[1]])] # Split and select the trailing file name
	log_it(kollapse ('![', fn, ']', '(', fname_wo_ext,'.pdf)',  print=F))
	log_it(kollapse ('![', fn, ']', '(', fname_wo_ext,'.png)',  print=F))
}

# Write out pretty tables to your markdown file ------------------------------------------------------------------------------------------------------------
MarkDown_Table_writer_DF_RowColNames <-  function (df, FnP=Log_PnF, percentify =F, title_of_table = NA) {
	if (is.na(title_of_table)) { t = substitute(df) } else {t = title_of_table} 			# Format title of table
	title_of_table = paste("\n#### ", t)
	write ( title_of_table, Log_PnF, append=T)
	h =	paste(colnames(df), collapse = " \t| ") 			# Format header
	h = paste ("\n| |", h, " |",collapse = "")
	ncolz = dim(df)[2]+1; nrows = dim(df)[1]
	rn =  rownames (df)
	sep = kollapse(rep("| ---", ncolz)," |", print=F)
	if (exists("Log_PnF") ) {		write ( h, Log_PnF, append=T); write ( sep, Log_PnF, append=T)
		for (r in 1:nrows){
			if (is.numeric(unlist(df[r,])))  { 	b = iround(df[r,]) 			# Round Nr-s
				if(percentify) { b =percentage_formatter(b)} 		# make %
				} else { b = df[r,]}
			b = paste ( b, collapse = " \t| ") 						# Format table body
			b = paste ("|", rn[r], "\t|", b, " |",collapse = "")
			write ( b, Log_PnF, append=T)
		} # for
	}
	else {		print("NOT LOGGED: Log path and filename is not defined in Log_PnF")	} # if cannot print
}

MarkDown_Table_writer_NamedVector <- function (NamedVector, FnP=Log_PnF, percentify =F, title_of_table = NA) {
	if (is.na(title_of_table)) { t = substitute(NamedVector) } else {t = title_of_table} 			# Format title of table
	title_of_table = paste("\n#### ", t)
	write ( title_of_table, Log_PnF, append=T)
	if (!is.table(NamedVector)) {if (is.numeric(NamedVector)) {NamedVector = iround(NamedVector)}}
	h =	paste(names(NamedVector), collapse = " \t| ") 			# Format header
	h = paste ("\n| ", h, " |",collapse = "")
	ncolz = l(NamedVector)
	sep = kollapse(rep("| ---", ncolz)," |", print=F)
	if (exists("Log_PnF") ) {
		write ( h, Log_PnF, append=T)
		write ( sep, Log_PnF, append=T)
		if(percentify & is.numeric(NamedVector)) { NamedVector =percentage_formatter(NamedVector)} 		# make %
		b = paste ( NamedVector, collapse = " \t| ") 			# Format table body
		b = paste ("|", b, " |",collapse = "")
		write ( b, Log_PnF, append=T)
	} else {		print("NOT LOGGED: Log path and filename is not defined in Log_PnF")	} # if cannot print
}

# Generate and save plots into pdf and insert a diplay-link into your markdown file -------------------------------------------------------------------------------------------------
wplot <-  function(variable, col ="gold1", ..., w=7, h=7,  plotname = substitute(variable), mdlink =F) {
	fname = kollapse (plotname, '.plot')
	plot (variable, ..., main=plotname, col=col)
	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

whist <-  function(variable, col ="gold1", w=7, h=7, plotname = substitute(variable), breaks = 20,
	main=kollapse("Histogram of ", substitute(variable)) , mdlink =F, ... ) {
	# name the file  by naming the variable! Cannot be used with dynamically called variables [e.g. call vectors within a loop]
	if ( length (variable) > 0 ) {
		fname = kollapse (plotname, '.hist')
		if ( !is.numeric(variable)) { variable = table (variable) ;
									cexNsize = 0.7/abs (log10 (length(variable)) ); cexNsize = min (cexNsize, 1)
									barplot (variable, ..., main=main, xlab=plotname, col=col, las=2, cex.names = cexNsize,
											 sub = paste ("mean:", iround(mean(variable, na.rm=T)),  "CV:", percentage_formatter(cv(variable)) ) )
		} else {
			hist (variable, ..., main = main, breaks = breaks, xlab=plotname, col=col, las=2)
		} # if is.numeric
		dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	} else { any_print (variable," IS EMPTY") } # if non empty
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

wbarplot <-  function(variable, col ="gold1", w=7, h=7, sub = F, ..., plotname = substitute(variable), main =substitute(variable), mdlink =F) {
	# in ... you can pass on ANY plotting parameter exc SUB, MAIN!!!!
	fname = kollapse (plotname, '.barplot')
		cexNsize = 0.7/abs (log10 (length(variable)) ); cexNsize = min (cexNsize, 1)
	if (sub==T) { 		subtitle = paste ("mean:", iround(mean(variable, na.rm=T)),  "CV:", percentage_formatter(cv(variable)) )
	} else if (sub==F) { subtitle="" } else { subtitle=sub }
	barplot (variable, ..., main=main, col=col, las=2, cex.names = cexNsize,
		sub = subtitle)
	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

wboxplot <-  function(variable, col ="gold1", ..., w=7, h=7,  plotname = as.character (substitute(variable)), sub=F, mdlink =F) {
	# in ... you can pass on ANY plotting parameter!!!!
	fname = kollapse (plotname, '.boxplot')
	boxplot (variable, ..., main=plotname, col=col, las=2)
	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

wpie <-  function(variable, percentage =T, ..., w=7, h=7, plotname = substitute(variable), mdlink =F) {
	# if (!is.vector(variable)) {any_print ("The input is not a vector, but coverted! Dim:", dim (variable)); cc = variable[,2]; names (cc) = variable[,1]; variable =cc}
	fname = kollapse (plotname, '.pie')
	subt = kollapse ("Total = ",sum(variable), print=F)
	if (percentage) {
		labs<- paste("(",names(variable),")", "\n", percentage_formatter(variable/sum(variable)), sep="")
	} else {
		labs<- paste("(",names(variable),")", "\n", variable, sep="")
		}
	pie (variable, ..., main=plotname, sub = subt, clockwise = T, labels = labs)
	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) } # put a markdown image link if the log file exists
}

# save only the currenlty active graphic device (for compliacted plots)
wplot_save_this <-  function(plotname = date(), col ="gold1", ..., w=7, h=7, mdlink =F, ManualName = F) {
	fname = kollapse (plotname, '.plot'); if (ManualName) {fname = plotname}
	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) } # put a markdown image link if the log file exists
}
