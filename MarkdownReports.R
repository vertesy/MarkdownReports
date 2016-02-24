######################################################################
# Funcitons to Generate Markdown (html) Reports with R
######################################################################
# source ("/Users/abelvertesy/MarkdownReports/MarkdownReports.R")
# Use MOU or alternatives to view and edit your reports

## Auxiliary functions -------------------------------------------------------------------------------------------------
kollapse <- function(..., print =T) {
	if (print==T) {print (paste0(c(...), collapse = "")) }
	paste0(c(...), collapse = "")
}

any_print <- function(...) { # more flexible printing fun
	argument_list <- c(...)
	print (
		paste( argument_list, collapse=" ")
	)
} # any_print (1,2,"macska")

iround <- function(x, digz = 3) {signif (x, digits=digz)}

percentage_formatter <- function(x, digitz=3) {
	a = paste (100*iround(x, digitz),"%", sep = " ")
	a[a == "NaN %"] = NaN; 	a[a == "NA %"] = NA
	return(a)
}

## Setup Logging -------------------------------------------------------------------------------------------------
create_set_OutDir <- function(...) {
	OutDir = kollapse(..., print=F)
	print (OutDir)
	if ( !exists (OutDir) ) {dir.create(OutDir)}
	assign ("OutDir", OutDir, envir = .GlobalEnv)
}

# setup_logging file, path and modification date
setup_logging_markdown <- function(fname, title="", append=T, png4Github = T) {
	if ( exists('OutDir') ) { path = OutDir } else { path = getwd() ; any_print ("OutDir not defined !!!") }
	Log_PnF <- kollapse (path,'/',fname,'.log.md')
	if (nchar(title)) { write (paste("# ", title), Log_PnF , append=append)
	} else { 			 write (paste("# ", fname,"Report"), Log_PnF , append=append) }
	write (kollapse("        Modified: ",format(Sys.time(), "%d/%m/%Y | %H:%M | by: "), fname ), Log_PnF , append=T)
	OutImg = kollapse(OutDir,"/",substr(fname, 1, nchar(fname)),"_",format(Sys.time(), "%Y_%m_%d-%Hh"), print=F)
	if ( !exists (OutImg) ) {dir.create(OutImg); assign ("OutImg", OutImg, envir = .GlobalEnv)}
	assign ("Log_PnF",Log_PnF, envir = .GlobalEnv)
	assign ("png4Github",png4Github, envir = .GlobalEnv)
}

continue_logging_markdown <- function(fname) {
	if ( exists('OutDir') ) { path = OutDir } else { path = getwd() ; any_print ("OutDir not defined !!!") }
	Log_PnF <- kollapse (path,'/',fname,'.log.md', print = F)
	return (Log_PnF)
	OutImg = kollapse(OutDir,"/",substr(fname, 1, (nchar(fname)-2)),format(Sys.time(), "%Y_%m_%d-%Hh"), print=F)
	if ( !exists (OutImg) ) {dir.create(OutImg); assign ("OutImg", OutImg, envir = .GlobalEnv)}
	return (OutImg)
}

## Write into your markdown log file -------------------------------------------------------------------------------------------------
log_settings_MarkDown <- function(...) { # log your parameter settings into a tabular format
	call <- match.call();
	namez = sapply(as.list(call[-1]), deparse)
	value = c(...)
	value = as.data.frame(value)
	rownames (value) = namez
	MarkDown_Table_writer_DF_RowColNames((value), title_of_table = "Settings")
}

llprint <- function(...) { # log to markdown file and print to screen
	argument_list <- c(...)
	LogEntry = print ( 		paste( argument_list, collapse=" ")  	)
	if (exists("Log_PnF") ) {		write ( kollapse ("\n", LogEntry, print=F), Log_PnF, append=T) 	}
	else {		print("NOT LOGGED: Log path and filename is not defined in Log_PnF")	} # if cannot print
}

llogit <- function(...) { # log to markdown file, do not print
	argument_list <- c(...)
	LogEntry = paste( argument_list, collapse=" ") 			# collapse by space
	LogEntry = gsub('^ +| +$', "", LogEntry) 				# remove trailing spaces
	if (!exists("Log_PnF") ) {print("Log path and filename is not defined in Log_PnF")}
	write ( kollapse ("\n", LogEntry, print=F), Log_PnF, append=T)
}

MarkDown_ImgLink_formatter <- function(...) { # insert a link to a pdf image
	FnP =kollapse(..., print=F)
	splt = strsplit(FnP,"/"); fn = splt[[1]][l(splt[[1]])] # Split and select the trailing file name
	kollapse ('![', fn, ']', '(', FnP,')',  print=F)
}

MarkDown_Img_Logger_PDF_and_PNG <- function (fname_wo_ext) {
	splt = strsplit(fname_wo_ext,"/"); fn = splt[[1]][l(splt[[1]])] # Split and select the trailing file name
	llogit(kollapse ('![]', '(', fname_wo_ext,'.pdf)',  print=F))
	if (exists("png4Github") & png4Github ==T ) { 	dirnm = strsplit(OutDir, split = "/")[[1]];dirnm = dirnm[length(dirnm)]
													llogit(kollapse ('![]', '(Reports/' ,dirnm,'/', fname_wo_ext,'.png)',  print=F))	# link to a png file to use locally
	} else { 	llogit(kollapse ('![', fn, ']', '(', fname_wo_ext,'.png)',  print=F))} 					# link to png 4 local use
}

## Write out pretty tables to your markdown file ------------------------------------------------------------------------------------------------------------
MarkDown_Table_writer_DF_RowColNames <- function(df, FnP=Log_PnF, percentify =F, title_of_table = NA) {
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

MarkDown_Table_writer_NamedVector <- function(NamedVector, FnP=Log_PnF, percentify =F, title_of_table = NA) {
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

## Generate and save plots into pdf and insert a diplay-link into your markdown file -------------------------------------------------------------------------------------------------
"Known issue: Cannot define xlim with error.bars"
wplot <- function(df_2columns, col =1, pch = 18, ...,
				  w=7, h=7,  plotname = substitute(df_2columns), mdlink =F, log4GitHuB = F,
				  errorbar = F, upper = 0, lower=upper, left = 0, right = left,  width=0.1, arrow_lwd =1,
				  abline = F, a = F, b =F, lty =1, lwd =1, lcol =1 ) { # Create and save scatter plots, add error bars or lines (ablines) to your plot.
	# abline = F, "h", "v", "ab"
	x = df_2columns[ ,1]; y = df_2columns[ ,2];
	fname = kollapse (plotname, '.plot')
	if (errorbar) { # increase plot boundaries so that error bars fit
		ylim= range(c(0, (y+upper+abs(.1*y) ), (y-lower-abs(.1*y) ) ) )
		xlim= range(c(0, (x+right+abs(.1*x) ), (1.1*x-left-abs(.1*x) ) ) ) } else { 	ylim = range (y); xlim = range (x) }
	plot (df_2columns, ..., main=plotname, col=col, pch = pch, ylim = ylim, xlim = xlim)

	if (errorbar) {
		arrows( x0 = x, y0 = y+upper, x1 = x, y1 = y-lower, angle=90, code=3, length=width, lwd = arrow_lwd)  # vertical error bars
		arrows( x0 = x+left, y0 = y, x1 = x-right, y1 = y, angle=90, code=3, length=width, lwd = arrow_lwd) } # horizontal error bars

	if (abline == "h")  { abline(h=a, lty = lty, lwd = lwd, col = lcol)}
	if (abline == "v")  { abline(v=a, lty = lty, lwd = lwd, col = lcol)}
	if (abline == "ab") { abline(a = a, b = b, lty = lty, lwd = lwd, col = lcol)}

	assign ("plotnameLastPlot", fname, envir = .GlobalEnv)
	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

## Save the currenlty active graphic device (for compliacted plots)
wplot_save_this <- function(plotname = date(), col ="gold1", ..., w=7, h=7, mdlink =FALSE, ManualName = FALSE) {
	if (plotname == plotnameLastPlot) { ManualName = T }
	fname = kollapse (plotname, '.plot'); if (ManualName) {fname = plotname}
	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) } # put a markdown image link if the log file exists
}

whist <- function(variable, col ="gold1", w=7, h=7, plotname = substitute(variable), breaks = 20,
				   main=kollapse("Histogram of ", substitute(variable)), xlabel =substitute(variable), mdlink =FALSE, log4GitHuB = TRUE,
				   hline=F, vline=F,lty =2, lwd =3, lcol =2, filtercol = 0,...) {
	# name the file  by naming the variable! Cannot be used with dynamically called variables [e.g. call vectors within a loop]
	# filtercol assumes  >= coloring!
	xtra =  list (...)
	if ( length (variable) > 0 ) {
		fname = kollapse (plotname, '.hist')
		if ( !is.numeric(variable)) {
			variable = table (variable) ;
			cexNsize = 0.7/abs (log10 (length(variable)) ); cexNsize = min (cexNsize, 1)
			barplot (variable, ..., main=main, xlab=xlabel, col=col, las=2, cex.names = cexNsize,
					 sub = paste ("mean:", iround(mean(variable, na.rm=T)),  "CV:", percentage_formatter(cv(variable)) ) )
		} else {
			histdata = hist(variable, breaks =breaks, plot = F)
			if(filtercol == 1) { 		col = (histdata$breaks >=vline)+2 }
			else if(filtercol == -1) { 	col = (histdata$breaks <vline)+2 }
			hist (variable, ..., main = main, breaks = breaks, xlab=xlabel, col=col, las=2)
		} # if is.numeric
		if (hline) { abline (h = hline, lty =lty, lwd = lwd, col = lcol) }
		if (vline & !l(xtra$xlim) ) {
			PozOfvline = mean(histdata$mids[c(max(which(histdata$breaks<vline)), min(which(histdata$breaks>=vline)))])   # this is complicated, i know...
			abline (v = PozOfvline, lty =lty, lwd = lwd, col = 1)
		} else if (vline & l(xtra$xlim)) {	abline (v = vline, lty =lty, lwd = lwd, col = 1)}
		dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	} else { any_print (variable," IS EMPTY") } # if non empty
	assign ("plotnameLastPlot", fname, envir = .GlobalEnv)
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

wbarplot <- function(variable, ..., col ="gold1", sub = F, plotname = substitute(variable), main =substitute(variable),
					  w=7, h=7, incrBottMarginBy = 0, mdlink =F, tilted_text =F,
					  hline=F, vline=F, filtercol=1,lty =1, lwd =2, lcol =2,
					  errorbar = F, upper = 0, lower=upper, width=0.1, arrow_lwd =1 ) {
	fname = kollapse (plotname, '.barplot')
	.ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) 	# Tune the margin
	cexNsize = .8/abs (log10 (length(variable)) ); cexNsize = min (cexNsize, 1)

	if (sub==T) { 		subtitle = paste ("mean:", iround(mean(variable, na.rm=T)),  "CV:", percentage_formatter(cv(variable)) )
	} else if (sub==F) { subtitle="" } else { subtitle=sub }

	if (hline & filtercol == 1 ) { col = (variable>=hline)+2 } # change color, if horizontal threshold is defined. (vertical threshold makes only sense in a histogram)
	if (hline & filtercol == -1) { col = (variable <hline)+2 }
	if (errorbar) { ylim= range(c(0, (variable+upper+abs(.1*variable)), variable-lower-abs(.1*variable)), na.rm = T) }  else { ylim = range (0,variable)} # increase ylim so that error bars fit
	if (tilted_text) { xlb = NA } else { xlb= names (variable) }

	x= barplot (variable, ..., names.arg = xlb, main=main, sub = subtitle, col=col, las=2, cex.names = cexNsize, ylim=ylim	) # xaxt="n",
	if (hline) { abline (h = hline, lty =lty, lwd = lwd, col = lcol) }
	if (vline) { abline (v = vline, lty =lty, lwd = lwd, col = lcol) }
	if (errorbar) {  	arrows(x, variable+upper, x, variable-lower, angle=90, code=3, length=width, lwd = arrow_lwd, ...) }
	if (tilted_text) { text(x=x-.25, y=-max(nchar(names(variable))) / 5, labels = names(variable), xpd=TRUE, srt=45, cex=cexNsize) } # 45 degree labels; y determines the -offset based on the nr of characters in the label

	dev.copy2pdf (file= FnP_parser (fname, 'pdf'), width=w, height=h)
	par("mar" = .ParMarDefault)
	assign ("plotnameLastPlot", fname, envir = .GlobalEnv)
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

wboxplot <- function(variable, ...,  col ="gold1", plotname = as.character (substitute(variable)), sub=FALSE,
					  incrBottMarginBy = 0, tilted_text =F, w=7, h=7, mdlink =F) {
	# in ... you can pass on ANY plotting parameter!!!!
	fname = kollapse (plotname, '.boxplot')
	.ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) 	# Tune the margin
	if (tilted_text) { xlb = NA } else { xlb= names (variable) }

	boxplot (variable, ..., names = xlb, main=plotname, col=col, las=2)
	if (tilted_text) { text(x=1:l(variable), y=-max(nchar(names (variable))) / 2, labels = names (variable), xpd=TRUE, srt=45) } # 45 degree labels; y determines the -offset based on the nr of characters in the label

	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	assign ("plotnameLastPlot", fname, envir = .GlobalEnv)
	par("mar" = .ParMarDefault)
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

wpie <- function(variable, ..., percentage =TRUE, plotname = substitute(variable), w=7, h=7, mdlink =F) {
	# if (!is.vector(variable)) {any_print ("The input is not a vector, but coverted! Dim:", dim (variable)); cc = variable[,2]; names (cc) = variable[,1]; variable =cc}
	fname = kollapse (plotname, '.pie')
	subt = kollapse ("Total = ",sum(variable), print=F)
	if (percentage) {
		labs<- paste("(",names(variable),")", "\n", percentage_formatter(variable/sum(variable)), sep="")
	} else {
		labs<- paste("(",names(variable),")", "\n", variable, sep="")
	}
	pie (variable, ..., main=plotname, sub = subt, clockwise = T, labels = labs, col = rainbow(l(variable)))
	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) } # put a markdown image link if the log file exists
}

wstripchart <-  function(yalist, ..., plotname = as.character (substitute(yalist)), sub=FALSE, border=1, BoxPlotWithMean =F,
						  pch=23, pchlwd =1, pchcex=1.5, bg="chartreuse2", col ="black", metod = "jitter", jitter = 0.2, colorbyColumn=F, # metod = "jitter" OR "stack"
						  w=7, h=7, incrBottMarginBy = 0, tilted_text =F, mdlink =F) {
	.ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) 	# Tune the margin
	cexNsize = 1/abs (log10 (length(yalist)) ); cexNsize = min (cexNsize, 1)
	fname = kollapse (plotname, '.stripchart')
	a =boxplot(yalist, plot=F)
	if (colorbyColumn) { pchlwd =5; pchcex=.5	}
	if (BoxPlotWithMean){		a$stats[3,] = unlist(lapply(yalist, mean)) }						# Replace mean with median
	if (tilted_text) { xlb = F } else { xlb= T }

	bxp(a, xlab ="", show.names = xlb, ..., main =plotname, border=border, outpch = NA, las=2, outline=T, cex.axis = cexNsize)
	stripchart(yalist, vertical = TRUE, add = TRUE, method = metod, jitter =jitter
			   , pch=pch, bg=bg, col=col, lwd =pchlwd, cex=pchcex)

	if (tilted_text) { text(x=1:l(yalist), y=-max(nchar(names (yalist))) / 2, labels = names (yalist), xpd=TRUE, srt=45) } # 45 degree labels; y determines the -offset based on the nr of characters in the label

	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	par("mar" = .ParMarDefault)
	assign ("plotnameLastPlot", fname, envir = .GlobalEnv)
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

# here you can define everything
wstripchart_list <-  function(yalist, ..., plotname = as.character (substitute(yalist)), sub=FALSE, ylb = NULL, xlab =NULL, border=1, bxpcol =0,
							   pch=23, pchlwd =1, pchcex=1.5, bg="chartreuse2", coll ="black", metod = "jitter", jitter = 0.2, # metod = "jitter" OR "stack"
							   w=7, h=7, incrBottMarginBy = 0, tilted_text =F, mdlink =F) {
	fname = kollapse (plotname, '.stripchart')
	.ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) 	# Tune the margin
	cexNsize = 1/abs (log10 (length(list)) ); cexNsize = min (cexNsize, 1)
	if (tilted_text) { xlb = F } else { xlb= T }

	boxplot (yalist, ..., show.names = xlb, main=plotname, border=border, outline=FALSE, las=2, col=bxpcol, cex.axis = cexNsize)
	for (i in 1:length(yalist)) {
		j=k=i
		if (length(coll) < length(yalist)) {j=1}
		if (length(bg) < length(yalist)) {k=1}
		stripchart(na.omit(yalist[[i]]), at = i, add = T, vertical = T, method = metod, jitter =jitter, pch =pch, bg = bg[[k]], col=coll[[j]], lwd =pchlwd, cex=pchcex)
	}
	if (tilted_text) { text(x=1:l(yalist), y=-max(nchar(names (yalist))) / 2, labels = names (yalist), xpd=TRUE, srt=45) } # 45 degree labels; y determines the -offset based on the nr of characters in the label

	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	par("mar" = .ParMarDefault)
	assign ("plotnameLastPlot", fname, envir = .GlobalEnv)
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

wvioplot_list <-  function(yalist, ..., xlb = names(yalist), ylb ="", coll = c(1:length(yalist)), incrBottMarginBy = 0,
							w=7, h=7, plotname = as.character (substitute(yalist)), tilted_text =F, mdlink =F ) {
	require(vioplot)
	.ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) 	# Tune the margin
	l_list = length(yalist)
	fname = kollapse (plotname, '.vioplot')
	if (length(coll) < l_list) { coll = rep (coll, l_list)}
	if (tilted_text) { xlb = NA } else { xlb= names (yalist) }

	plot(0,0, type="n", xlim= c(.5, (l_list +.5)), ylim=range (unlist(yalist)),  xaxt = 'n', xlab ="", ylab = ylb, main = plotname)
	for (i in 1:l_list) { vioplot(na.omit(yalist[[i]]), ..., at = i, add = T, col = coll[i] ) }
	axis(side=1,at=1:l_list,labels=xlb, las=2)
	if (tilted_text) { text(x=1:l(yalist), y=-max(nchar(names (yalist))) / 2, labels = names (yalist), xpd=TRUE, srt=45) } # 45 degree labels; y determines the -offset based on the nr of characters in the label

	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	par("mar" = .ParMarDefault)
	assign ("plotnameLastPlot", fname, envir = .GlobalEnv)
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

wviostripchart_list <-  function(yalist, ..., pch=23, viocoll = 0, vioborder =1, ylb ="", plotname = as.character (substitute(yalist)), sub=F,
								  bg=0, coll ="black", metod = "jitter", jitter = 0.1,
								  w=7, h=7, incrBottMarginBy = 0, mdlink =F) { # , tilted_text =F
	# in ... you can pass on ANY plotting parameter!!!!
	# metod = "jitter" OR "stack"
	fname = kollapse (plotname, '.VioStripchart')
	require(vioplot)
	.ParMarDefault <- par("mar"); 	par(mar=c(par("mar")[1]+incrBottMarginBy, par("mar")[2:4]) ) 	# Tune the margin
	l_list = length(yalist)
	# if (tilted_text) { xlb = NA } else { xlb= names (yalist) }

	plot(0,0, type="n", xlim= c(.5, (l_list +.5)), ylim=range (unlist(yalist)),  xaxt = 'n', xlab ="", ylab = ylb, main = plotname)
	for (i in 1:l_list) { vioplot(na.omit(yalist[[i]]), ..., at = i, add = T, col = viocoll[i], border =vioborder[i] ) }
	for (i in 1:length(yalist)) {
		j=k=i
		if (length(coll) < length(yalist)) {j=1}
		if (length(bg) < length(yalist)) {k=1}
		stripchart(na.omit(yalist[[i]]), at = i, add = T, vertical = T, method = metod, jitter =jitter, pch =pch, bg = bg[[k]], col=coll[[j]])
	}
	# if (tilted_text) { text(x=1:l(yalist), y=-max(nchar(names (yalist))) / 2, labels = names (yalist), xpd=TRUE, srt=45) } # 45 degree labels; y determines the -offset based on the nr of characters in the label

	dev.copy2pdf (file=FnP_parser (fname, 'pdf'), width=w, height=h )
	par("mar" = .ParMarDefault)
	assign ("plotnameLastPlot", fname, envir = .GlobalEnv)
	if (mdlink) { 	MarkDown_Img_Logger_PDF_and_PNG (fname_wo_ext = fname) }# put a markdown image link if the log file exists
}

