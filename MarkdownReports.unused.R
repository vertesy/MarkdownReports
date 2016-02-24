######################################################################
# Funcitons to Generate Markdown (html) Reports with R
######################################################################

# "Unused version:"
# MarkDown_Img_Logger_PDF_and_PNG <-  function(fname_wo_ext) { # insert 2 links, one for PDF, one for PNG version of the same image (png files are needed for web or email sharing!!!)
# 	splt = strsplit(fname_wo_ext,"/"); fn = splt[[1]][l(splt[[1]])] # Split and select the trailing file name
# 	llogit(kollapse ('![', fn, ']', '(', fname_wo_ext,'.pdf)',  print=F))
# 	llogit(kollapse ('![', fn, ']', '(', fname_wo_ext,'.png)',  print=F))	# link to a png file to use locally
# }


# "Unused version:"
# MarkDown_Img_Logger_4GitHub <-  function(fname_wo_ext) { # insert 2 links, one for PDF, one for PNG version of the same image (png files are needed for web or email sharing!!!)
# 	splt = strsplit(fname_wo_ext,"/"); fn = splt[[1]][l(splt[[1]])] # Split and select the trailing file name
# 	llogit(kollapse ('![', fn, ']', '(Reports/',fn ,'.png)',  print=F))
# }

# "Unused version:"
# MarkDown_Img_Logger_PDF_and_PNG_GitHub <-  function (FullPathtoPDF) {
# 	# MarkDown_Img_Logger_PDF_and_PNG_GitHub is a function to put a png and a pdf link to .md log file.
# 	print("Link to both pdf and png versions are created in the .md logfile.")
# 	trunk = strsplit(FullPathtoPDF, "\\.pdf")[[1]]
# 	splt = strsplit(trunk,"/"); fname_wo_ext = splt[[1]][l(splt[[1]])] # Split and select the trailing file name
# 	llogit(kollapse ('![]', '(', trunk,'.pdf)',  print=F))
# 	if (exists("png4Github") & png4Github ==T ) { 	dirnm = strsplit(OutDir, split = "/")[[1]]; dirnm = dirnm[length(dirnm)]
# 													llogit(kollapse ('![]', '(Reports/' ,dirnm,'/', fname_wo_ext,'.png)',  print=F))	# link to a png file that needs to be uploaded to GitHub-wiki under Reports/Folder-of-the-report/
# 	} else { 	llogit(kollapse ('![', fname_wo_ext, ']', '(', trunk,'.png)',  print=F))} 	# link to png 4 local use
# }


# MarkDown_Table_writer_DF_RowColNames <-  function (df, FnP=Log_PnF, percentify =FALSE, title_of_table = NA) {
# 	if (is.na(title_of_table)) { t = substitute(df) } else {t = title_of_table} 			# Format title of table
# 	title_of_table = paste("\n#### ", t)
# 	write ( title_of_table, Log_PnF, append=T)
# 	h =	paste(colnames(df), collapse = " \t| ") 			# Format header
# 	h = paste ("\n| |", h, " |",collapse = "")
# 	ncolz = dim(df)[2]+1; nrows = dim(df)[1]
# 	rn =  rownames (df)
# 	sep = kollapse(rep("| ---", ncolz)," |", print=F)
# 	if (exists("Log_PnF") ) {		write ( h, Log_PnF, append=T); write ( sep, Log_PnF, append=T)
# 		for (r in 1:nrows){
# 			if (is.numeric(unlist(df[r,])))  { 	b = iround(df[r,]) 			# Round Nr-s
# 			if(percentify) { b =percentage_formatter(b)} 		# make %
# 			} else { b = df[r,]}
# 			b = paste ( unlist(b), collapse = " \t| ") 						# Format table body
# 			# This looked errorous: I needed to transpose it to make it work. Why not as a simple vector?
# 			# b = paste ( as.data.frame(b), collapse = " \t| ") 						# Format table body
# 			b = paste ("|", rn[r], "\t|", b, " |",collapse = "")
# 			write ( b, Log_PnF, append=T)
# 		} # for
# 	}
# 	else {		print("NOT LOGGED: Log path and filename is not defined in Log_PnF")	} # if cannot print
# }


# MarkDown_Table_writer_NamedVector <- function (NamedVector, FnP=Log_PnF, percentify =FALSE, title_of_table = NA) {
# 	if (is.na(title_of_table)) { t = substitute(NamedVector) } else {t = title_of_table} 			# Format title of table
# 	title_of_table = paste("\n#### ", t)
# 	write ( title_of_table, Log_PnF, append=T)
# 	if (!is.table(NamedVector)) {if (is.numeric(NamedVector)) {NamedVector = iround(NamedVector)}}
# 	h =	paste(names(NamedVector), collapse = " \t| ") 			# Format header
# 	h = paste ("\n| ", h, " |",collapse = "")
# 	ncolz = l(NamedVector)
# 	sep = kollapse(rep("| ---", ncolz)," |", print=F)
# 	if (exists("Log_PnF") ) {
# 		write ( h, Log_PnF, append=T)
# 		write ( sep, Log_PnF, append=T)
# 		if(percentify & is.numeric(NamedVector)) { NamedVector =percentage_formatter(NamedVector)} 		# make %
# 		b = paste ( NamedVector, collapse = " \t| ") 			# Format table body
# 		b = paste ("|", b, " |",collapse = "")
# 		write ( b, Log_PnF, append=T)
# 	} else {		print("NOT LOGGED: Log path and filename is not defined in Log_PnF")	} # if cannot print
# }
