## Function Overview
You find the list of function of this library below. For details, please use the `help()` function, or browse the [source code](). <br>


### 1. ttl_field
- Internal function. Creates the string written into the PDF files "Title' (metadata) field/

### kollapse
- Collapses values and strings to one string (without a white space). It also prints the results (good for a quick check)

### iprint
- A more intelligent printing function that collapses any variable passed to it by white spaces.

### iround
- Rounds a value to the significant amount of digits. Its a wrapper for signif().

### percentage_formatter
- Parse a string of 0-100% from a number between 0 and 1.

### setup_MarkdownReports
- Setup the markdown report file and the output directory, create a sub directory in "OutDir". Its name is stamped with the script name and the modification time. Create the "path_of_report" variable used by all log-writing and ~wplot functions.

### create_set_OutDir (deprecated, use with setup_logging_markdown, will be removed from V3)
- Create or set the output directory of the script, and set the "OutDir" variable that is used by all ~wplot functions.

### setup_logging_markdown (deprecated, use with create_set_OutDir, will be removed from V3)
- Setup the markdown report file, create a sub directory in "OutDir". Its name is stamped with the script name and the modification time. Create the "path_of_report" variable used by all log-writing and ~wplot functions.

### continue_logging_markdown
- Continue writing to an existing report file.

### log_settings_MarkDown
- Log the parameters & settings used in the script in a table format.

### llprint
- Collapse by white spaces a sentence from any variable passed on to the function. Print the sentence to the screen and write it to your markdown report file, if the "path_of_report" variable is defined.

### llogit
- Collapse by white spaces a sentence from any variable passed on to the function. llogit() writes it to your markdown report file, if the "path_of_report" variable is defined. It does not print the sentence to the screen.

### MarkDown_ImgLink_formatter
- Format a markdown image reference (link) from the file path to the file. It can parse the file path, if you pass it in separate variables and strings. E.g. MarkDown_ImgLink_formatter(Directory, "MyImage.png").

### MarkDown_Img_Logger_PDF_and_PNG
- Format a markdown image reference (link) to a .pdf and .png versions of graph, and insert both links to the markdown report, set by "path_of_report". If the "png4Github" variable is set, the .png-link is set up such, that you can upload the whole report with the .png image into your GitHub repo's wiki, under "Reports"/OutDir/ (Reports is a literal string, OutDir is the last/deepest directory name in the "OutDir" variable. See create_set_OutDir() function.). This function is called by the ~wplot functions.

### MarkDown_Table_writer_DF_RowColNames
- Take an R data frame with row- and column- names, parse a markdown table from it, and write it to the markdown report, set by "path_of_report".

### MarkDown_Table_writer_NamedVector
- Take an R vector with names, parse a markdown table from it, and write it to the markdown report, set by "path_of_report".

### wplot
- Create and save scatter plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions. The .png version is not created, only the link is put in place. You can add 2D error bars around the dots, or add lines (ablines) to your plot, by setting "abline" argument to = F (no line, default), "h" (horizontal, further specified by a = y-offset), "v" (vertical, further specified by a = x-offset), "ab" (line with an angle, further specified by a = offset, b = slope).

### wscatter.fill
-A scatterplot with color gradient and color legend. Modified from: http://stackoverflow.com/questions/20127282/r-color-scatterplot-points-by-col-value-with-legend

### ww_autoPlotName
- Internal function. Creates automatic plot and file-names.

### wplot_save_this
- Save the currently active graphic device (for complicated plots).  Insert links to your markdown report, set by "path_of_report". Name the file by naming the variable!

### whist
- Create and save histograms as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions. Name the file by naming the variable! Cannot be used with dynamically called variables [e.g. call vectors within a loop]. "filtercol" assumes  >= coloring!

### wbarplot
- Create and save bar plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.

### wboxplot
- Create and save box plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.

### wpie
- Create and save pie charts as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.

### wstripchart
- Create and save strip charts as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.

### wstripchart_list
- Create and save stripcharts from a list as .pdf, in "OutDir". This version allows individual coloring of each data point, by a color-list of the same dimension. If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.

### wvioplot_list
- Create and save violin plots as .pdf, in "OutDir". It requires (and calls) "vioplot" package. If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.

### wviostripchart_list
- Create and save violin plots as .pdf, in "OutDir". It requires (and calls) "vioplot" package. If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "path_of_report". The .png version is not created, only the link is put in place, not to overwrite previous versions.

### whist_dfCol
- Use this version of whist() if you iterate over columns  or rows of a data frame. You can name the file by naming the variable. Cannot be used with dynamically called variables [e.g. call vectors within a loop]

### whist.back2back
- Two back-to-back histograms from a list. The X-axis is only correct if  breaks1 ==breaks2. Undeveloped function, contains graphical bugs, no support for this function.

### wbarplot_dfCol
- wbarplot for a column of a data frame.

### val2col
- This function converts a vector of values("yourdata") to a vector of color levels. One must define the number of colors. The limits of the color scale("zlim") or the break points for the color changes("breaks") can also be defined. when breaks and zlim are defined, breaks overrides zlim. Source: http://menugget.blogspot.nl/2011/09/converting-values-to-color-levels.html

### wvenn
- Save venn diagrams. Unlike other ~vplot funcitons, this saves directly into a .png, and it does not use the dev.copy2pdf() function.

### error_bar
- Put error bars on top of your bar plots. This functionality is now integrated into MarkdownReporter's wbarplot() function

### barplot_label
- Add extra labels to your bar plots at the top or the base.

### subscript_in_plots
- Create an expression with subscript for axis labels. Parsed when provided to xlab or ylab of a function.

### superscript_in_plots
- Create an expression with superscript for axis labels. Parsed when provided to xlab or ylab of a function.

### filter_HP
- Filter values that fall between above high-pass-threshold (X >).

### filter_LP
- Filter values that fall below the low-pass threshold (X <).

### filter_MidPass
- Filter values that fall above high-pass-threshold !(X >=)! and below the low-pass threshold (X <).

### llwrite_list
- Print a list, one element per line,  into your markdown report

### wlegend
- Quickly add a legend to an existing plot, and save the plot immediately.

### getCategories
- Extract unique entries with a corresponding name.

###create_set_SubDir
- Create or set the output directory of the script, and set the "NewOutDir" variable that is used by all ~wplot functions.

###wLinRegression
-Add linear regression, and descriptors to line to your scatter plot. Provide the same dataframe as you provided to wplot() before you called this function

### parFlags
- Create a string from the names of the (boolean) parameters (T or F) of true values. Use it for Suffixing plot names with the parameters that were used for that plot.


### pdfA4plot_on
- Create A4 PDFs to plot multiple subplots in one file


### pdfA4plot_on.layout
- Create A4 PDFs to plot multiple subplots in one file with custom numbers of columns in each row

### pdfA4plot_off
- pair of the "pdfA4plot_on()" function; to finish plotting in the A4 pdf.
