## Function Overview
You find the list of function of this library below. For details, please use the `help()` function, or browse the [source code](). <br>


### 1. kollapse
- Collapses values and strings to one string (without a white space). It also prints the results (good for a quick check)


### 2. any_print
- A more flexible printing function that collapses any variable passed to it by white spaces.


### 3. iround
- Rounds a value to the significant amount of digits. Its a wrapper for signif().


### 4. percentage_formatter
- Parse a string of 0-100% from a number between 0 and 1.


### 5. create_set_OutDir
- Create or set the output directory of the script, and set the "OutDir" variable that is used by all ~wplot functions.


### 6. setup_logging_markdown
- Setup the markdown report file, create a sub directory in "OutDir". Its name is stamped with the script name and the modification time. Create the "Log_PnF" variable used by all log-writing and ~wplot functions.


### 7. continue_logging_markdown
- Continue writing to an existing report file.


### 8. log_settings_MarkDown
- Log the parameters & settings used in the script in a table format.


### 9. llprint
- Collapse by white spaces a sentence from any variable passed on to the function. Print the sentence to the screen and write it to your markdown report file, if the "Log_PnF" variable is defined.


### 10. llogit
- Collapse by white spaces a sentence from any variable passed on to the function. llogit() writes it to your markdown report file, if the "Log_PnF" variable is defined. It does not print the sentence to the screen.


### 11. MarkDown_ImgLink_formatter
- Format a markdown image reference (link) from the file path to the file. It can parse the file path, if you pass it in separate variables and strings. E.g. MarkDown_ImgLink_formatter(Directory, "MyImage.png").


### 12. MarkDown_Img_Logger_PDF_and_PNG
- Format a markdown image reference (link) to a .pdf and .png versions of graph, and insert both links to the markdown report, set by "Log_PnF". If the "png4Github" variable is set, the .png-link is set up such, that you can upload the whole report with the .png image into your GitHub repo's wiki, under "Reports"/OutDir/ (Reports is a literal string, OutDir is the last/deepest directory name in the "OutDir" variable. See create_set_OutDir() function.). This function is called by the ~wplot functions.


### 13. MarkDown_Table_writer_DF_RowColNames
- Take an R data frame with row- and column- names, parse a markdown table from it, and write it to the markdown report, set by "Log_PnF".


### 14. MarkDown_Table_writer_NamedVector
- Take an R vector with names, parse a markdown table from it, and write it to the markdown report, set by "Log_PnF".


### 15. wplot
- Create and save scatter plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "Log_PnF". The .png version is not created, only the link is put in place, not to overwrite previous versions. The .png version is not created, only the link is put in place. You can add 2D error bars around the dots, or add lines (ablines) to your plot, by setting "abline" argument to = F (no line, default), "h" (horizontal, further specified by a = y-offset), "v" (vertical, further specified by a = x-offset), "ab" (line with an angle, further specified by a = offset, b = slope).


### 16. wplot_save_this
- Save the currently active graphic device (for complicated plots).  Insert links to your markdown report, set by "Log_PnF". Name the file by naming the variable!


### 17. whist
- Create and save histograms as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "Log_PnF". The .png version is not created, only the link is put in place, not to overwrite previous versions. Name the file by naming the variable! Cannot be used with dynamically called variables [e.g. call vectors within a loop]. "filtercol" assumes  >= coloring!


### 18. wbarplot
- Create and save bar plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "Log_PnF". The .png version is not created, only the link is put in place, not to overwrite previous versions.


### 19. wboxplot
- Create and save box plots as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "Log_PnF". The .png version is not created, only the link is put in place, not to overwrite previous versions.


### 20. wpie
- Create and save pie charts as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "Log_PnF". The .png version is not created, only the link is put in place, not to overwrite previous versions.


### 21. wstripchart
- Create and save strip charts as .pdf, in "OutDir". If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "Log_PnF". The .png version is not created, only the link is put in place, not to overwrite previous versions.


### 22. wstripchart_list
- Create and save stripcharts from a list as .pdf, in "OutDir". This version allows individual coloring of each data point, by a color-list of the same dimension. If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "Log_PnF". The .png version is not created, only the link is put in place, not to overwrite previous versions.


### 23. wvioplot_list
- Create and save violin plots as .pdf, in "OutDir". It requires (and calls) "vioplot" package. If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "Log_PnF". The .png version is not created, only the link is put in place, not to overwrite previous versions.


### 24. wviostripchart_list
- Create and save violin plots as .pdf, in "OutDir". It requires (and calls) "vioplot" package. If mdlink =T, it inserts a .pdf and a .png link in the markdown report, set by "Log_PnF". The .png version is not created, only the link is put in place, not to overwrite previous versions.


### 25. whist_dfCol
- Use this version of whist() if you iterate over columns  or rows of a data frame. You can name the file by naming the variable. Cannot be used with dynamically called variables [e.g. call vectors within a loop]


### 26. wbarplot_dfCol
- wbarplot for a column of a data frame.


### 27. val2col
- Convert numeric values to a scaled color gradient. Source: https://stackoverflow.com/questions/8717669/heat-map-colors-corresponding-to-data-in-r


### 28. error.bar
- Put error bars on top of your bar plots. This functionality is now integrated into MarkdownReporter's wbarplot() function


### 29. barplot.label
- Add extra labels to your bar plots at the top or the base.
