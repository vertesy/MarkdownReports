######################################################################
#  Usage_Example_Script
######################################################################
# source ("/Users/abelvertesy/MarkDownLogs/Usage_Example_Script.R")

Par1 =33
Par3 =3

OutDir = create_set_OutDir("/Users/abelvertesy/MarkDownLogs/Usage_Example_Script");
setup_logging_markdown ("Usage_Example_Script.R", append = F)

# save your script params
log_settings_MarkDown (Par1, Par3)

# start logging
llogit("llogit command puts a line in the file, but do not print it ")
llprint("llprint command prints your to the terminal, giving immediate feedback.")
llprint("This is useful if you parse sentences:")
llprint("Par1 (", Par1, ") is bigger than Par3 (", Par3,")" )

# Start formatting
llprint("# Start formatting: This is header 1")
llprint("## This is header 2")
llprint(" - This is a list")
llprint(" - that goes on...")
llprint(' - [This link shows you more of the MarkDown Syntax]("https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")')



llprint("## Start integrating your plotted results into the story")

# create a plot
Data = rnorm(1000)
hist(Data)
llprint("To integrate plots, you need to:")
llprint("1. create the plot,")
llprint("- save into a file, ")
llprint("- and put a link into your report referring to the file.")
llprint("This is all done at once with my wplot, whist, etc functions.")

# create a plot and save it to the output folder
whist(Data)

# create a plot and save it, and put it in the report by the "MDLINK=TRUE" parameter
whist(Data, mdlink = T)


llprint("I do not have save_and_log plotting functions  for every plot you might want, so create your dream plot and then save it from the active device by: wplot_save_this function")

# Lets save figures for that there is
heatmap(matrix(Data, ncol=3,nrow = 5))
hist(Data, add=T)
wplot_save_this(plotname = "DreamMap", mdlink = T)

llprint("## You can insert code if you wanna")

llprint('`heatmap(matrix(Data, ncol=3,nrow = 5))`')
llprint('`hist(Data, add=T)`')
llprint('`wplot_save_this(plotname = "DreamMap", mdlink = T)`')



##

# llprint("It inserts 2 links, one for the .pdf version (created) and one for a .png version (that you might wanna create). Simply ignore the broken link")
# llprint(' - If you dont like this style, just remove the png line from function "MarkDown_Img_Logger_PDF_and_PNG" (around line 89).')
