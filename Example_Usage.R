######################################################################
#  Example usage
######################################################################
# source ("/Users/abelvertesy/MarkDownLogs/ .R")

Var1 =233

Par1 =33
Par3 =3

OutDir = create_set_OutDir("/Users/abelvertesy/MarkDownLogs/YourOutput");
setup_logging_markdown ("ExampleLog", append = F)

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
llogit("")
llprint('[This link shows you more of the MarkDown Syntax]("https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")')



llprint("## Start integrating your plotted results into the story")

Data = rnorm(1000)

# create a plot
hist(Data)

# create a plot and save it to the output folder
whist(Data)

# create a plot and save it, and put it in the report by the "MDLINK=TRUE" parameter
whist(Data, mdlink = T)

llprint("It inserts 2 links, one for the .pdf version (created) and one for a .png version (that you might wanna create). Simply ignore the broken link")
llprint(' - If you dont like this style, just remove the png line from function "MarkDown_Img_Logger_PDF_and_PNG" (around line 89).')


llprint("I do not have save_and_log plotting functions  for every plot you might want, so create your dream plot and then save it from the active device by: wplot_save_this function")

# Lets save figures for that there is
heatmap(matrix(Data, ncol=3,nrow = 5))
hist(Data, add=T)
wplot_save_this(plotname = "DreamMap", mdlink = T)
