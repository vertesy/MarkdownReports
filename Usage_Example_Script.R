######################################################################
#  Usage_Example_Script
######################################################################
# source ("/Users/abelvertesy/MarkDownLogs/Usage_Example_Script.R")

print("Use MOU or any other Markdown editor to see resutls")

# Setup --------------------
Par1 =33
Par3 =3

OutDir = create_set_OutDir("/Users/abelvertesy/MarkDownLogs/Usage_Example_Script");
setup_logging_markdown ("Usage_Example_Script.R", append = F)

# Start logging ----------------------------
llprint("## Start logging")
llprint("1 The **llprint** command prints your to the terminal, giving immediate feedback.")
llogit ("- The **llogit** command puts a line in the file, but does not print it in the terminal.")
llprint("- You can parse sentences from your variables, parameters and text easily:")
llprint("Par1 (", Par1, ") is bigger than Par3 (", Par3,")." )
llprint("- You can put source code within backticks if neccesary to interpret results:")
llprint("A peculiar feature of R is rounding: `round(1.5) = 2`, just like `round(2.5) = 2`.")


# Markdown Formatting --------------------------------
llprint("## You already used the handiest formatting commands ...")
llprint("### This is header 3")
llprint(" - This is a list")
llprint(" - that goes on...")
llprint(' - [This link shows you more of the MarkDown Syntax]("https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")')

# Table handling ----------------------------------------------------
llprint("## Table handling")
llprint("Save your script parameters in the report (you might need it to exactly reproduce a plot a month later)")

log_settings_MarkDown (Par1, Par3)


# Create and integrate figures ----------------------------------------------------
llprint("## Plot, save and integrate (link & display) your results into the story")

# create a plot
data = rnorm(1000)
hist(data)

llprint("To integrate plots, you need to:")
llprint("1. create the plot,")
llprint("- save into a file, ")
llprint("- and put a link into your report referring to the file.")
llprint("***This is all done at once with my wplot, whist, etc functions.***")

# create a plot and save it to the output folder
llprint("The `whist(data)` creates a plot and saves it as pdf.")
whist(data)

# create a plot, save it, AND put it in the report by the "MDLINK=TRUE" parameter
whist(data, mdlink = T)
llprint("The `whist(data, mdlink = T)` creates a plot and saves it as pdf, and displays.")


llprint("There might be no 'save&log' plotting functions for every kind of plot you might need,
		so create your dream plot with any R function, and then save it from the active device by the `wplot_save_this()` function!")

# Lets save figures for that there is
heatmap(matrix(data, ncol=3,nrow = 5))
hist(data, add=T)
wplot_save_this(plotname = "DreamMap", mdlink = T)

llprint("## GitHub Integration")

llprint("To upload and correctly display figures on GitHub, you need")
llprint(" - a png/jpg version of the pdf ")
llprint(" - an extra link to that image in the markdown file. ")
llprint('     - This is already in place, unless you set the "png4Github" parameter to FALSE in the `setup_logging_markdown()` function.')
llprint("     - *GitHub takes relative path from the wiki folder, MOU takes it from the .md files location*")

llprint("Take a look at the [code](https://github.com/vertesy/MarkDownLogs/blob/master/MarkDownLogger_functions.R) (MarkDownLogger_functions.R) for further explanation. It is very short :-).")


llprint("#### You probably can already decide if MarkDownLogs can help you or not. Thanks for reading!")
