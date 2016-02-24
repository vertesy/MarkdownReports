#  Usage_Example_Script.R Report
        Modified: 24/02/2016 | 15:35 | by: Usage_Example_Script.R

## Start logging

1 The **llprint** command prints your to the terminal, giving immediate feedback.

- The **llogit** command puts a line in the file, but does not print it in the terminal.

- You can parse sentences from your variables, parameters and text easily:

Par1 ( 33 ) is bigger than Par3 ( 3 ).

- You can put source code within backticks if neccesary to interpret results:

A peculiar feature of R is rounding: `round(1.5) = 2`, just like `round(2.5) = 2`.

## You already used the handiest formatting commands ...

### This is header 3

 - This is a list

 - that goes on...

 - [This link shows you more of the MarkDown Syntax]("https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")

## Table handling

Save your script parameters in the report (you might need it to exactly reproduce a plot a month later)

####  Settings

| | value  |
| ---| --- |
| Par1 	| 33  |
| Par3 	| 3  |

## Plot, save and integrate (link & display) your results into the story

To integrate plots, you need to:

1. create the plot,

- save into a file, 

- and put a link into your report referring to the file.

***This is all done at once with my wplot, whist, etc functions.***

The `whist(data)` creates a plot and saves it as pdf.

![](data.hist.pdf)

![](Usage_Example_Script/data.hist.png)

![](https://raw.githubusercontent.com/vertesy/MarkdownReports/master/Usage_Example_Script/data.hist.png)

The `whist(data, mdlink = T)` creates a plot and saves it as pdf, and displays.

There might be no 'save&log' plotting functions for every kind of plot you might need,
		so create your dream plot with any R function, and then save it from the active device by the `wplot_save_this()` function!

![](DreamMap.plot.pdf)

![](Usage_Example_Script/DreamMap.plot.png)

![](https://raw.githubusercontent.com/vertesy/MarkdownReports/master/Usage_Example_Script/DreamMap.plot.png)


## GitHub Integration

To upload and correctly display figures on GitHub, you need

 - a png/jpg version of the pdf 

 - an extra link to that image in the markdown file. 

     - This is already in place, unless you set the "png4Github" parameter to FALSE in the `setup_logging_markdown()` function.

     - *GitHub takes relative path from the wiki folder, MOU takes it from the .md files location*

Take a look at the [code](https://github.com/vertesy/MarkdownReports/blob/master/MarkDownLogger_functions.R) (MarkDownLogger_functions.R) for further explanation. It is very short :-).

#### You probably can already decide if MarkdownReports can help you or not. Thanks for reading!
