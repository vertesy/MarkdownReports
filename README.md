
[![DOI](https://zenodo.org/badge/20391/vertesy/MarkdownReports.svg)](https://zenodo.org/badge/latestdoi/20391/vertesy/MarkdownReports)

## What is ***MarkdownReports***?


MarkdownReports is a set of **R** functions that allows you to generate precise figures easily, and create clean reports about what you just discovered with your analysis script. It helps you to:


1. Create scientifically accurate figures and save them automatically as vector graphic (.pdf), that you can use from presentation to posters anywhere.
2. Note down your findings easily in a clear and nicely formatted way, parsed from your variables into english sentences.
- Link & display your figures automatically inside your report, right there where they are needed.
- Version your findings, annotating which parameters were used to reach certain results.
- Share your report with others via email, Github or a personal website.

 
## Why did I make it & why you might like it too?

I do exploratory data analysis as a daily routine, and I have constant interaction with all sorts of people: supervisors, collaborators, colleagues, etc. 

I often have to...

1. ...write emails summarizing the results (text & figures) of the last few days.
2. ...find back results a couple of month back, with all tiny details (parameters used, etc).
3. ...assemble each step I did that day into a logical story line, that others can understand at first glimpse, e.g.: *I observed X; I controlled for Y; Hypothesized explanation A; Falsified it; Came up with explanation B; Tested & proven it...*
	
For all of the above, my solution is MarkdownReports. I think its better than other solutions I found. Many of those like to combine source code with results, and many are too complex to use. Most of people I interact with are not interested in  the source code, but are very keen on seeing my results from all possible angles and are asking detailed questions about the analysis.

### Where does ***MarkdownReports*** stand out?

- Pure markdown output, simple and elegant layout.
- Handy integration of text with figures (linked to an external file & displayed: makes manual tailoring easy)
- Easy generation of precise figures (axis labels, etc) that are saved as a vector graphic (pdf), making it scalable for presentations, posters, etc
- The report file is automatically named after the R-script, so that it is linked to the source code that generated it.
- A timestamped subdirectory is created that you can backup once satisfied with your results.
- The generated report is easy to [share on a GitHub wiki](https://github.com/vertesy/MarkdownReports/wiki/Github-wiki-integration).
- It natively exports tables from R to Markdown
- It is all achieved in ~ 400 lines of well commented code.


<br><br>
## Installation

Install directly from **GitHub** via **devtools** with one R command:

    devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
    
...then simply load the package:

    require("MarkdownReports")
    
Alternatively, you simply source it from the web. 
*This way function help will not work, and you will have no local copy of the code on your hard drive.*

    source("https://raw.githubusercontent.com/vertesy/MarkdownReports/master/MarkdownReports/R/MarkdownReports.R")

<br><br>
## Discover 4 Yourself!

-  See it working: Check out a dummy [R script](https://github.com/vertesy/MarkdownReports/blob/master/Examples/Usage_Example_Script.R) 
 and the [MarkDown report](https://github.com/vertesy/MarkdownReports/blob/master/Examples/Usage_Example_Script/Usage_Example_Script.R.log.md) 
 it generates inside this [GitHub Repo](https://github.com/vertesy/MarkdownReports).

- Check out the [wiki](https://github.com/vertesy/MarkdownReports/wiki) and the [list of functions in the package.](https://github.com/vertesy/MarkdownReports/wiki/Function-Overview)

- [Browse the code of the functions.](https://github.com/vertesy/MarkdownReports/blob/master/MarkdownReports/R/MarkdownReports.R)


### Learn about the markdown format

- See the power and simplicity of markdown format [explained on Github](https://guides.github.com/features/mastering-markdown)
- Checkout some cool markdown editors, like [MOU](http://25.io/mou/), or [markdownpad](http://markdownpad.com/).


 <br/><br/>
#### Cite it via its Digital Object Identifier (DOI): 


[![DOI](https://zenodo.org/badge/20391/vertesy/MarkdownReports.svg)](https://zenodo.org/badge/latestdoi/20391/vertesy/MarkdownReports)



<br>
**MarkdownReports** is a project of @vertesy.

 <br/> <br/> <br/> <br/> <br/>
[*edit the website*](https://github.com/vertesy/MarkdownReports/generated_pages/new)