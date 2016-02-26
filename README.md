

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.45945.svg)](http://dx.doi.org/10.5281/zenodo.45945)


## What is ***MarkdownReports***?

MarkdownReports is a set of **R** functions that allows you to generate neat reports / summaries what you discovered with your script. It helps you to:

1. Write down your findings easily in a clear and nicely formatted way.
- Support your findings with quick & pretty figures saved along your report
- Link and show these in your markdown or html document, *at the right spot*.
- Share your findings with others via email, github or a personal website.

 
## Why did I make it & why might you like it?

I do exploratory data analysis as a daily routine and I have constant interaction with all sorts of people: supervisors, collaborators, colleagues, etc. 

I often have to...

1. ... write emails summarizing the results (text accompanied by figures) of the last few days.
2. ...find back results a couple of month back, with all tiny details (parameters used, etc)
3. ...assemble each step I did that day into a logical story line, that others can follow right as they see it, e.g.: *I observed X; I controlled for Y; Hypothesized explanation A; Falsified it; Came up with explanation B; Tested & proven it...*
	
For all of the above, my solution is MarkdownReports. I think its better than other solutions I found. Many of those like to combine source code with results, and many are too complex to use. Most of people I interact with are not interested in code, but are very keen on seeing my results from all possible angles and are asking detailed questions about the analysis.

### Where does ***MarkdownReports*** stand out?

- Pure markdown output, simple and elegant layout.
- Handy integration of text with figures (linked to an external file & displayed: makes manual tailoring easy)
- Easy generation of precise figures (axis labels, etc) that are saved as a vector graphic (pdf), making it scalable for presentations, posters, etc
- The report file can easily be named after the source file with date & time: it is linked to the source code, but avoids overwriting.
- It does not spam your report with code, but it shares the name, so you can look it up fast if needed. 
- It is usable on GitHub Wiki's (you need to [create](https://stackoverflow.com/questions/28657992/automator-apple-script-to-convert-jpeg-image) .png versions of .pdf - a current limitation of GitHub)
- It natively exports tables from R to Markdown
- It is all achieved in ~ 200 lines of well commented code.

<br><br>
## Installation

Install directly from **GitHub** via **devtools** with one R command:

    devtools::install_github(repo = "vertesy/MarkdownReports/MarkdownReports")
    
...then simply load the package:

    require("MarkdownReports")

<br><br>
## Discover 4 Yourself!

-  See it working: Check out a dummy [R script](https://github.com/vertesy/MarkdownReports/blob/master/Usage_Example_Script.R) 
 and the [MarkDown report](https://github.com/vertesy/MarkdownReports/blob/master/Usage_Example_Script/Usage_Example_Script.R.log.md) 
 it generates inside this [GitHub Repo](https://github.com/vertesy/MarkdownReports).
- Check out the [list of functions in the package.](https://github.com/vertesy/MarkdownReports/wiki/Function-Overview)
- [Browse the code of the functions](https://github.com/vertesy/MarkdownReports/blob/master/MarkdownReports/R/MarkdownReports.R)


### Learn about the markdown format

- See the power and simplicity of markdown format [explained on Github](https://guides.github.com/features/mastering-markdown)
- Checkout some cool markdown editors, like [MOU](http://25.io/mou/), or [markdownpad](http://markdownpad.com/).


 <br/><br/>
#### Cite it via its Digital Object Identifier (DOI): 


[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.45945.svg)](http://dx.doi.org/10.5281/zenodo.45945)


<br>
**MarkdownReports** is a project of @vertesy.

 <br/> <br/> <br/> <br/> <br/>
[*edit the website*](https://github.com/vertesy/MarkdownReports/generated_pages/new)