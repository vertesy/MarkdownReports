## What is MarkDownLogs

MarkDownLogs is a set of **R** functions that allows you to generate summaries what you discovered with your source code. It helps you to:

1. Write down your findings in an easy, clear and nicely formatted way.
- Support your findings with quick & pretty figures saved along your report
- Link and show these in your markdown or html document
- Share your findings

 
## Why did I make it, why you might like it?

I do exploratory data analysis as daily routine, and I have constant interaction with all sorts of people: supervisors, collaborators, colleagues, etc. I often ...

1. ...have to write emails summarizing the results (text accompanied by figures) of the last few days.
2. ...find back results a couple of month back, with all tiny details (parameters used, etc)
3. ...assemble each step I did that day into a logical story line, that others can follow right as they see it, e.g.: *I observed X; I controlled for Y; Hypothesized explanation A; Falsified it; Came up with explanation B; Tested & proven it...*
	
For all of the above, my solution is MarkDownLogs. I think its better than other solutions I found, like  that combine source code with results. Most of people I interact with are not interested in the source code, but are very keen on seeing all kind of validations.

#### It is better than existing report generators

- Pure Markdown output, Simple and Elegant layout.
- Integration with figures (linked to an external file & displayed: makes manual tailoring easy)
- Easy generation of accurate figures (axis labels, etc) that are saved as a vector graphic (pdf): scalable for presentations, posters, etc
- Named as source file + date: relate to the script, but avoid overwriting.
- Does not spam your report with code, but its linked to it via its name, so you can look it up of if needed. (iPython notebook, KnittR)
- Usable on GitHub Wiki's (although GitHub at the moment cannot)
- Native export of tables from R to Markdown
- Its all achieved in ~ 100 lines of well commented code.
