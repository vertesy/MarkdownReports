                   Modified: Mon Sep  7 15:21:49 2015

####  Settings

| | value  |
| ---| --- |
| Par1 	| 33  |
| Par3 	| 3  |

llogit command puts a line in the file, but do not print it

llprint command prints your to the terminal, giving immediate feedback.

This is useful if you parse sentences:

Par1 ( 33 ) is bigger than Par3 ( 3 )

# Start formatting: This is header 1

## This is header 2

 - This is a list

 - that goes on...



[This link shows you more of the MarkDown Syntax]("https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")

## Start integrating your plotted results into the story

![Data.hist](Data.hist.pdf)

![Data.hist](Data.hist.png)

It inserts 2 links, one for the .pdf version (created) and one for a .png version (that you might wanna create). Simply ignore the broken link

 - If you dont like this style, just remove the png line from function "MarkDown_Img_Logger_PDF_and_PNG" (around line 89).

I do not have save_and_log plotting functions  for every plot you might want, so create your dream plot and then save it from the active device by: wplot_save_this function

![DreamMap.plot](DreamMap.plot.pdf)

![DreamMap.plot](DreamMap.plot.png)
