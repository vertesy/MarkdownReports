# Functions of the MarkDownLogger library
...and some explanation...

## Auxiliary functions 

1. ***kollapse***
	 - parses (and prints) flexibly anything you pass on to it into a string
- ***any_print***
	 - more flexible printing fun
- ***iround***
- ***percentage_formatter***

## Setup Logging

1. ***create_set_OutDir***
	- # Defines and creates the Output / Reports directory, with a timestamped subfolder where you can backup manually.
- ***setup_logging_markdown***
	 - setup_logging file, path and modification date
- ***continue_logging_markdown***
	 - continue writing to an existing markdown file

## Write into your markdown log file

1. ***log_settings_MarkDown***
	 - log your parameter settings into a tabular format
- ***llprint***
	 - log to markdown file and print to screen
- ***llogit***
	 - log to markdown file, do not print
- ***MarkDown_ImgLink_formatter***
	 - insert a link to a pdf image
- ***MarkDown_Img_Logger_PDF_and_PNG***
	 - insert 2 links, one for PDF, one for PNG version of the same image (png files are needed for web or email sharing!!!)
- ***MarkDown_Img_Logger_4GitHub***
	 - insert 2 links, one for PDF, one for PNG version of the same image (png files are needed for web or email sharing!!!)

## Write out pretty tables to your markdown file

1. ***MarkDown_Table_writer_DF_RowColNames***
- ***MarkDown_Table_writer_NamedVector***

## Generate and save plots into pdf and insert a diplay-link into your markdown file
*All of them are based on the rfunction with the same name (without w prefix)*

1. ***wplot***
- ***whist***
- ***wbarplot***
- ***wboxplot***
- ***wpie***
- ***wplot_save_this***
	 - This in a specific function:  It saves the plot you created from the active device (in a pdf, with a link in the report if specified -  as for all the above plotting functions).