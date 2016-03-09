#  MarkdownReports_in_Action.r Report
		Modified: 09/03/2016 | 13:24 | by: MarkdownReports_in_Action.r

_I will show an (imaginary) example workflow on complitely made up data._

### Hey Snowflake collector, welcome back from Reykjavik! How big are the snowflakes over there?

![](SnowflakeSizes_Reykjavik.barplot.pdf)

![](Reports/MarkdownReports_in_Action/SnowflakeSizes_Reykjavik.barplot.png)

### At first we would like to throw away every  measurement where the measurement bias (reported by your snowflake collecting machine) is above 10%:

![](Measurement_Bias.barplot.pdf)

![](Reports/MarkdownReports_in_Action/Measurement_Bias.barplot.png)

 76.9 %  or  10  of  13  entries in  Measurement_Bias  fall below a threshold value of:  10

The code:

### Let's see how it compares with snow flakes from other cities?

![](SnowflakeSizes.stripchart.pdf)

![](Reports/MarkdownReports_in_Action/SnowflakeSizes.stripchart.png)

### Let's say, we also measured the temperature of the flakes. We can color flakes that had temperature below -10:

![](SnowflakeSizes_colored_by_temp.stripchart.pdf)

![](Reports/MarkdownReports_in_Action/SnowflakeSizes_colored_by_temp.stripchart.png)

The code:

### And let's see how the correlation looks like for snowflakes in each city:

![](Mean_Snowflake_Size_and_Temp.plot.pdf)

![](Reports/MarkdownReports_in_Action/Mean_Snowflake_Size_and_Temp.plot.png)

The code:
#  MarkdownReports_in_Action.r Report
		Modified: 09/03/2016 | 13:27 | by: MarkdownReports_in_Action.r
