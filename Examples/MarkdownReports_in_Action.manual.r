######################################################################################################
## MarkdownReports in Action
######################################################################################################
# source("~/MarkdownReports.wiki/Reports/MarkdownReports_in_Action.r")
rm(list=ls(all.names = TRUE));
require(MarkdownReports)
try.dev.off()



# Functions ------------------------
# source("~/GitHub/MarkdownReports/MarkdownReports/R/MarkdownReports.R")
# source("https://raw.githubusercontent.com/vertesy/TheCorvinas/master/R/CodeAndRoll.R")
# library(stringr)

sem <- function (x, na.rm=T) sd(unlist(x), na.rm = na.rm)/sqrt(length(na.omit(as.numeric(x))))  # Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default)

# Setup ------------------------
OutDir = "~/GitHub/MarkdownReports/Examples/MarkdownReports_in_Action"
setup_MarkdownReports(OutDir=OutDir, scriptname =  "MarkdownReports_in_Action.r", title = "Snowflakes"
                      , b.usepng = T, b.mdlink = T )
llprint("_I will show an (imaginary) example workflow on complitely made up data._")



llprint("### This example is for MarkdownReports v3.1")
llprint("Other major version (v2, v4-dev) might not run !")



# Your data ------------------------

SnowflakeSizes_Reykjavik = c(1.21, 1.31, 1.14, 1.3, 1.11, 1.09, 1.22, 1.191, 1.31, 1.16, 1.19, 1.23, 1.29); length(SnowflakeSizes_Reykjavik)
names (SnowflakeSizes_Reykjavik) = c( '09-Jan', '10-Jan', '11-Jan', '12-Jan', '15-Jan', '16-Jan', '17-Jan', '18-Jan', '19-Jan', '20-Jan', '21-Jan', '22-Jan', '23-Jan' )

# Measurement_Bias = 	3*(2+(rnorm(13))^2) # inline_vec.num(iround(Measurement_Bias))
Measurement_Bias = c( 11.9, 7.28, 6.66, 6.18, 7.64, 6.23, 7.97, 19.1, 23.8, 6.01, 8.02, 6.04, 7.01 )


SnowflakeSizes =  list (
	"Murmansk" = c(1.1, 1.111, 1.2, 1, 1, 1.4, 1.3),
	"Anchorage" = c(1.3, 1.41, 1.24, 1.6, 1.7, .9)
)


# Parameters ------------------------
thresholdX = 10 # % bias is accepted


# GO ------------------------

llprint("### Hey Snowflake collector, welcome back from Reykjavik! How big are the snowflakes over there?")

llprint("Take a look at the raw numbers:")
md.tableWriter.VEC.w.names(SnowflakeSizes_Reykjavik)
llogit('The code:
```
md.tableWriter.VEC.w.names(SnowflakeSizes_Reykjavik)
```')


llprint("Let's visualize them:")
wbarplot(SnowflakeSizes_Reykjavik)
llogit('The code:
```
wbarplot(SnowflakeSizes_Reykjavik)
```
NOTE: use the `mdlink = FALSE` argument if you don not want to save this specific plot.
       See wiki for more ')

llprint("### At first we would like to throw away every  measurement where the measurement bias (reported by your snowflake collecting machine) is above 10%:")
wbarplot(Measurement_Bias, ylab = "Measurement Bias (%)", hline = thresholdX, filtercol = -1)
barplot_label(Measurement_Bias, barplotted_variable = Measurement_Bias, TopOffset = 2)

pass = filter_LP(Measurement_Bias, threshold = thresholdX) # report the actual numbers
llogit('The code:
```
wbarplot(Measurement_Bias, ylab = "Measurement Bias (%)", hline = thresholdX, filtercol = -1)
barplot_label(Measurement_Bias, TopOffset = 2)
```')

Nr_of_measurements = unlist(lapply(SnowflakeSizes, length))
wpie(Nr_of_measurements, both_pc_and_value = F)
llogit('The code:
```
wpie(Nr_of_measurements, both_pc_and_value = F)
```')

# --------------------------------------------------------------------------------
llprint("### Let's see how it compares with snow flakes from other cities?")
SnowflakeSizes$Reykjavik = SnowflakeSizes_Reykjavik[pass]

Average_SnowflakeSizes = unlist(lapply(SnowflakeSizes, mean))
SEM_SnowflakeSizes = unlist(lapply(SnowflakeSizes, sem))
wbarplot(Average_SnowflakeSizes, tilted_text = T,
         errorbar = T, upper = SEM_SnowflakeSizes)
wlegend.label("Error bars denote +- SEM", cex = .75)

wstripchart(SnowflakeSizes, tilted_text = T)
llogit('The code:
```
wstripchart(SnowflakeSizes, tilted_text = T)
```')


wvioplot_list(SnowflakeSizes, tilted_text = T, yoffset = -.2)
llogit('The code:
       ```
       wvioplot_list(SnowflakeSizes, tilted_text = T, yoffset = -.2)
       ```')
wplot_save_this()



# --------------------------------------------------------------------------------
llprint("### Let's say, we also measured the temperature of the flakes. We can color flakes that had temperature below -10:")
# xx <- function(x) { x = -(20-5*(x^3))}
# SnowflakeTemperature = lapply(SnowflakeSizes, xx)
# inline_vec.num(lapply(SnowflakeTemperature, iround))
SnowflakeTemperature = list( c(-13.3, -13.1, -11.4, -15, -15, -6.28, -9.02),
							 c(-9.02, -5.98, -10.5, 0.48, 4.56, -16.4),
							 c(-8.76, -12.6, -9.02, -13.2, -13.5, -10.9, -12.2, -11.6, -10.7, -9.27) )


colz = lapply(SnowflakeTemperature, function(x) (x< -10)+1)
SnowflakeSizes_colored_by_temp = SnowflakeSizes
wstripchart_list(SnowflakeSizes_colored_by_temp, tilted_text = T, pch = 23, bg = colz)
llogit('The code:
```
SnowflakeTemperature = list( c(-13.3, -13.1, -11.4, -15, -15, -6.28, -9.02),
							 c(-9.02, -5.98, -10.5, 0.48, 4.56, -16.4),
       c(-8.76, -12.6, -9.02, -13.2, -13.5, -10.9, -12.2, -11.6, -10.7, -9.27) )

       colz = lapply(SnowflakeTemperature, function(x) (x< -10)+1)
       SnowflakeSizes_colored_by_temp = SnowflakeSizes
       wstripchart_list(SnowflakeSizes_colored_by_temp, tilted_text = T, bg = colz)

```')

Snowflakes = cbind(
	"Temperature" = unlist(SnowflakeTemperature),
	"Size" = unlist(SnowflakeSizes)
)

Mean_Snowflake_Size_and_Temp = cbind(
	"Temperature" = unlist(lapply(SnowflakeTemperature, mean)),
	"Size" = unlist(lapply(SnowflakeSizes, mean))
)


Snowflakes_SEM = cbind(
	"Temperature" = unlist(lapply(SnowflakeTemperature, sem)),
	"Size" = unlist(lapply(SnowflakeSizes, sem))
)

llprint("### And let's see how the correlation looks like for snowflakes in each city:")
wplot(Mean_Snowflake_Size_and_Temp, errorbar = T, upper = Snowflakes_SEM[,"Size"], left = Snowflakes_SEM[,"Temperature"], col =3:5, cex=2)

legend_=3:5
names(legend_) = rownames(Mean_Snowflake_Size_and_Temp)
wlegend(legend_, poz = 3)
wLinRegression(Mean_Snowflake_Size_and_Temp, lty=3 )



llogit('The code:
```Snowflakes = cbind(
	"Temperature" = unlist(SnowflakeTemperature),
       "Size" = unlist(SnowflakeSizes)
)

Mean_Snowflake_Size_and_Temp = cbind(
"Temperature" = unlist(lapply(SnowflakeTemperature, mean)),
"Size" = unlist(lapply(SnowflakeSizes, mean))
)

sem <- function(x, na.rm=T) sd(unlist(x), na.rm = na.rm)/sqrt(length(na.omit.strip(as.numeric(x))))  # Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default)
Snowflakes_SEM = cbind(
"Temperature" = unlist(lapply(SnowflakeTemperature, sem)),
"Size" = unlist(lapply(SnowflakeSizes, sem))
)

llprint("### And lets see how the correlation looks like for snowflakes in each city:")
wplot(Mean_Snowflake_Size_and_Temp, errorbar = T, upper = Snowflakes_SEM[,"Size"], left = Snowflakes_SEM[,"Temperature"], col =3:5, cex=2)

legend_=3:5
names(legend_) = rownames(Mean_Snowflake_Size_and_Temp)
wlegend( fill_= legend_, poz = 3,bty="n")

# linear regression and correlation coefficient
wLinRegression(Mean_Snowflake_Size_and_Temp, lty=3 )
```')


