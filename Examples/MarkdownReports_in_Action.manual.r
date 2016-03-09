######################################################################################################
## MarkdownReports in Action
######################################################################################################
# source("/Users/abelvertesy/MarkdownReports.wiki/Reports/MarkdownReports_in_Action.r")



# Functions ------------------------
try (source ('/Users/abelvertesy/TheCorvinas/R/CodeAndRoll.R'),silent= F)
# library(stringr)

# Setup ------------------------
setup_MarkdownReports(OutDir = "/Users/abelvertesy/MarkdownReports/MarkdownReports_in_Action", fname = "MarkdownReports_in_Action.r", append = F)
# create_set_OutDir("/Users/abelvertesy/Google_Drive/X_react_Data/Abelz/X_react_2016")
llprint("_I will show an (imaginary) example workflow on complitely made up data._")

OutDirOrig = OutDir

# Your data ------------------------

SnowflakeSizes_Reykjavik = c(1.21, 1.31, 1.14, 1.3, 1.11, 1.09, 1.22, 1.191, 1.31, 1.16, 1.19, 1.23, 1.29); l(SnowflakeSizes_Reykjavik)
names (SnowflakeSizes_Reykjavik) = c( '09-Jan', '10-Jan', '11-Jan', '12-Jan', '15-Jan', '16-Jan', '17-Jan', '18-Jan', '19-Jan', '20-Jan', '21-Jan', '22-Jan', '23-Jan' )

# Measurement_Bias = 	3*(2+(rnorm(13))^2) # inline_vec.num(iround(Measurement_Bias))
Measurement_Bias = c( 11.9, 7.28, 6.66, 6.18, 7.64, 6.23, 7.97, 19.1, 23.8, 6.01, 8.02, 6.04, 7.01 )


SnowflakeSizes =  list (
	"Murmansk" = c(1.1, 1.111, 1.2, 1, 1, 1.4, 1.3),
	"Anchorage" = c(1.3, 1.41, 1.24, 1.6, 1.7, .9)
)

# Parameters ------------------------
thresholdX = 10


# GO ------------------------

llprint("### Hey Snowflake collector, welcome back from Reykjavik! How big are the snowflakes over there?")

wbarplot(SnowflakeSizes_Reykjavik, mdlink = T)

llprint("### At first we would like to throw away every  measurement where the measurement bias (reported by your snowflake collecting machine) is above 10%:")
wbarplot(Measurement_Bias, ylab = "Measurement Bias (%)", hline = thresholdX, filtercol = -1, mdlink = T)

pass = filter_LP(Measurement_Bias, threshold = thresholdX) # report the actual numbers
llprint("The code:")

# --------------------------------------------------------------------------------
llprint("### Let's see how it compares with snow flakes from other cities?")
SnowflakeSizes$Reykjavik = SnowflakeSizes_Reykjavik[pass]

wstripchart(SnowflakeSizes, tilted_text = T, mdlink = T)



# --------------------------------------------------------------------------------
llprint("### Let's say, we also measured the temperature of the flakes. We can color flakes that had temperature below -10:")
# xx <- function(x) { x = -(20-5*(x^3))}
# SnowflakeTemperature = lapply(SnowflakeSizes, xx)
# inline_vec.num(lapply(SnowflakeTemperature, iround))
SnowflakeTemperature = list( c(-13.3, -13.1, -11.4, -15, -15, -6.28, -9.02),
							 c(-9.02, -5.98, -10.5, 0.48, 4.56, -16.4),
							 c(-8.76, -12.6, -9.02, -13.2, -13.5, -10.9, -12.2, -11.6, -10.7, -9.27) )

colore = lapply(SnowflakeTemperature, function(x) (x< -10)+1)
SnowflakeSizes_colored_by_temp = SnowflakeSizes
wstripchart_list(SnowflakeSizes_colored_by_temp, tilted_text = T, bg = colore, mdlink = T)
llprint("The code:")

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
legend("topleft", legend = rownames(Mean_Snowflake_Size_and_Temp), fill = 3:5, bty="n")

wplot_save_this(plotname = plotnameLastPlot, mdlink = T)


llprint("The code:")
