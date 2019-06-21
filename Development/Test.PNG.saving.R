rm(list=ls(all.names = TRUE)); try(dev.off(), silent = T)
source("~/GitHub/MarkdownReports/MarkdownReports/R/MarkdownReports.R")


"Test: Save images as PNG"
setup_MarkdownReports(OutDir = "~/Downloads/from.8GB.else/", b.usepng=T)

Y =1:3; names(Y) = letters[3:5]
wbarplot(Y)
wpie(Y)
md.tableWriter.VEC.w.names(Y, print2screen = T)


X = rnorm(1000)
whist(X)

wboxplot(X)
wstripchart(X)
Z= list(X)
wvioplot_list(Z)
wviostripchart_list(Z)

ZZ=cbind("Ax" = rnorm(1000),
      "Bx" = rnorm(1000))
wplot(ZZ)
wlegend(Y, mdlink = T)

ZZZ = head(ZZ)
md.tableWriter.DF.w.dimnames(ZZZ)
