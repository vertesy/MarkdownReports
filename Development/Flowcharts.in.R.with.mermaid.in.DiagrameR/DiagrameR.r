

setup_MarkdownReports("~/GitHub/MarkdownReports/Development/Flowcharts.in.R.with.mermaid.in.DiagrameR/")

irequire(DiagrammeR)
require(MarkdownReportsDev)

try.dev.off()
pdf(file = "Aaa.pdf")
# DiagrammeR("
mg1 = mermaid("
  graph TD;
    A[rect]-- add style -->A2[rect + style];
    B{rhombus}---|+ some style|B2{rhombus + style};
    C(rounded);   D((circle));
    style A2 fill:#c12,stroke-width:5px;
    style B2 fill:none, stroke-dasharray:10;
")
mg1
try.dev.off()
wplot_save_this(plotname = "DiagrammeR")


library("DiagrammeR")
mg <- mermaid("graph TB
              A-->B")

html_print(mg)
llprint(mg)

install.packages("webshot")
irequire("htmlwidgets")
webshot::install_phantomjs()

saveWidget(mg1,"11.html", selfcontained = T)
webshot::webshot("11.html","12.png", zoom = 2, delay =3)
?webshot
