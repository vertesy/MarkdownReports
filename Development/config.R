# Configuration for the Package
DESCRIPTION <- list(
  package.name = "MarkdownReports",
  version = "4.7.0",
  title = "Generate Scientific Figures and Reports Easily",
  description = "MarkdownReports is a set of R functions that allows you to generate precise figures easily,
    and create clean markdown reports about what you just discovered with your analysis script. It helps you to:
    1. Create scientifically accurate (annotated) figures with very short code, making use of variable-, row- and columnnames.
    2. Save figures automatically as vector graphic (.pdf), that you can use from presentation to posters anywhere.
    3. Incorporate your figures automatically in a markdown report file.
    4. Describe your figures & findings in the same report in a clear and nicely formatted way, parsed from your variables into english sentences.
    5. Share your report, by exporting your report to .pdf, .html or .docx, or via Github or a personal website.",

  author.given = "Abel",
  author.family = "Vertesy",
  author.email = "av@imba.oeaw.ac.at",
  license = "GPL-3 + file LICENSE",
  depends =  "Stringendo, CodeAndRoll2, MarkdownHelpers",
  imports = "colorRamps, gplots, clipr, RColorBrewer, VennDiagram, vioplot, ReadWriter",
  github.user = "vertesy"
)
