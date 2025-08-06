
 _cite via:_ [![DOI](https://zenodo.org/badge/20391/vertesy/MarkdownReports.svg)](https://zenodo.org/badge/latestdoi/20391/vertesy/MarkdownReports)

# News - MarkdownReports


## News

### Version 4.5.2

In a major work to form installable R-packages from all my function libraries, I reorganized the codebase as below:



<img width="1005" alt="R-package Tree" src="https://user-images.githubusercontent.com/5101911/143560128-065d8a49-0283-4a3a-9448-540fa424d0ef.png">



MarkdownReports v4.5.2 Relies on

- [Stringendo](https://github.com/vertesy/Stringendo)
- [ReadWriter](https://github.com/vertesy/ReadWriter)
- [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2)
- [MarkdownHelpers](https://github.com/vertesy/MarkdownHelpers)

... and provides functions for

- [ggExpress](https://github.com/vertesy/ggExpress)
- [SeuratUtils](https://github.com/vertesy/SeuratUtils)

### Version 4.3.2 is released

In a major work to form installable R-packages from all my function libraries, I reorganized the codebase as below:

<img width="915" alt="Package Reorganisation Diagram" src="https://user-images.githubusercontent.com/5101911/140038110-b0e843cf-10c7-45c7-87dc-0525fafb0f57.png">

MarkdownReports v4.3.2 Relies on
- [Stringendo](https://github.com/vertesy/Stringendo)
- [ReadWriter](https://github.com/vertesy/ReadWriter)
- [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2)

... and provides functions for
- [ggExpress](https://github.com/vertesy/ggExpress)
- [SeuratUtils](https://github.com/vertesy/SeuratUtils).



### Version 4.1.0 is ready and installed by default.

1. See legacy/old version at: https://github.com/vertesy/MarkdownReports.v2.9.5
2. See development version at: https://github.com/vertesy/MarkdownReportsDev


### New features:

- **Formatted session info (sessionInfo)**
- Updated dependencies
- Many functions are more stable and versatile

In case you find a bug, please report. Try the [development version](https://github.com/vertesy/MarkdownReportsDev/) or a legacy version. **Old versions** are under `MarkdownReports.LEGACY.VERSION.X.X.X`.



[3.1.1  is under legacy now]

### Version 3.1.1 is ready and installed by default.

1. See legacy/old version at: https://github.com/vertesy/MarkdownReports.v2.9.5
2. See development version at: https://github.com/vertesy/MarkdownReportsDev


### New features

- **Function argument names now mirror the `R base` argument names (99%).**

  - Think of `xlb >>> xlab`, or  `sub_ >>> sub`
- This however breaks the compatibility with earlier versions, so you might need to replace some function arguments
- **The package now can also work with png images.**

  - You can save files in png, which can be displayed inside the markdown file on windows 7.

  - You need to set `b.usepng=T` in `setup_MarkdownReports`: `setup_MarkdownReports(OutDir = "/Users/...blabla....", b.usepng=T)`
- The package contains multiple other bug fixes:

  - Self consistency: some missing functions moved from `CodeAndRoll.R` 
  - Table writing functions `md.tableWriter.DF.w.dimnames()` and `md.tableWriter.VEC.w.names()`
- Enhancements: 

  - `filter_HP(), filter_LP(), filter_MidPass()` show histogram
  - `whist()` can invite the above filter functions.
- Numerous other small fixes.



**Old version** is under `MarkdownReports.LEGACY.VERSION`.



 <br/><br/>

#### 
