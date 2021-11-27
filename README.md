# MarkdownHelpers
Helper functions (control and parsing) for MarkdownReports and ggExpress



*In 2021, function libraries got reorganized as below:*

<img width="1005" alt="R-package Tree" src="https://user-images.githubusercontent.com/5101911/143560128-065d8a49-0283-4a3a-9448-540fa424d0ef.png">

# Installation

It depends on:

- [Stringendo](https://github.com/vertesy/Stringendo)
- [ReadWriter](https://github.com/vertesy/ReadWriter)
- [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2)

... and provides functions for

- [MarkdownReports](https://github.com/vertesy/MarkdownReports)
- [ggExpress](https://github.com/vertesy/ggExpress)
- [Seurat.utils](https://github.com/vertesy/Seurat.utils)
- [Seurat.pipeline](https://github.com/vertesy/Seurat.pipeline)



You can install all of them directly from **GitHub** via **devtools** with one R command:

```R
# install.packages("devtools"); # If you don't have it.
require("devtools")

# Install dependencies
devtools::install_github(repo = "vertesy/Stringendo", upgrade = F)
devtools::install_github(repo = "vertesy/CodeAndRoll2", upgrade = F)
devtools::install_github(repo = "vertesy/ReadWriter", upgrade = F)

# Install MarkdownHelpers
devtools::install_github(repo = "vertesy/MarkdownHelpers", upgrade = F)

```

...then simply load the package:

```R
require("MarkdownHelpers")
```

Alternatively, you simply source it from the web. 
*This way function help will not work, and you will have no local copy of the code on your hard drive.*

```R
source("https://raw.githubusercontent.com/vertesy/MarkdownHelpers/master/R/MarkdownHelpers.R")
```

<br>

## Usage

```r
# It is best to load all dependencies:
require('Stringendo')
require('ReadWriter')
require('CodeAndRoll2')
require('MarkdownReports')


require('MarkdownHelpers')
MarkdownReports::setup_MarkdownReports()
# ------------------
"Used by other packages mostly"
```



##### Most  `MarkdownHelpers` function write into a markdown file.  

##### That file is defined in `path_of_report`, by first calling `MarkdownReports::setup_MarkdownReports()`. 

#####  `MarkdownReports` and `MarkdownHelpers` works with such _background variables_ defined by `setup_MarkdownReports()` in the global env .



<br>

### List of Functions



1. `stopif()`
2. `irequire()`
3. `unless.specified()`
4. `lookup()`
5. `combine.matrices.by.rowname.intersect()`
6. `ww.variable.and.path.exists()`
7. `llprint()`
8. `llogit()`
9. `llwrite_list()`
10. `md.import()`
11. `md.LogSettingsFromList()`
12. `md.List2Table()`
13. `md.LinkTable()`
14. `ww.variable.and.path.exists()`
15. `ww.variable.exists.and.true()`
16. `ww.set.OutDir()`
17. `ww.set.path_of_report()`
18. `ww.set.PlotName()`
19. `ww.set.mdlink()`
20. `ww.md.image.link.parser()`
21. `ww.ttl_field()`
22. `ww.autoPlotName()`
23. `ww.assign_to_global()`
24. `try.dev.off()`
25. `jjpegA4()`
26. `color_check()`
27. `filter_survival_length()`
