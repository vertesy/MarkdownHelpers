# MarkdownHelpers ![status: active](https://raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/active.svg)
Helper functions (control and parsing) for [`MarkdownReports`](https://github.com/vertesy/MarkdownReports) and [`ggExpress`](https://github.com/vertesy/ggExpress). 



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

# Install dependencies
devtools::install_github(repo = "vertesy/Stringendo", ref = "main", upgrade = F)
devtools::install_github(repo = "vertesy/CodeAndRoll2", ref = "main", upgrade = F)
devtools::install_github(repo = "vertesy/ReadWriter", ref = "main", upgrade = F)

# Install MarkdownHelpers
devtools::install_github(repo = "vertesy/MarkdownHelpers", ref = "main", upgrade = F)

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



----------------------------------------------------------------------------------------------------

## Function relationships

 > (of connected functions)

 ```mermaid
 flowchart LR 

  ww.ttl_field(ww.ttl_field) --> unless.specified(unless.specified)
  ww.set.path_of_report(ww.set.path_of_report) --> ww.variable.and.path.exists(ww.variable.and.path.exists)
  ww.set.mdlink(ww.set.mdlink) --> ww.variable.and.path.exists(ww.variable.and.path.exists)
  ww.set.file.extension(ww.set.file.extension) --> unless.specified(unless.specified)
  ww.set.PlotName(ww.set.PlotName) --> ww.autoPlotName(ww.autoPlotName)
  wcolorize(wcolorize) --> color_check(color_check)
  md.import.table(md.import.table) --> md.tableWriter.DF.w.dimnames(md.tableWriter.DF.w.dimnames)
  md.import(md.import) --> ww.variable.and.path.exists(ww.variable.and.path.exists)
  md.image.linker(md.image.linker) --> unless.specified(unless.specified)
  md.image.linker(md.image.linker) --> llogit(llogit)
  md.LogSettingsFromList(md.LogSettingsFromList) --> md.tableWriter.DF.w.dimnames(md.tableWriter.DF.w.dimnames)
  md.List2Table(md.List2Table) --> md.tableWriter.DF.w.dimnames(md.tableWriter.DF.w.dimnames)
  md.LinkTable(md.LinkTable) --> md.tableWriter.DF.w.dimnames(md.tableWriter.DF.w.dimnames)
  lookup(lookup) --> llprint(llprint)
  llwrite_list(llwrite_list) --> llogit(llogit)
  llwrite_list(llwrite_list) --> llprint(llprint)
  filter_survival_length(filter_survival_length) --> llprint(llprint)
  filter_MidPass(filter_MidPass) --> ww.variable.and.path.exists(ww.variable.and.path.exists)
  filter_MidPass(filter_MidPass) --> llogit(llogit)
  filter_LP(filter_LP) --> ww.variable.and.path.exists(ww.variable.and.path.exists)
  filter_LP(filter_LP) --> llogit(llogit)
  llogit(llogit) --> ww.variable.and.path.exists(ww.variable.and.path.exists)
  filter_HP(filter_HP) --> ww.variable.and.path.exists(ww.variable.and.path.exists)
  filter_HP(filter_HP) --> llogit(llogit)
  llprint(llprint) --> ww.variable.and.path.exists(ww.variable.and.path.exists)
  combine.matrices.by.rowname.intersect(combine.matrices.by.rowname.intersect) --> llprint(llprint)
  ww.FnP_parser(ww.FnP_parser) --> ww.set.OutDir(ww.set.OutDir)
  color_check(color_check) --> ww.FnP_parser(ww.FnP_parser)

 ```

 *created by `convert_igraph_to_mermaid()`*

<br>

----------------------------------------------------------------------------------------------------

## List of Functions in MarkdownHelpers (38) 
Updated: 2024/10/24 16:04

- #### 1 `irequire()`
irequire. Load a package. If it does not exist, try to install it from CRAN.

- #### 2 `unless.specified()`
unless.specified. Return value X (TRUE by default) unless the variable is defined.  If defined, it returns the variable.

- #### 3 `TRUE.unless()`
TRUE.unless. Return TRUE unless the variable is defined. If defined, it returns the value of the variable.

- #### 4 `FALSE.unless()`
FALSE.unless. Return FALSE unless the variable is defined. If defined, it returns the value of the variable.

- #### 5 `lookup()`
lookup. Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of results.

- #### 6 `combine.matrices.by.rowname.intersect()`
combine.matrices.by.rowname.intersect. Combine two matrices by their rownames intersect.

- #### 7 `ww.variable.and.path.exists()`
ww.variable.and.path.exists. Check if a variable name is defined, and if so, does the path (to a file) stored in that   variable points to an existing directory?

- #### 8 `llprint()`
llprint. Collapse by white spaces a sentence from any variable passed on to the function.  Print the sentence to the screen and write it to your markdown report file,  if the "path_of_report" variable is defined.

- #### 9 `llogit()`
llogit. Collapse by white spaces a sentence from any variable passed on to the function.  llogit() writes it to your markdown report file, if the "path_of_report" variable is defined.  It does not print the sentence to the screen.

- #### 10 `md.write.as.list()`
md.write.as.list. Writes a vector as a (numbered) list into the report file.

- #### 11 `md.image.linker()`
md.image.linker. Format a markdown image reference (link) to a .pdf and .png versions of graph,  and insert both links to the markdown report, set by "path_of_report".  If the "b.png4Github" variable is set, the .png-link is set up such,  that you can upload the whole report with the .png image into your GitHub repo's wiki,  under "Reports"/OutDir/ (Reports is a literal string, OutDir is the last/deepest  directory name in the "OutDir" variable. See create_set_OutDir() function.).  This function is called by the ~wplot functions.

- #### 12 `llwrite_list()`
llwrite_list. Print a list object from R, one element per line, into your markdown report

- #### 13 `md.import()`
md.import. Import and concatenated an external markdown or text file to the report

- #### 14 `md.LogSettingsFromList()`
md.LogSettingsFromList. Log the parameters & settings used in the script and stored in a list, in a table format   in the report.

- #### 15 `md.List2Table()`
md.List2Table. Broader variant of md.LogSettingsFromList(). Log the values (col2) from a  named (col1) list, in a table format in the report.

- #### 16 `md.tableWriter.DF.w.dimnames()`
md.tableWriter.DF.w.dimnames. Take an R data frame with row- and column- names, parse a markdown table from it,  and write it to the markdown report, set by "path_of_report".

- #### 17 `md.tableWriter.VEC.w.names()`
md.tableWriter.VEC.w.names. Take an R vector with names, parse a markdown table from it, and write it to the markdown report,   set by "path_of_report".

- #### 18 `md.LinkTable()`
md.LinkTable. Take a dataframe where every entry is a string containing an html link, parse and write out.   a properly formatted markdown table.

- #### 19 `md.import.table()`
md.import.table. Import a table (.csv, or tab seprated values, .tsv file) and write it  in markdown format to the report.

- #### 20 `filter_HP()`
filter_HP. Filter values that fall between above high-pass-threshold (X >). 

- #### 21 `filter_LP()`
filter_LP. Filter values that fall below the low-pass threshold (X <).

- #### 22 `filter_MidPass()`
filter_MidPass. Filter values that fall above high-pass-threshold !(X >= )! and below  the low-pass threshold (X <).

- #### 23 `ww.FnP_parser()`
ww.FnP_parser. Internal Function. Parses the full path from the filename & location of the file.

- #### 24 `ww.variable.and.path.exists()`
ww.variable.and.path.exists. Check if a variable name is defined, and if so, does the path (to a file) stored in that   variable points to an existing directory?

- #### 25 `ww.variable.exists.and.true()`
ww.variable.exists.and.true. Check if a variable name is defined, and if so, is it TRUE

- #### 26 `ww.set.OutDir()`
ww.set.OutDir. Checks if global variable OutDir is defined. If not, it returns the current  working directory

- #### 27 `ww.set.path_of_report()`
ww.set.path_of_report. Checks if global variable path_of_report is defined. If not,  it defines it as Analysis.md in the current working directory  @importFrom Stringendo iprint  @examples ww.set.path_of_report() 

- #### 28 `ww.set.PlotName()`
ww.set.PlotName. Generates a plotname (use if none is specified)  @importFrom Stringendo iprint  @examples ww.set.PlotName() 

- #### 29 `ww.set.mdlink()`
ww.set.mdlink. Internal function. Sets inserting a markdown link to the image  (created by the wplot* function that calls this function) only if 'path_of_report' is defined   and 'b.mdlink' is defined as TRUE.

- #### 30 `ww.md.image.link.parser()`
ww.md.image.link.parser. Format a markdown image reference (link) from the file path to the file.  It can parse the file path, if you pass it in separate variables and strings.  E.g. ww.md.image.link.parser(Directory, "MyImage.png").

- #### 31 `ww.ttl_field()`
ww.ttl_field. Internal function. Creates the string written into the PDF files "Title' (metadata) field.

- #### 32 `ww.autoPlotName()`
ww.autoPlotName. Internal function. Creates automatic plot and file-names.

- #### 33 `ww.assign_to_global()`
ww.assign_to_global. A function loading results to the global environment.  Source: https://stackoverflow.com/questions/28180989/

- #### 34 `try.dev.off()`
try.dev.off. Tries to close R graphical devices without raising an error.

- #### 35 `jjpegA4()`
jjpegA4. Setup an A4 size jpeg.

- #### 36 `color_check()`
color_check. Display the colors encoded by the numbers / color-ID-s you pass on to this function

- #### 37 `wcolorize()`
wcolorize. Generate color palettes. Input: a vector with categories, can be numbers or strings.  Handles repeating values. Output: color vector of equal length as input.  Optionally it can ouput a list where an extra element lists the  categories (simply using unique would remove the names). See example.  Some color scale depend on packages "colorRamps", or "gplots". 

- #### 38 `filter_survival_length()`
filter_survival_length. Parse a sentence reporting the % of filter survival.




------

*In 2021, function libraries got reorganized as below:*

<img width="1005" alt="R-package Tree" src="https://user-images.githubusercontent.com/5101911/143560128-065d8a49-0283-4a3a-9448-540fa424d0ef.png">

# 
