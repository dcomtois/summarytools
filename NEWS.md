Version 0.9.3
-------------
 - New functions:
   + `tb()` turns `freq()` and `descr()` outputs into "tidy" tibbles
   + `use_custom_lang()` replaces `useTranslations()` and triggers an
     _Open File..._ dialog when no argument is supplied   
   + `define_keywords()` allows defining translatable terms in GUI and
     optionally save the results in a _csv_ file (through _Save File..._ dialog)
 - Deprecated functions:
   + `byst()` had to be dropped because of issues related to objects names;
     so only `stby()` is accepted from now on
   + `useTranslations()` has been replaced by `use_custom_lang()`
 - In `freq()`: 
   + new parameter `cumul` allows turning on or off cumulative proportions
   + `order` parameter: "names", "freq", and "levels" values now have their
     counterparts "-names" (or "names-"), "-freq" and "-levels"
   + a new parameter `rows` has been added; it allows subsetting the output 
     table either with a numeric vector, a character vector, or a single
     search string (regex)
 - In `ctable()`: 
   + Added weights support
   + Added logical argument "chisq.test" to display chi-square results below
     the cross-tabulation table
 - In `dfSummary()`: 
   + Added content specific to email addresses -- valid, invalid, duplicates
 - Added translations : 
   + Portuguese ("pt")
   + Turkish ("tr")
   + Russian ("ru")
   

Version 0.9.2
-------------
No changes (re-submission of 0.9.1 to CRAN)


Version 0.9.1
-------------
For users updating solely from CRAN, this is a **major** update. _Many_ changes
were introduced since version 0.8.8 (versions 0.8.9 and 0.9.0 were released 
solely on _GitHub_). Please refer to the README file, the two vignettes and the
information below for all the details.

In this version:

 - `stby()`, a summarytools-specific version of `by()`, is 
   introduced. It is **highly recommended** that you use it instead of `by()`; 
   its syntax is identical and it greatly simplifies the printing of the 
   generated objects
 - In `dfSummary()`:  
   + 'max.tbl.height' allows printing summaries in scrollable windows (useful
     in _.Rmd_ when a data frame contains numerous variables)  
   + setting 'tmp.img.dir' allows the inclusion of _png_ graphs in Rmarkdown
     documents when using pander method in combination with arguments
     plain.ascii = FALSE and style = "grid"  
   + Platform-specific _png_ device types are used, improving image quality  
 - Several examples are added to all main functions; use the `example()`
   function to access them  


Version 0.9.0
-------------  
 - **Output translations** are introduced. For instance, setting
   `st_options(lang='fr')` gives access to French translations. Spanish ('es')
   translations are also available. 
 - Function `useTranslations()` allows using custom translations; see the
   introductory vignette for details  
 - In `descr()`, the weighting variable (when used) is automatically removed 
   from the list of variables to analyze  
 - In `dfSummary()`, images are processed using functions from the **magick**
   package, improving the general layout of the output tables
 - Improved support for **magrittr** operators


Version 0.8.9
-------------  
 - In `dfSummary()`:  
   + Number of columns and number of duplicates added to the _headings_ 
     section  
   + Integer sequences as well as **UPC/EAN codes** are detected and identified  
   + Statistics for unary / binary data are simplified  
   + Bar charts now reflect frequencies across variables  
 - In `descr()`, the 'stats' parameter accepts values "common" and "fivenum"  
 - With `st_options()`, setting multiple options at once is now possible; all
   options have their own parameter (the legacy way of setting options is still
   supported)  
 - More parameters can be overridden when calling `print()` or `view()` - 
   refer to the `print()` method's documentation to learn more  
   
Two *somewhat* backward-compatibility breaking changes:  
 - The 'omit.headings' parameter is replaced by the more straightforward
   (and still boolean) 'headings'. The former is still supported but will 
   disappear in a future release (possibly 0.9.2)  
 - Row subsetting is no longer displayed in the headings section, as it was
   error-prone  
   
Special thanks to Paul Feitsma for his numerous suggestions.


Version 0.8.8
-------------   
 - Fixed character encoding issues  
 - In `dfSummary()`:
   + Fixed an issue with `dfSummary()` where reported percentages could exceed
     100% under specific circumstances  
   + Fixed issue with groups not being properly updated when used in _Shiny_
     apps  


Version 0.8.7
-------------  
 - Fixed an issue with `dfSummary()` when missing values were present along
   with whole numbers  
 - Fixed an issue with `descr()` where group was not shown for the first 
   group when omitting headings  


Version 0.8.6
-------------  
 - In `dfSummary()`:  
   + _"Label"_ column now shows proper line breaks in the html versions  
   + In lists of values / frequencies, digits are omitted for integer
     variables and for numerics containing whole numbers only  
   + Fixed an error when using the pipe (`%>%`) operator  
 - In `descr()`, fixed a calculation error for coefficient of variation (cv)  
 - In `ctable()` html outputs, '<' and '>' are properly escaped when appearing
   in row or column names  


Version 0.8.5
-------------  
 - In `dfSummary()`:  
   + Time intervals in `dfSummary()` now use `lubridate::as.period()`  
   + Line feeds in ASCII barplots now displayed correctly  
   + String trimming is applied consistently  
 - In `ctable()`, argument 'useNA' now correctly accepts value "no"  


Version 0.8.4
-------------  
 - Method for calculating number of bins in `dfSummary()` histograms changed 
   (from `nclass.FD()` to `nclass.Sturges()`)  
 - Removed extra space in `dfSummary()` with time objects  
 - Allowed one more value for frequency counts in `dfSummary()` when
   _"[1 other value]"_ was displayed; this actual value is now displayed instead  


Version 0.8.3
-------------  
 - Introduced global options with `st_options()`  
 - In all functions, argument 'split.table' becomes 'split.tables' as per
   changes in the 'pander' package  
 - Argument 'omit.headings' added to all main functions  
 - New logical options for `freq()`: 'totals' and 'display.nas'  
 - In `descr()`, _Q1_ and _Q3_ were added; also, the order in the 'stats' 
   argument is now reflected in the output table  
 - In `dfSummary()`:  
   + Number alignment improvements  
   + Fixed frequencies not appearing when value 0 was present
   + Fixed arguments 'valid.col' and 'na.col' being unresponsive  
   + Fixed warnings when generating graphics columns  
   + 'graph.magnif' argument now allows shrinking or enlarging graphs  
 - For `print()` and `view()`, argument 'html.table.class' is now called
   'table.classes' and its usage is simplified (please refer to the 
   corresponding help files for details  
 - CSS class 'st-small' can be used to make html tables smaller by slightly
   reducing font size and cell padding


Version 0.8.2
-------------  
 - Fixed performance issue with numerical data having a very large range  
 - Fixed missing line feeds in `dfSummary()` bar charts  
 - Added support for Date / POSIXt objects in `dfSummary()`  
 - Improved support for `lapply()` when used with `freq()`  


Version 0.8.1
-------------  
 - Added vignettes  
 - Improved handling of negative column indexing by the parsing function  
 - Fixed issue with all-NA factors in `dfSummary()`  
 - Fixed issues with graphs in `dfSummary()`  
 - Removed accentuated characters from docs  


Version 0.8.0
-------------  
 - Introducing graphs in `dfSummary()`  
 - Improved alignment of numbers in both html and ASCII tables  
 - Cleanup in css and upgrade to Bootstrap 4 beta  
 - Added rudimentary support for `lapply()` to be used with `freq()`  
 - Improved support for `by()` and `with()` with `descr()`, `freq()`, 
   and `ctable()`  
 
Backward-compatibility notes: in `dfSummary()`, parameter name 'display.labels' 
has been changed to 'labels.col', for consistency reasons. Also, see Notes 
for Version 0.6.9 about the 'file' parameter.


Version 0.7.0
-------------  
Another _GitHub-only_ release  
 - Improved alignment in cells having counts + proportions  
 - Updated vignette to reflect latest changes and added examples using the
   example datasets "exams" and "tobacco"  
 - `dfSummary()`'s last column now includes counts and percentages for both 
   valid and missing data   
 - Internal change: **Roxygen2** is now used to generate documentation  


Version 0.6.9
-------------  
In this _GitHub-only_ release:  
 - Introduced `ctable()` for cross-tabulations  
 - Extended support for printing objects created using `by()` and/or `with()`:
   variable names, labels and by-groups are now displayed correctly  
 - `view()` is now more than just a wrapper function for the `print()` method;
   it is the function to use when printing an object created with `by()`  
 - Appending to summarytools-generated html files is now possible
 - Most pander options stored in summarytools objects can be overridden by
   `print()` or `view()`  
 - `freq()` has an new parameter, 'order', allowing to order rows by count 
   rather than values  
 - Alignment of numbers in `descr()` observations table has been improved
 
An important change causing a minor break in backward compatibility: 
the 'file' parameter must now be used with `print()` or `view()`; its
use with other functions is now deprecated. 


Version 0.6.5
-------------  
 - Fixed a problem with `dfSummary()` which arose when number of factor levels 
   exceeded max.distinct.values  
 - Improved the way `dfSummary()` reports frequencies for character variables  
 - Fixed problems with outputs when using weights  
 - Added hash markup to table headings for better markdown integration  
 - Added an option to the `print()` method to suppress the footnote in HTML 
   outputs  


Version 0.6
-----------  
 - Added Introductory vignette
 - Fixed _markdown_ output that would not render strings such as <NA>  
 - Improved multiline tables line feeds  
 - Improved sample datasets  
 - Removed _Bootstrap_ content not likely to be used  
 - Changed the way `method = "browser"` sends file path to browser for better 
   cross-platform compatibility  
 - Improved results when using `by()`  


Version 0.5
-----------  
For this _GitHub-only_ release:  
 - Function `descr()` now supports weights  
 - Output from `what.is()` has been simplified  
 - Other changes are transparent to the user, but make the internals
   more consistent across functions  


Version 0.4
-----------  
Added `cat()` functions to fully support knitr's document generation. Also added
sample datasets so that users can experiment using summarytools functions with
them. `freq()` now supports weights.


Version 0.3
-----------  
Another round of major changes  

  - Bringing in HTML table built with **htmltools** and viewable in RStudio's
    Viewer  
  - Function `desc()` is renamed `descr()` (mainly to avoid conflict with
    `plyr::desc()`)  
  - Argument 'echo' is deprecated; either display with **pander** or use
    `as.table()`  
  - Returned objects are now of class "summarytools" and have several 
    attributes that are used by `print.summarytools()`  
	  + _st.type_ : one of "freq", "descr" and "dfSummary"  
  	+ _date_ : date at which the function was called  
  	+ _var.name_ and _var.label_ : for `freq()`, and also `descr()` when a single
  	   vector is used  
  	+ _pander.args_ : "style", "justify", "plain.ascii", "split.table"  
  - `print.summarytools()` has argument 'method' that can be one of "pander",
    "viewer", or "browser", the last two being used to display an HTML version
    of the output, using Bootstrap's css (https://getbootstrap.com)  
  - Row indexing is "detected" and reported (function `.parse.arg.x()` takes
    care of this)  
  - Rounding now only occurs at the printing stage  


Version 0.2
-----------  
Several major changes since version 0.1  

  - `unistats()` is now called `desc()`
  - `frequencies()` is now called `freq()`   
  - `properties()` is now called `prop()`  
  - Shortcuts have been added to keep backward-compatibility  
  - `desc()` now accepts data frames as first argument; factors and character
    columns will be ignored  
  - `desc()` can be transposed to suit one's preferences  
  - `freq()` just returns a matrix-table, not a list anymore  
  - In `desc()` and `freq()`, no more argument 'display.label'. Those are
    displayed automatically when present  
  - **rapportools** is used instead of **Hmisc** for variable labels  
  - Function `properties()` was removed for now. May be reintegrated in a future
    version  

Initial Release
