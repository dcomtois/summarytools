# summarytools 1.0.1 (2022-05-19)
 - This version only includes minors fixes requested by CRAN.

# summarytools 1.0.0 (2021-07-27)

 - In `dfSummary()`:  
   + It is now possible to control which statistics to show in the
     *Freqs / Values* column (see `help("st_options", "summarytools")`
     for examples)   
   + In *html* outputs, tables are better aligned horizontally (categories >>
     counts >> charts); if misalignment occurs, adjusting `graph.magnif` should
     resolve it 
   + List-type columns and `Inf` values no longer generate errors 
   + `tmp.img.dir` can be left to `NA` when `style = "grid"`  
   + Fixed typo in attribute name `Dataf.rame.label` 
   + Removal of grouping variables is now consistent across all languages 
 - In `descr()`:  
   + Fixed headings being shown even if `headings=FALSE` (when using
    `stby()` or `dplyr::group_by()`) 
 - In `ctable()`:  
   + Fixed row/column names not always properly displayed   
   + Fixed risk ratios showing when only odds ratios should  
   + Fixed error when `prop="none"` with integer data  
 - Selected heading elements can be totally omitted by one of two ways: 
   + Setting their *value* to empty string using `print()` or
     `view()` parameters (in `?print.summarytools`, refer to list of arguments
     that can be used to override heading elements)  
   + Using `define_keywords()` and setting the *heading's label* to empty
     string 
 - Improved functionality for customized terms / translations (see
   `vignette("introduction", "summarytools")` for details) 
 - `fix-valign.tex` is now in the *includes* directory for use with
   *R Markdown* when creating *pdf* documents with `dfSummary()` outputs -
   see `vignette("rmarkdown", )
   
 - Navigation links and table of contents were added to introductory vignette,
   making it is easier to navigate 

# summarytools 0.9.9 (2021-02-04)

 - Style "jira" has been added to reflect pander's support for it. 
 - Documentation has been reviewed and improved. 
 - In `dfSummary()`:
   + When generating a `dfSummary()` in Rmarkdown using `method = "render"`,
     it is possible to set `tmp.img.dir = NA`. It must still be defined 
     (not as `NA`) when `method = "pander"` and `style = "grid"`.
   + Grouping variable(s) are now excluded from results when using
     `stby()` or `dpyr::group_by()`. Use `keep.grp.vars = TRUE` to replicate
     previous behavior.
   + Removed an extra (empty) line in text graphs
 - In `ctable()` and `freq()`:
   + Fixed bug with integers
 - The `ctable.round.digits` was added to the list of `st_options()` (there
   is already a global `round.digits` option, but it uses `2` as default,
   while `1` is a more sensible value for `ctable()`.
 - `print.summarytools()` now removes titles from headings when keyword 
   "title.function" is set to `NA` or empty string. 

# summarytools 0.9.8 (2020-12-10)

Version 0.9.8 is essentially the CRAN release of the 0.9.7 _GitHub-Only_ release
which saw gradual changes being implemented over the course of several months.
See changes listed under 0.9.7 for changes since last CRAN release (0.9.6)

# summarytools 0.9.7 (2020-05-21)

_GitHub-only_ release - this was a constantly evolving version to be eventually
released as 0.9.8 on CRAN when it reached maturity.

 - Added shortcut function `stview()` pointing to
   `summarytools::view()`. This avoids potential conflicts with
   other packages using the more and more popular `view()` function
   (notably, **tibble**, part of the **tidyverse** family, defines
   `view()` as an alias for `View()`) 
 - Enforced adequate number formatting using `format()` internally, so that
   using the following optional arguments with any core function or with 
   `print()` or `view()` will produce expected results:
   + decimal.mark, big.mark, small.mark
   + nsmall, digits
   + scientific
   + big.interval, small.interval (limited support)
 - Fixed a bug arising when an object created using a language other
   than the active one (`st_options("lang")`) was displayed 
 - Improved string encoding behavior
 - Added global option "char.split" to control maximum number of characters 
   allowed in `descr()` and `ctable()` column headings showing variable names
 - *html* footnotes are now always enclosed within a `<p>` tag
 - Updated hex logo and added a favicon in html reports 
 - Simplified and improved performance of what.is()
 - In `dfSummary()`: 
   + Added support for list-type columns 
   + Improved performance by optimizing barcode detection and blank character
     replacements, which are the two main bottlenecks 
   + Fixed a bug with barcode detection 
   + Changed default value of round.numbers to 1 (which was de facto applied)
   + round.numbers doesn't affect proportions - only 1 decimal is shown, always
   + Made slight adjustments to the html graphs appearance 
   + Improved alignment of Freq cell when numerical values are shown
   + Replaced "!" with "*" for rounded-values notice 
   + Fixed issue where grouped `dfSummary()` tables would end up
     nested in one another 
   + Added a check for numerical variables having infinitesimal
     variability, in which case a linear transformation is applied to
     obtain better histograms 
 - In `descr()`:
   + Added the `order` argument that gives the option to display variables in
     their order of appearance in the data or in a custom order (as opposed to
     the default behavior which is to display them alphabetically sorted)
 - In `freq()`, values for the `order` argument are now singular (backward
   compatibility is preserved for now)
   + levels --> level
   + names --> name
 - Three global options (set via `st_options()` were added:
   + dfSummary.style ("multiline" by default; can also be set to "grid")
   + freq.cumul (TRUE by default; set to FALSE to hide cumulative proportions)
   + freq.ignore.threshold (25 by default; when feeding `freq()` a whole data
     frame, this number determines how many distinct values are allowed for
     numerical variables. Above that number, the variable will be ignored)


# summarytools 0.9.6 (2020-03-01)

 - In `ctable()`:
   + Added Odds Ratio and Risk Ratio (aka Relative Risk) 
     statistics with 95% C.I.'s
   + Fixed issue with chi-square statistic not reporting
     appropriate values
   + Fixed html alignment of statistics below the table
     (now centering based on table width as it should)
 - In `dfSummary()`, fixed an issue arising when a very
   large range of numeric values exists in a column

# summarytools 0.9.5 (2020-02-10)

 - Eliminated automatic check for X11 capabilities as it caused problems on some
   systems; the user can instead set global option `st_options(use.x11 = FALSE)`
   if encountering problems
 - To simplify installation on Unix-like systems (including Mac OS), the
   `RCurl::base64Encode()` function used to create ascii-encoded graphs in
   *html* documents isn't used anymore; `base64enc::base64encode()` is used instead
 - When saving outputs to *.Rmd* documents; 'plain.ascii' is now automatically set
   to FALSE and 'style' is automatically set to "rmarkdown", in accordance with
   with the way *.md* documents are generated
 - Fixed bug arising with data frames called "data"
 - `freq.silent` was added to global options
 - Weights are now supported for `freq()` used in conjunction with `stby()` or 
   `dplyr::group_by()`
 - Weights are also supported for `ctable()` used in conjunction with `stby()` (but
   not with `dplyr::group_by()`)
 - Improvements and fixes for `dfSummary()`:
   + Fixed null graphic device appearing in *RGui* and non-GUI interfaces
   + Calling `summarytools::dfSummary()` (without loading the package) is now
     possible
   + Improved *Rmarkdown* compatibility
 - Improvements and fixes for `descr()`:
   + When `descr()` with `stby()`, results are no longer assembled into a single
     table if more than one grouping variables are used
   + Fixed bug arising when using `stby()` with several grouping variables

# summarytools 0.9.4 (2019-08-24)

 - Added support for **dplyr**'s `group_by()` function as an alternative
   to `stby()`
 - Added support for **magrittr** `%$%` operator 
 - Added support for **pipeR** `%>>%` operator 
 - `freq()` recognizes factor level "(Missing)" from `forcats::fct_explicit_na`
   as `NA`'s
 - For `freq()` objects, `collapse` boolean parameter has been added as an
   experimental feature (must be set in the `print()` method)
 - Improved output when grouping by more than one variable, either with
   `stby()` or `dplyr::group_by()`
 - `tb()` supports objects having several grouping variables
 - `tb()` has an added parameter "na.rm" for `freq()` objects
 - Improved how `descr()` deals with empty vectors and invalid weights
 - Fixed a problem with `freq()` arising when using sampling weights while no
   missing values were present
 
# summarytools 0.9.3 (2019-04-11)

 - Function `tb()` turns `freq()` and `descr()` outputs into "tidy" tibbles
 - Function `define_keywords()` allows defining translatable terms in GUI and
   optionally save the results in a _csv_ file (through _Save File..._ dialog)
 - Function `use_custom_lang()` replaces `useTranslations()` and triggers an
   _Open File..._ dialog when no argument is supplied   
 - In `freq()`:
   + A new parameter `cumul` allows turning on or off cumulative
     proportions
   + The _order_ parameter values "names", "freq", and "levels"
     now have their counterparts "-names" (or "names-"), "-freq" 
     and "-levels"
   + A new parameter `rows` has been added; it allows subsetting
     the output table either with a numeric vector, a character vector, or a
     single search string (regular expression)
 - In `ctable()`:
   + Added support for weights
   + Added logical argument "chisq.test" to display chi-square 
   results below the cross-tabulation table
 - In `dfSummary()`, added content specific to email addresses: valid, 
   invalid, duplicates
 - Added translations : Portuguese ("pt"), Turkish ("tr"), and Russian ("ru")

### Deprecated functions:

 - `byst()` had to be dropped because of issues related to objects names;
    only `stby()` is accepted from now on
 - `useTranslations()` has been replaced by `use_custom_lang()`
   

# summarytools 0.9.2 (2019-02-22)

No changes (re-submission of 0.9.1 to CRAN)


# summarytools 0.9.1 (2019-02-20)

For users updating solely from CRAN, this is a **major** update. _Many_ changes
were introduced since version 0.8.8 (versions 0.8.9 and 0.9.0 were released 
solely on _GitHub_). Please refer to the README file, the two vignettes and the
information below for all the details.

 - `stby()`, a summarytools-specific version of `by()`, is 
   introduced. It is **highly recommended** that you use it instead of `by()`; 
   its syntax is identical and it greatly simplifies the printing of the 
   generated objects
 - In `dfSummary()`:
   + 'max.tbl.height' allows printing summaries in scrollable 
     windows (useful in _.Rmd_ when a data frame contains numerous variables)  
   + Setting 'tmp.img.dir' allows the inclusion of _png_ graphs
     in Rmarkdown documents when using pander method in combination with 
     arguments plain.ascii = FALSE and style = "grid"  
   + Platform-specific _png_ device types are used, improving image quality  
 - Several examples are added to all main functions; use the `example()`
   function to access them  

# summarytools 0.9.0 (2019-01-05)

 - **Output translations** are introduced. For instance, setting
   `st_options(lang='fr')` gives access to French translations. Spanish ('es')
   translations are also available. 
 - Function `useTranslations()` (which in later versions becomes
   `use_custom_lang()`) allows using custom translations
 - In `descr()`, the weight variable (when used) is automatically removed 
   from the list of variables to analyze  
 - In `dfSummary()`, images are processed using functions from the **magick**
   package, improving the general layout of the output tables
 - Improved support for **magrittr** operators


# summarytools 0.8.9 (2018-12-17)

 - In `dfSummary()`:  
   + Number of columns and number of duplicates added to the _headings_ 
     section  
   + Integer sequences as well as **UPC/EAN codes** are detected and identified  
   + Statistics for unary / binary data are simplified  
   + Dimension of the bars in barplots now reflect frequencies relative to the
     whole dataset, allowing comparisons across variables  
 - In `descr()`, the 'stats' parameter accepts values "common" and "fivenum"  
 - With `st_options()`, setting multiple options at once is now possible; all
   options have their own parameter (the legacy way of setting options is still
   supported)  
 - More parameters can be overridden when calling `print()` or `view()` - 
   refer to the `print()` method's documentation to learn more  
 - The 'omit.headings' parameter is replaced by the more straightforward
   (and still boolean) 'headings'. The former is still supported but will 
   disappear in a future release (possibly 0.9.2)  
 - Row subsetting is no longer displayed in the headings section, as it was
   error-prone  

Special thanks to Paul Feitsma for his numerous suggestions.


# summarytools 0.8.8 (2018-10-07)

 - Fixed character encoding issues  
 - In `dfSummary()`:
   + Fixed an issue with `dfSummary()` where reported percentages could exceed
     100% under specific circumstances  
   + Fixed issue with groups not being properly updated when used in _Shiny_
     apps  


# summarytools 0.8.7 (2018-07-23)

 - Fixed an issue with `dfSummary()` when missing values were present along
   with whole numbers  
 - Fixed an issue with `descr()` where group was not shown for the first 
   group when omitting headings  


# summarytools 0.8.6 (2018-07-20)

 - In `dfSummary()`:  
   + _"Label"_ column now shows proper line breaks in the html versions  
   + In lists of values / frequencies, digits are omitted for integer
     variables and for numerics containing whole numbers only  
   + Fixed an error when using the pipe (`%>%`) operator  
 - In `descr()`, fixed a calculation error for coefficient of variation (cv)  
 - In `ctable()` html outputs, '<' and '>' are properly escaped when appearing
   in row or column names  


# summarytools 0.8.5 (2018-06-17)

 - In `dfSummary()`:  
   + Time intervals in `dfSummary()` now use `lubridate::as.period()`  
   + Line feeds in ASCII barplots now displayed correctly  
   + String trimming is applied consistently  
 - In `ctable()`, argument 'useNA' now correctly accepts value "no"  


# summarytools 0.8.4 (2018-06-07)

 - Method for calculating number of bins in `dfSummary()` histograms changed 
   (from `nclass.FD()` to `nclass.Sturges()`)  
 - Removed extra space in `dfSummary()` with time objects  
 - Allowed one more value for frequency counts in `dfSummary()` when
   _"[1 other value]"_ was displayed; this actual value is now displayed instead  


# summarytools 0.8.3 (2018-04-16)

 - Introduced global options with `st_options()`  
 - New logical options for `freq()`: 'totals' and 'display.nas'  
 - In `descr()`, _Q1_ and _Q3_ were added; also, the order in the 'stats' 
   argument is now reflected in the output table  
 - In all functions, argument 'split.table' becomes 'split.tables' as per
   changes in the 'pander' package  
 - Argument 'omit.headings' added to all main functions  
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


# summarytools 0.8.2 (2018-02-11)

 - Added support for Date / POSIXt objects in `dfSummary()`  
 - Improved support for `lapply()` when used with `freq()`  
 - Fixed performance issue with numerical data having a very large range  
 - Fixed missing line feeds in `dfSummary()` bar charts  


# summarytools 0.8.1 (2018-01-14)

 - Fixed issue with all-NA factors in `dfSummary()`  
 - Fixed issues with graphs in `dfSummary()`  
 - Added vignettes  
 - Improved handling of negative column indexing by the parsing function  
 - Removed accentuated characters from docs  


# summarytools 0.8.0 (2017-12-10)

 - Introducing graphs in `dfSummary()`  
 - Added rudimentary support for `lapply()` to be used with `freq()`  
 - Improved alignment of numbers in both html and ASCII tables  
 - Cleanup in css and upgrade to Bootstrap 4 beta  
 - Improved support for `by()` and `with()` with `descr()`, `freq()`, 
   and `ctable()`  
 
### Backward-compatibility Note

In `dfSummary()`, parameter name 'display.labels' has been changed to
'labels.col' for consistency reasons. Also, see Notes for Version 0.6.9 about
the 'file' parameter.


# summarytools 0.7.0

_GitHub-only_ release  

 - Improved alignment in cells having counts + proportions  
 - Updated vignette to reflect latest changes and added examples using the
   example datasets "exams" and "tobacco"  
 - `dfSummary()`'s last column now includes counts and percentages for both 
   valid and missing data   
 - Internal change: **Roxygen2** is now used to generate documentation  


# summarytools 0.6.9

_GitHub-only_ release

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
 
### Backward Compatibility Note

The 'file' parameter must now be used with `print()` or `view()`; its
use with other functions is now deprecated. 


# summarytools 0.6.5 (2016-12-05)

 - Improved the way `dfSummary()` reports frequencies for character variables  
 - Fixed problems with outputs when using weights  
 - Added hash markup to table headings for better markdown integration  
 - Added an option to the `print()` method to suppress the footnote in HTML 
   outputs  
 - Fixed a problem with `dfSummary()` which arose when number of factor levels 
   exceeded max.distinct.values  


# summarytools 0.6 (2016-11-21)

 - Added Introductory vignette
 - Fixed _markdown_ output that would not render strings such as <NA>  
 - Improved multiline tables line feeds  
 - Improved sample datasets  
 - Removed _Bootstrap_ content not likely to be used  
 - Changed the way `method = "browser"` sends file path to browser for better 
   cross-platform compatibility  
 - Improved results when using `by()`  


# summarytools 0.5

_GitHub-only_ release

 - Function `descr()` now supports weights  
 - Output from `what.is()` has been simplified  
 - Other changes are transparent to the user, but make the internals
   more consistent across functions  


# summarytools 0.4 (2015-08-05)

 - `freq()` now supports weights.
 - Better **knitr** integration
 - Added sample datasets


# summarytools 0.3 (2015-03-11)

  - `view()` allows opening HTML tables in RStudio's Viewer 
  - `desc()` is renamed `descr()` 
  - Returned objects are now of class "summarytools" and have several 
    attributes that are used by `print.summarytools()`  
  - `print.summarytools()` has argument 'method' that can be one of "pander",
    "viewer", or "browser", the last two being used to display an HTML version
    of the output, using Bootstrap's CSS (https://getbootstrap.com)  
  - Row indexing is "detected" and reported
  - Rounding occurs when results are displayed (non-rounded results are stored)
  - Argument 'echo' is deprecated 

# summarytools 0.2 (2014-11-25)

  - `unistats()` is now called `desc()` 
  - `frequencies()` is now called `freq()` 
  - `desc()` now accepts data frames as first argument; factors and character
    columns will be ignored  
  - `desc()` results tables can be transposed  
  - `freq()` returns a matrix-table rather than a list  
  - **rapportools** is used instead of **Hmisc** for variable labels  
  - Function `properties()` was removed.

# summarytools 0.1 (2014-08-11)

Initial Release
