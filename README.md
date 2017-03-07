# summarytools version 0.6.9 (in development)

This development version introduces major improvements and I appeal to useRs to test it out before I can upload it to R-CRAN! The most important changes are:
  - An added cross-tabulation function 
  - Much improved support for `by()` and `with()` functions 
  - Ordering of frequency tables by counts now possible  
  - Appending content to existing html reports now possible 

Changes that break backward compatibility:
  - To create an ouput file, the `print()` or `view()` functions must be used. The file= parameter is therefore deprecated with other functions. The reason for this is that several options were added to the `print()` / `view()` functions, and that passing every parameter to those would add much redundancy in the code and complicate maintenance. 
  - In `cleartmp()`, the what= parameter has been replaced by all= ; it is still possible to use `cleartmp("all")`, but easier is now to just use `cleartmp(1)` or `cleartmp(TRUE)`.

# How to install the development version

```r
install.packages("devtools")
library(devtools)
install_github('dcomtois/summarytools', ref='dev')
```

## Introducing cross-tabulation function ctable()

Adding to the package's main three functions `freq()`, `descr()` and `dfSummary()`, `ctable()` creates cross-tabulations allowing flexibility over:
 - Marginal totals being displayed or not 
 - Type of proportions to include in the table (row, column, or total) 

While the html output is satisfying, the plain text / markdown output is far from perfect. But until rmarkdown supports dimnames, using ftable is the best I could find. Please let me know if you find an interesting alternative.

## New features to try out

Using the view() function, you can now
 - display (in html) objects created with `by()` 
 - append content to existing html files using `append = TRUE` 
 
## Improvements to existing functions

In previous versions, **variable names** were problematic when `descr()` or `freq()` were called
 - via `by()` 
 - via `with()` 
 - via a combination of `by()` and `with()` 

Now all variable names should be displayed properly.

## Final notes

The package comes with no guarantees. It is a work in progress and feedback / feature requests are most welcome. Just write me an email at dominic.comtois (at) gmail.com, or open an [Issue](https://github.com/dcomtois/summarytools/issues) if you find a bug.
