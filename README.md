# [summarytools: An _R_ Package For Descriptive Statistics](https://github.com/dcomtois/summarytools)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/summarytools)](https://cran.r-project.org/package=summarytools)
[![](http://cranlogs.r-pkg.org/badges/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html)
[![](http://cranlogs.r-pkg.org/badges/grand-total/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html)
[![Rdoc](http://www.rdocumentation.org/badges/version/summarytools)](http://www.rdocumentation.org/packages/summarytools)


## Latest News
Version 0.8.3 brings several improvements to summarytools. I'm making it available here on GitHub before submitting it to CRAN. For details on the changes, please see the NEWS file, as well as the rest of this document.

Two vignettes complement information found on this page:  
- [Introduction to summarytools](https://cdn.rawgit.com/dcomtois/summarytools/dev-current/inst/doc/Introduction.html)  
- [Recommendations for Using summarytools With Rmarkdown](https://cdn.rawgit.com/dcomtois/summarytools/dev-current/inst/doc/Recommendations-rmarkdown.html)

## What is summarytools?

*summarytools* is an [R](http://r-project.org) package providing tools to _neatly and quickly summarize data_. Its main purpose is to provide hassle-free functions that every _R_ programmer once wished were included in base R:

- `descr()` : **descriptive statistics** with all common univariate statistics for numerical vectors.
- `freq()` : **frequency tables** with proportions, cumulative proportions and missing data information.
- `ctable()` : **cross-tabulations** between two factors or any discrete data, with total, rows or columns proportions.
- `dfSummary()` : Extensive **data frame summaries** that facilitate data cleaning and firsthand evaluation.

Most commercial statistical software suites provide a wide range of functions and procedures out-of-the-box, making it very simple to create, with code or with a few point-and-click actions, well-formatted reports. For most tasks not relying on advanced statistical methods, _summarytools_ allows you to do just that. 

# How to install

To benefit from all the latests fixes, install it from GitHub:

```r
install.packages("devtools")
library(devtools)
install_github('dcomtois/summarytools')
```

To install the most recent version on the _R-CRAN_ repository, just type into your _R_ console: 

```r
install.packages("summarytools")
```

For the most **up-to-date version** that has all the latest features **but** might also contain bugs (which can be fixed rapidly in most cases): 

```r
install.packages("devtools")
library(devtools)
install_github('dcomtois/summarytools', ref='dev-current')
```

You can also see the source code and documentation on the official _R_ site [here](http://cran.r-project.org/web/packages/summarytools/).

# A First Example

Using the _iris_ sample data frame, we'll jump right to the most popular function in the package, `dfSummary` (*Data Frame Summary*). 

With the following 2 lines of code, we'll generate a summary report for ''iris`` and have it displayed in [_RStudio_](http://www.rstudio.com/)'s Viewer pane:

```r
library(summarytools)
view(dfSummary(iris))
```

![Example of dfSummary Output displayed in RStudio's viewer](img/dfSummary_in_RStudio_Viewer.png)

##### Building on the strengths of [pander](https://github.com/Rapporter/pander) and [htmltools](http://cran.r-project.org/web/packages/htmltools/index.html), the outputs produced by summarytools can be:

- Displayed in plain text in the _R_ console (default behaviour) 
- Used in \emph{Rmd} documents and \emph{knitted} along with other text and _R_ output
- Written to _html_  files that fire up in [_RStudio_](http://www.rstudio.com/)'s Viewer pane or in your system's default browser
- Written to plain text files / _markdown_ text files 

# Four Core Functions

## 1 - Frequency tables with `freq()`

The `freq()` function generates a table of frequencies with counts and proportions.

```r
library(summarytools)
freq(iris$Species)
```

```
Frequencies 
iris$Species 
Type: Factor (unordered) 

                   Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
---------------- ------ --------- -------------- --------- --------------
          setosa     50     33.33          33.33     33.33          33.33
      versicolor     50     33.33          66.67     33.33          66.67
       virginica     50     33.33         100.00     33.33         100.00
            <NA>      0                               0.00         100.00
           Total    150    100.00         100.00    100.00         100.00
```

## 2 - Descriptive (univariate) statistics with `descr()`
The `descr()` function generates common central tendency statistics and measures of dispersion for numerical data. It can handle single vectors as well as dataframes, in which case it just ignores non-numerical columns.

We'll use the _rmarkdown_ style for the next example:

```r
descr(iris, style = "rmarkdown")
```

### Descriptive Statistics: iris 
**N:** 150 

|          &nbsp; | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width |
|----------------:|-------------:|------------:|-------------:|------------:|
|        **Mean** |         5.84 |        3.06 |         3.76 |        1.20 |
|     **Std.Dev** |         0.83 |        0.44 |         1.77 |        0.76 |
|         **Min** |         4.30 |        2.00 |         1.00 |        0.10 |
|      **Median** |         5.80 |        3.00 |         4.35 |        1.30 |
|         **Max** |         7.90 |        4.40 |         6.90 |        2.50 |
|         **MAD** |         1.04 |        0.44 |         1.85 |        1.04 |
|         **IQR** |         1.30 |        0.50 |         3.50 |        1.50 |
|          **CV** |         7.06 |        7.01 |         2.13 |        1.57 |
|    **Skewness** |         0.31 |        0.31 |        -0.27 |       -0.10 |
| **SE.Skewness** |         0.20 |        0.20 |         0.20 |        0.20 |
|    **Kurtosis** |        -0.61 |        0.14 |        -1.42 |       -1.36 |
|     **N.Valid** |       150.00 |      150.00 |       150.00 |      150.00 |
|   **Pct.Valid** |       100.00 |      100.00 |       100.00 |      100.00 |```

##### Transposing and selecting the stats you need 

If your eyes/brain prefer seeing things the other way around, just use "transpose=TRUE". Here, we also select only the statistics we wish to see: 

```r
descr(iris, stats = c("mean", "sd", "min", "med", "max"), transpose = TRUE, 
      omit.headings = TRUE, style = "rmarkdown")
```

|           &nbsp; | Mean | Std.Dev |  Min | Median |  Max |
|-----------------:|-----:|--------:|-----:|-------:|-----:|
| **Sepal.Length** | 5.84 |    0.83 | 4.30 |   5.80 | 7.90 |
|  **Sepal.Width** | 3.06 |    0.44 | 2.00 |   3.00 | 4.40 |
| **Petal.Length** | 3.76 |    1.77 | 1.00 |   4.35 | 6.90 |
|  **Petal.Width** | 1.20 |    0.76 | 0.10 |   1.30 | 2.50 |


## 3 - Cross-tabulations with `ctable()`

Here we'll use a sample data frame included in the package (_tobacco_), which contains simulated data. Say we want to cross-tabulate variables `smoker` and `diseased`. By default, `ctable()` gives row proportions, so we don't need to specify any additionnal parameter. 

Here, instead of `ctable(tobacco$smoker, tobacco$diseased)`, we'll make use of the `with()` (R-base) function:

```r
data("tobacco")
with(tobacco, ctable(smoker, diseased))
``` 

```
Cross-Tabulation / Row proportions 
smoker * diseased 
Data Frame: tobacco 
-------- ---------- ------------- ------------- ---------------
           diseased           Yes            No           Total
  smoker                                                       
     Yes              125 (41.9%)   173 (58.1%)    298 (100.0%)
      No               99 (14.1%)   603 (85.9%)    702 (100.0%)
   Total              224 (22.4%)   776 (77.6%)   1000 (100.0%)
-------- ---------- ------------- ------------- ---------------
```

Note that _markdown_ has no support (yet) for multi-line headers. Here is what the _html_ version looks like:

```r
with(tobacco, view(ctable(smoker, diseased)))
```

![html output for ctable](img/ctable-with-row-props.png)


It is possible to display _column_, _total_, or no proportions at all. We can also omit the marginal totals to have a simple 2x2 table. (Results not shown).

```r
with(tobacco, view(ctable(smoker, diseased, prop = 'n', totals = FALSE)))
```

## 4 - Data Frame Summaries

As seen earlier, the `dfSummary()` function gives information for all variables in a singe table. Version 0.8.0 introduced graphs for both ascii and _html_ tables.

```r
dfSummary(tobacco)
```

```
Data Frame Summary   
tobacco   
N: 1000 
--------------------------------------------------------------------------------------------------------------------
No   Variable        Stats / Values              Freqs (% of Valid)    Text Graph                Valid     Missing  
---- --------------- --------------------------- --------------------- ------------------------- --------- ---------
1    gender          1. F                        489 (50.0%)           IIIIIIIIIIIIIIII          978       22       
     [factor]        2. M                        489 (50.0%)           IIIIIIIIIIIIIIII          (97.8%)   (2.2%)   

2    age             mean (sd) : 49.6 (18.29)    63 distinct val.      .     .     . . . :       975       25       
     [numeric]       min < med < max :                                 : : : : : . : : : :       (97.5%)   (2.5%)   
                     18 < 50 < 80                                      : : : : : : : : : :                          
                     IQR (CV) : 32 (0.37)                              : : : : : : : : : :                          
                                                                       : : : : : : : : : :                          

3    age.gr          1. 18-34                    258 (26.5%)           IIIIIIIIIIIII             975       25       
     [factor]        2. 35-50                    241 (24.7%)           IIIIIIIIIIII              (97.5%)   (2.5%)   
                     3. 51-70                    317 (32.5%)           IIIIIIIIIIIIIIII                             
                     4. 71 +                     159 (16.3%)           IIIIIIII                                     

4    BMI             mean (sd) : 25.73 (4.49)    974 distinct val.               :               974       26       
     [numeric]       min < med < max :                                         : : :             (97.4%)   (2.6%)   
                     8.83 < 25.62 < 39.44                                      : : :                                
                     IQR (CV) : 5.72 (0.17)                                  : : : : :                              
                                                                           . : : : : : .                            

5    smoker          1. Yes                      298 (29.8%)           IIIIII                    1000      0        
     [factor]        2. No                       702 (70.2%)           IIIIIIIIIIIIIIII          (100%)    (0%)     

6    cigs.per.day    mean (sd) : 6.78 (11.88)    37 distinct val.      :                         965       35       
     [numeric]       min < med < max :                                 :                         (96.5%)   (3.5%)   
                     0 < 0 < 40                                        :                                            
                     IQR (CV) : 11 (1.75)                              :                                            
                                                                       :   . .           .                          

7    diseased        1. Yes                      224 (22.4%)           IIII                      1000      0        
     [factor]        2. No                       776 (77.6%)           IIIIIIIIIIIIIIII          (100%)    (0%)     

8    disease         1. Hypertension             36 (16.2%)            IIIIIIIIIIIIIIII          222       778      
     [character]     2. Cancer                   34 (15.3%)            IIIIIIIIIIIIIII           (22.2%)   (77.8%)  
                     3. Cholesterol              21 ( 9.5%)            IIIIIIIII                                    
                     4. Heart                    20 ( 9.0%)            IIIIIIII                                     
                     5. Pulmonary                20 ( 9.0%)            IIIIIIII                                     
                     6. Musculoskeletal          19 ( 8.6%)            IIIIIIII                                     
                     7. Diabetes                 14 ( 6.3%)            IIIIII                                       
                     8. Hearing                  14 ( 6.3%)            IIIIII                                       
                     9. Digestive                12 ( 5.4%)            IIIII                                        
                     10. Hypotension             11 ( 5.0%)            IIII                                         
                     [ 3 others ]                21 ( 9.4%)            IIIIIIIII                                    

9    samp.wgts       mean (sd) : 1 (0.08)        0.86!: 267 (26.7%)    IIIIIIIIIIIII             1000      0        
     [numeric]       min < med < max :           1.04!: 249 (24.9%)    IIIIIIIIIIII              (100%)    (0%)     
                     0.86 < 1.04 < 1.06          1.05!: 324 (32.4%)    IIIIIIIIIIIIIIII                             
                     IQR (CV) : 0.19 (0.08)      1.06!: 160 (16.0%)    IIIIIII                                      
                                                 ! rounded                                                          
--------------------------------------------------------------------------------------------------------------------
```


## Using summarytools in Rmarkdown documents

_summarytools_ uses the [pander](https://github.com/Rapporter/pander) package to generate plain-text content, and _htmltools_ to generate _html_. Both types of outputs can be used in Rmarkdown, according to our preferences. See [this vignette](https://cran.r-project.org/web/packages/summarytools/vignettes/Recommendations-rmarkdown.html) to get all the details, but if you're in a hurry, here are a few tips to get good results:

- Always set the `knitr` chunk option `results = 'asis'`. You can do this globally or on a chunk-by-chunk basis. See [this page](https://yihui.name/knitr/options/) for more information.
- To use the 'render' method, set up your .Rmd document so it includes summarytool's css (see example)


### Example (result not shown)
````
---
title: "RMarkdown using summarytools"
output: 
  html_document: 
    css: C:/R/win-library/3.4/summarytools/includes/stylesheets/summarytools.css
---

```{r, results='asis'}
library(summarytools)  
freq(tobacco$smoker, style='rmarkdown')  

print(dfSummary(tobacco, style = 'grid', plain.ascii = FALSE, graph.magnif = 0.85), 
      method = 'render', omit.headings = TRUE)
```

````

## The print() and view() functions

_summarytools_ has a generic `print()` method, `print.summarytools`. By default, its `method` argument is set to `'pander'`. To easily display _html_ outputs in _RStudio_'s Viewer, we use the `view()` function, which acts as a wrapper around the generic `print()` function, this time using `method = 'viewer'`.  When used outside _RStudio_, the `method` falls back on `'browser'` and the report is opened with your system's default browser.

## Using by() to Split Results By Sub-Groups

With `freq()` and `descr()` you can use _R_'s base function `by()` to have statistics split by a ventilation variable. _R_ returns a `list` containing _summarytools_ objects. Using the `view()` function with those objects is necessary in order to have non-redundant and clean section headings.

Example: Using the _iris_ data frame, we will display descriptive statistics broken down by Species.

```r
# First save the results
iris_stats_by_species <- by(data = iris, 
                            INDICES = iris$Species, 
                            FUN = descr, stats = c("mean", "sd", "min", "med", "max"), 
                            transpose = TRUE)
# Then use view(), like so:
view(iris_stats_by_species, method = "pander", style = "rmarkdown")
```
### Descriptive Statistics   
**Data Frame:** iris   
**Group:** Species = setosa   
**N:** 50   

|           &nbsp; | Mean | Std.Dev |  Min | Median |  Max |
|-----------------:|-----:|--------:|-----:|-------:|-----:|
| **Sepal.Length** | 5.01 |    0.35 | 4.30 |   5.00 | 5.80 |
|  **Sepal.Width** | 3.43 |    0.38 | 2.30 |   3.40 | 4.40 |
| **Petal.Length** | 1.46 |    0.17 | 1.00 |   1.50 | 1.90 |
|  **Petal.Width** | 0.25 |    0.11 | 0.10 |   0.20 | 0.60 |

**Group:** Species = versicolor   
**N:** 50   

|           &nbsp; | Mean | Std.Dev |  Min | Median |  Max |
|-----------------:|-----:|--------:|-----:|-------:|-----:|
| **Sepal.Length** | 5.94 |    0.52 | 4.90 |   5.90 | 7.00 |
|  **Sepal.Width** | 2.77 |    0.31 | 2.00 |   2.80 | 3.40 |
| **Petal.Length** | 4.26 |    0.47 | 3.00 |   4.35 | 5.10 |
|  **Petal.Width** | 1.33 |    0.20 | 1.00 |   1.30 | 1.80 |

**Group:** Species = virginica   
**N:** 50   

|           &nbsp; | Mean | Std.Dev |  Min | Median |  Max |
|-----------------:|-----:|--------:|-----:|-------:|-----:|
| **Sepal.Length** | 6.59 |    0.64 | 4.90 |   6.50 | 7.90 |
|  **Sepal.Width** | 2.97 |    0.32 | 2.20 |   3.00 | 3.80 |
| **Petal.Length** | 5.55 |    0.55 | 4.50 |   5.55 | 6.90 |
|  **Petal.Width** | 2.03 |    0.27 | 1.40 |   2.00 | 2.50 |


To see an _html_ version of these results, we'd simply do:

```r
view(iris_stats_by_species)
```
## Special Case - Using descr() With by() For One Variable Only

Instead of showing several tables having one column each, the `view()` will assemble the results into a single table:

```r
BMI_by_age <- with(tobacco, 
                   by(BMI, age.gr, descr, 
                      stats = c("mean", "sd", "min", "med", "max")))
view(BMI_by_age, "pander", style = "rmarkdown")
```

### Descriptive Statistics   
**BMI, split by age.gr**   
**Data Frame:** tobacco   

       &nbsp;   age.gr = 18-34   35-50   51-70    71 +
------------- ---------------- ------- ------- -------
     **Mean**            23.84   25.11   26.91   27.45
  **Std.Dev**             4.23    4.34    4.26    4.37
      **Min**             8.83   10.35    9.01   16.36
   **Median**            24.04   25.11   26.77   27.52
      **Max**            34.84   39.44   39.21   38.37


(avec style = 'rmarkdown' -- celle ci-haut avait seulement plain.ascii=TRUE)
### Descriptive Statistics   
**BMI, split by age.gr**   
**Data Frame:** tobacco   

|      &nbsp; | age.gr = 18-34 | 35-50 | 51-70 |  71 + |
|------------:|---------------:|------:|------:|------:|
|    **Mean** |          23.84 | 25.11 | 26.91 | 27.45 |
| **Std.Dev** |           4.23 |  4.34 |  4.26 |  4.37 |
|     **Min** |           8.83 | 10.35 |  9.01 | 16.36 |
|  **Median** |          24.04 | 25.11 | 26.77 | 27.52 |
|     **Max** |          34.84 | 39.44 | 39.21 | 38.37 |


### And the transposed version would look like this:

|    &nbsp; |  Mean | Std.Dev |   Min | Median |   Max |
|----------:|------:|--------:|------:|-------:|------:|
| **18-34** | 23.84 |    4.23 |  8.83 |  24.04 | 34.84 |
| **35-50** | 25.11 |    4.34 | 10.35 |  25.11 | 39.44 |
| **51-70** | 26.91 |    4.26 |  9.01 |  26.77 | 39.21 |
|  **71 +** | 27.45 |    4.37 | 16.36 |  27.52 | 38.37 |

## Using lapply() to Show Several freq() tables at once

As is the case for `by()`, the `view()` function is useful in making results nice and tidy.

```r
tobacco_subset <- tobacco[ ,c("gender", "age.gr", "smoker")]
freq_tables <- lapply(tobacco_subset, freq)
view(freq_tables, method = "pander", style = "rmarkdown")
```

### Frequencies   
**gender  **   
**Data frame:** tobacco_subset   
**Type:** Factor (unordered)   

|     &nbsp; | Freq | % Valid | % Valid Cum. | % Total | % Total Cum. |
|-----------:|-----:|--------:|-------------:|--------:|-------------:|
|      **F** |  489 |   50.00 |        50.00 |   48.90 |        48.90 |
|      **M** |  489 |   50.00 |       100.00 |   48.90 |        97.80 |
| **\<NA\>** |   22 |         |              |    2.20 |       100.00 |
|  **Total** | 1000 |  100.00 |       100.00 |  100.00 |       100.00 |

**age.gr  **   
**Type:** Factor (unordered)   

|     &nbsp; | Freq | % Valid | % Valid Cum. | % Total | % Total Cum. |
|-----------:|-----:|--------:|-------------:|--------:|-------------:|
|  **18-34** |  258 |   26.46 |        26.46 |   25.80 |        25.80 |
|  **35-50** |  241 |   24.72 |        51.18 |   24.10 |        49.90 |
|  **51-70** |  317 |   32.51 |        83.69 |   31.70 |        81.60 |
|   **71 +** |  159 |   16.31 |       100.00 |   15.90 |        97.50 |
| **\<NA\>** |   25 |         |              |    2.50 |       100.00 |
|  **Total** | 1000 |  100.00 |       100.00 |  100.00 |       100.00 |

**smoker  **   
**Type:** Factor (unordered)   

|     &nbsp; | Freq | % Valid | % Valid Cum. | % Total | % Total Cum. |
|-----------:|-----:|--------:|-------------:|--------:|-------------:|
|    **Yes** |  298 |   29.80 |        29.80 |   29.80 |        29.80 |
|     **No** |  702 |   70.20 |       100.00 |   70.20 |       100.00 |
| **\<NA\>** |    0 |         |              |    0.00 |       100.00 |
|  **Total** | 1000 |  100.00 |       100.00 |  100.00 |       100.00 |


## Writing to files

The console will always tell you the location of the temporary _html_ file that is created in the process. However, you can specify the name and location of that file explicitly if you need to reuse it later on:

```r
view(iris_stats_by_species, file = "~/iris_stats_by_species.html")
```

There is also an `append = ` boolean parameter for adding content to existing reports.

## Global options

Version 0.8.3 introduced the following set of global options:

  - `round.digits` = `2`
  - `plain.ascii` = `TRUE`
  - `omit.headings` = `FALSE` (if using in a markdown document or a shiny app, setting this to `TRUE` might be preferable
  - `footnote` = `'default'` (set to empty string or `NA` to omit footnote)
  - `display.labels` = `TRUE`
  - `freq.totals` = `TRUE`
  - `freq.display.nas` = `TRUE`
  - `ctable.totals` = `TRUE`
  - `ctable.prop` = `'r'` (display *r*ow proportions by default)
  - `descr.stats` = `'all'`
  - `descr.transpose` = `FALSE`
  - `bootstrap.css` = `TRUE` (if using in a markdown document or a shiny app, setting this to `FALSE` might be preferable
  - `custom.css` = `NA` 
  - `escape.pipe` = `FALSE`

Examples:
```r
st_options()                      # display all global options' values
st_options('round.digits')        # display only one option
st_options('omit.headings', TRUE) # change an option's value
```

## Bootstrap CSS

Version 0.8.0 of _summarytools_ uses _RStudio_'s [htmltools package](http://cran.r-project.org/web/packages/htmltools/index.html) and version 4 of [Bootstrap](http://getbootstrap.com/)'s cascading stylesheets.

It is also possible to include your own _css_ if you wish to customize the look of the output tables. See the documentation for the package's `print.summarytools()` function for details.

## Overriding formatting attributes

When a _summarytools_ object is stored, its formatting attributes are stored with it. However, you can override most of them when using the `print()` and `view()` functions. 

Example: 

```r
st_options(plain.ascii, FALSE)     # this allows outputting markdown-ready content by default
age_stats <- freq(tobacco$age.gr)  # age_stats contains a regular output for freq including 
                                   # headings, NA reporting and a Totals row.
print(age_stats, report.nas = FALSE, 
                 totals = FALSE, 
                 omit.headings = TRUE)
```

     &nbsp;   Freq       %   % Cum.
----------- ------ ------- --------
  **18-34**    258   26.46    26.46
  **35-50**    241   24.72    51.18
  **51-70**    317   32.51    83.69
   **71 +**    159   16.31   100.00
   

## The what.is() function

...helps you figure out quickly what an object is by:

- Putting together the object's class(es), type (typeof), mode, storage mode, length, dim and object.size, all in a single table
- Extending the `is()` function in a way that the object is tested against __all__ functions starting with `is.` -- see [this post on StackOverflow](http://stackoverflow.com/questions/8855589/a-comprehensive-survey-of-the-types-of-things-in-r-mode-and-class-and-type/26435993#26435993) for details;
- Giving a list of the object's attributes names and length (c.g. rownames, dimnames, labels, etc.)

#### Examples

```r
> what.is(c)
$properties
      property    value
1        class function
2       typeof  builtin
3         mode function
4 storage.mode function
5          dim         
6       length        1
7  object.size 56 Bytes

$extensive.is
[1] "is.function"  "is.primitive" "is.recursive"

$function.type
[1] "primitive" "generic"  


> what.is(NaN)
$properties
      property    value
1        class  numeric
2       typeof   double
3         mode  numeric
4 storage.mode   double
5          dim         
6       length        1
7  object.size 48 Bytes

$extensive.is
[1] "is.atomic"  "is.double"  "is.na"      "is.nan"     "is.numeric" "is.vector" 

$object.type
[1] "base"

```

# News

  - To support shiny apps, it is now possible to adjust the size of the graphs in `dfSummary()`, as well as omit the core Bootstrap CSS from the outputs.
  - _summarytools_ now has **global options** (see `?st_options`)
  - `dfSummary()` now supports Date / POSIX data 
  - in `descr()`, Q1 and Q3 are now included. Also, the order of the statistics specified with `stats =` is retained for the output. 
  
## Final notes

The package comes with no guarantees. It is a work in progress and feedback / feature requests are welcome. Just send me an email (dominic.comtois (at) gmail.com), or open an [Issue](https://github.com/dcomtois/summarytools/issues) if you find a bug.

Also, the package grew significantly larger, and maintaining it all by myself is time consuming. If you would like to contribute, please get in touch, I'd greatly appreciate the help.
