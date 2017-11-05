# summarytools version 0.8.0
CRAN stats: [![](http://cranlogs.r-pkg.org/badges/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html) [![](http://cranlogs.r-pkg.org/badges/grand-total/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html)  
Other stats: [![Research software impact](http://depsy.org/api/person/338759/badge.svg)](http://depsy.org/person/338759)

## Latest News
I've been working on `summarytools` on and off over the last few months. Now I'm happy to introduce version 0.8.0, here on GitHub at first, and then push it to R-Cran. 

The most notable changes are:
  - A cross-tabulation function, `ctable()` 
  - Improved support for `by()`, `with()` and `lapply()` functions 
  - `dfSummary()` now optionnaly shows barplots and histograms 
  - New layouts, most noticeable in html reports 
  - Added flexibility on many fronts 

## What is summarytools?

*summarytools* is an [R](http://r-project.org) package providing tools to _neatly and quickly summarize data_. Its main purpose is to provide hassle-free functions that every R programmer once wished were included in base R:

- Extensive **data frame summaries** that facilitate data cleaning and firsthand evaluation
- **frequency tables** with proportions, cumulative proportions and missing data information.
- **cross-tabulations** between two factors or any discrete data, with total, rows or columns proportions.
- **descriptive statistics** with all common univariate statistics for numerical vectors

One of its goals is to make it easier for newcomeRs who might be used to other statistical software which usually provide a wide range of functions and procedures out-of-the-box, making it very simple to create, with a few point-and-click actions, some pretty decent quality material. For most tasks not relying on advanced statistical methods, summarytools should be well enough to explore the data and create nicely formatted outputs to use in presentations or to send to colleagues and associates. Just a few of the available features are:
- Support for variable and data frame labels 
- Support for sampling weights in frequency tables and descriptive statistics 
- Support for creation of output files (both text/_rmarkdown_ and _html_) 
- Quite a bit of flexibility on the output formats: 
  . Plain-ascii (ideal for quickly looking at results directly in the console) 
  . _Rmarkdown_ (ideal for creating presentations and publish your work) 
  . _Html_ (easily generated and viewed, especially if you use RStudio, and are much more appealing than the plain-ascii outputs).
  
### Why use summarytools?

- You're not satisfied with R's out-of-the-box functions to get to know your data in an efficient manner 
- You're looking for flexibility in terms of outputs  
- You want to be able to get up and running with your analysis even if you're not an expert R programmer just yet

### An example

As a first example, we'll use what is the most popular function in the package, `dfSummary` (*Data Frame Summary*). We'll apply it to the _iris_ sample data frame.

With the following 2 lines of code, we'll generate a summary report for ''tobacco`` and have it displayed in [_RStudio_](http://www.rstudio.com/)'s Viewer pane:

```r
library(summarytools)
view(dfSummary(iris))
```

![Example of dfSummary Output displayed in RStudio's viewer](img/dfSummary_in_RStudio_Viewer.png)

##### Building on the strengths of [pander](https://github.com/Rapporter/pander) and [htmltools](http://cran.r-project.org/web/packages/htmltools/index.html), the summary reports produced by summarytools can be:

- Displayed as plain text in the R console (default behaviour) 
- Written to plain text files / [markdown](http://daringfireball.net/projects/markdown/) text files 
- Written to _html_  files that fire up in [_RStudio_](http://www.rstudio.com/)'s Viewer pane or in your system's default browser. 

##### Also, all functions:
- Support _variable labels_ and _data frame labels_. 
- Return table or dataframe objects for further manipulation if needed. 

# How to install

To install the **latest stable version** of **summarytools** on the _R-CRAN_ repository, just type into your R console: 

```r
install.packages("summarytools")
```

For the most **up-to-date version** that has all the latest features **but** might also contain bugs (which can be fixed rapidly in most cases): 

```r
install.packages("devtools")
library(devtools)
install_github('dcomtois/summarytools', ref='dev-current')
```

You can also see the source code and documentation on the official R site [here](http://cran.r-project.org/web/packages/summarytools/).

# Demo of the 4 main functions with plain-ascii output

## Frequency tables with freq()

The `freq()` function generates a table of frequencies with counts and proportions.

```r
library(summarytools)
data(iris)
# We'll insert some NA's for illustration purposes
is.na(iris) <- matrix(sample(x = c(TRUE,FALSE), size = 150*5, 
                             replace = T, prob = c(.1,.9)),nrow = 150)
# ... and add a variable label for the Species column
label(iris$Species) <- "The Species (duh)"
freq(iris$Species)
```

```
Frequencies 
iris$Species 
Variable Label: The Species (duh) 
Type: Factor (unordered) 

                   Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
---------------- ------ --------- -------------- --------- --------------
          setosa     45     32.85          32.85     30.00          30.00
      versicolor     47     34.31          67.15     31.33          61.33
       virginica     45     32.85         100.00     30.00          91.33
            <NA>     13                               8.67         100.00
           Total    150    100.00         100.00    100.00         100.00
```

## Descriptive (univariate) statistics with descr()
The `descr()` function generates common central tendency statistics and measures of dispersion for numerical data. It can handle single vectors as well as dataframes, in which case it just ignores non-numerical columns.

#### descr() on the iris dataframe

```r
data(iris)
descr(iris)
Non-numerical variable(s) ignored: Species 
```

```
Non-numerical variable(s) ignored: Species

Descriptive Statistics: iris 
N: 150 

                    Sepal.Length   Sepal.Width   Petal.Length   Petal.Width
----------------- -------------- ------------- -------------- -------------
             Mean           5.88          3.05           3.80          1.17
          Std.Dev           0.83          0.44           1.76          0.76
              Min           4.40          2.00           1.00          0.10
           Median           5.80          3.00           4.40          1.30
              Max           7.90          4.40           6.90          2.50
              MAD           1.04          0.37           1.78          1.04
              IQR           1.35          0.50           3.50          1.50
               CV           7.04          6.96           2.16          1.55
         Skewness           0.30          0.34          -0.29         -0.06
      SE.Skewness           0.21          0.21           0.21          0.21
         Kurtosis          -0.64          0.26          -1.39         -1.37
          N.Valid         135.00        134.00         136.00        130.00
        Pct.Valid          90.00         89.33          90.67         86.67
```



##### Transposing and selecting the stats you need 

If your eyes/brain prefer seeing things the other way around, just use "transpose=TRUE". Here, we also select only the statistics we wish to see: 

```r
descr(iris, stats = c("mean", "sd", "min", "med", "max"), transpose = TRUE)
```

```
Descriptive Statistics: iris 
N: 150 

                     Mean   Std.Dev    Min   Median    Max
------------------ ------ --------- ------ -------- ------
      Sepal.Length   5.88      0.83   4.40     5.80   7.90
       Sepal.Width   3.05      0.44   2.00     3.00   4.40
      Petal.Length   3.80      1.76   1.00     4.40   6.90
       Petal.Width   1.17      0.76   0.10     1.30   2.50
```

## Cross-tabulations with ctable()

Here we'll use a sample data frame included in the package called `tobacco` and which contains simulated data. We want to cross-tabulate variables `smoker` and `diseased`. By default, ctable() gives row proportions, so we don't need to specify any additionnal parameter. 

Instead of `ctable(tobacco$smoker, tobacco$diseased)`, we'll make use of the `with()` function:

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

It is possible to display _column_, _total_, or no proportions at all. We can also omit the _Totals_, so at its simplest form, we would have:

```r
with(tobacco, ctable(smoker, diseased, prop = "n", totals = FALSE))
```

```
Cross-Tabulation 
smoker * diseased 
Data Frame: tobacco 
-------- ---------- ----- -----
           diseased   Yes    No
  smoker                       
     Yes              125   173
      No               99   603
-------- ---------- ----- -----
```

If the presentation is not too convincing at this stage, bare with me. We'll see down below that there are interesting alternatives to this plain-ascii console output.

## Dataframe summaries

The `dfSummary()` function gives information for all variables contained in a data frame. It 

```r
dfSummary(tobacco)
```

```
Data Frame Summary: tobacco                                                                         
N: 1000                                                                                             
-------------------------------------------------------------------------------------------------------------------------
No   Variable         Stats / Values               Freqs (% of Valid)     Text Graph                  Valid     Missing  
---- ---------------- ---------------------------- ---------------------- --------------------------- --------- ---------
1    gender           1. F                         489 (50.0%)            ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤            978       22       
     [factor]         2. M                         489 (50.0%)            ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤            (97.8%)   (2.2%)   
                                                                                                    
2    age              mean (sd) : 49.6 (18.29)     63 distinct val.       .     .         .   . :     975       25       
     [numeric]        min < med < max :                                   : . . : : :   : : . : :     (97.5%)   (2.5%)   
                      18 < 50 < 80                                        : : : : : : : : : : : :                        
                      IQR (CV) : 32 (0.37)                                : : : : : : : : : : : :                        
                                                                          : : : : : : : : : : : :                        
                                                                          : : : : : : : : : : : :                        
                                                                                                    
3    age.gr           1. 18-34                     258 (26.5%)            ¤¤¤¤¤¤¤¤¤¤¤¤¤               975       25       
     [factor]         2. 35-50                     241 (24.7%)            ¤¤¤¤¤¤¤¤¤¤¤¤                (97.5%)   (2.5%)   
                      3. 51-70                     317 (32.5%)            ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤                               
                      4. 71 +                      159 (16.3%)            ¤¤¤¤¤¤¤¤                                       
                                                                                                    
4    BMI              mean (sd) : 25.73 (4.49)     974 distinct val.                  :               974       26       
     [numeric]        min < med < max :                                             : : .             (97.4%)   (2.6%)   
                      8.83 < 25.62 < 39.44                                          : : :                                
                      IQR (CV) : 5.72 (0.17)                                      : : : : :                              
                                                                                  : : : : : .                            
                                                                              . : : : : : : : .                          
                                                                                                    
5    smoker           1. Yes                       298 (29.8%)            ¤¤¤¤¤¤                      1000      0        
     [factor]         2. No                        702 (70.2%)            ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤            (100%)    (0%)     
                                                                                                    
6    cigs.per.day     mean (sd) : 6.78 (11.88)     37 distinct val.       :                           965       35       
     [numeric]        min < med < max :                                   :                           (96.5%)   (3.5%)   
                      0 < 0 < 40                                          :                                              
                      IQR (CV) : 11 (1.75)                                :                                              
                                                                          :                                              
                                                                          :     .         .     .                        
                                                                                                    
7    diseased         1. Yes                       224 (22.4%)            ¤¤¤¤                        1000      0        
     [factor]         2. No                        776 (77.6%)            ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤            (100%)    (0%)     
                                                                                                    
8    disease          1. Hypertension              36 (16.2%)             ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤            222       778      
     [character]      2. Cancer                    34 (15.3%)             ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤             (22.2%)   (77.8%)  
                      3. Cholesterol               21 ( 9.5%)             ¤¤¤¤¤¤¤¤¤                                      
                      4. Heart                     20 ( 9.0%)             ¤¤¤¤¤¤¤¤                                       
                      5. Pulmonary                 20 ( 9.0%)             ¤¤¤¤¤¤¤¤                                       
                      6. Musculoskeletal           19 ( 8.6%)             ¤¤¤¤¤¤¤¤                                       
                      7. Diabetes                  14 ( 6.3%)             ¤¤¤¤¤¤                                         
                      8. Hearing                   14 ( 6.3%)             ¤¤¤¤¤¤                                         
                      9. Digestive                 12 ( 5.4%)             ¤¤¤¤¤                                          
                      10. Hypotension              11 ( 5.0%)             ¤¤¤¤                                           
                      [ 3 others ]                 21 ( 9.4%)             ¤¤¤¤¤¤¤¤¤                                      
                                                                                                    
9    samp.wgts        mean (sd) : 1 (0.08)         0.86*: 267 (26.7%)     ¤¤¤¤¤¤¤¤¤¤¤¤¤               1000      0        
     [numeric]        min < med < max :            1.04*: 249 (24.9%)     ¤¤¤¤¤¤¤¤¤¤¤¤                (100%)    (0%)     
                      0.86 < 1.04 < 1.06           1.05*: 324 (32.4%)     ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤                               
                      IQR (CV) : 0.19 (0.08)       1.06*: 160 (16.0%)     ¤¤¤¤¤¤¤                                        
                                                   * rounded                                                             
-------------------------------------------------------------------------------------------------------------------------
```

# Formatting options

So far we've only shown the default, plain-ascii console output. 

Thanks to Gergely Daróczi's [pander](https://github.com/Rapporter/pander) package, all functions can printout [markdown](http://daringfireball.net/projects/markdown/); just use the option `style="rmarkdown"`. That is useful for instance here on GitHub, where `.md` files are converted and displayed as _html_. Thanks to John MacFarlane's [Pandoc](http://johnmacfarlane.net/pandoc/), you can further convert markdown text files into a wide range of common formats such as _.pdf_, _.docx_ and _.odt_, among others. 

Here is an **example** of a markdown table, as processed by GitHub, using freq():


```r
> freq(iris$Species, style="rmarkdown", plain.ascii=FALSE, missing="---")
```
Dataframe name: iris  
 Variable name: Species  
Variable label: The Species (duh)  
          Date: 2014-12-05  

Frequencies

|           &nbsp; |   N |   %Valid |   %Cum.Valid |   %Total |   %Cum.Total |
|-----------------:|----:|---------:|-------------:|---------:|-------------:|
|       **setosa** |  46 |    33.58 |        33.58 |    30.67 |        30.67 |
|   **versicolor** |  45 |    32.85 |        66.42 |       30 |        60.67 |
|    **virginica** |  46 |    33.58 |          100 |    30.67 |        91.33 |
|       **\<NA\>** |  13 |      --- |          --- |     8.67 |          100 |
|        **Total** | 150 |      100 |          100 |      100 |          100 |

###### Two things to note here:

  1. We specified `plain.ascii=FALSE`. This allows additional markup in the text (here, the bold-typed row names, added automatically by [pander](https://github.com/Rapporter/pander)).
  2. We used the option `missing="---"`, to show that if we don't like seeing `NA`'s in our tables, it's quite easy to get rid of them or replace them with any character (or combination of characters).

To **learn more about _markdown_ and _rmarkdown_** formats, see [John MacFarlane's page](http://johnmacfarlane.net/pandoc/) and this [RStudio's R Markdown Quicktour](http://rmarkdown.rstudio.com/).

## Create and view html reports

Version 0.5 of _summarytools_ combines the strengths of the following packages and tools to generate basic html reports:

 - _RStudio_'s [htmltools package](http://cran.r-project.org/web/packages/htmltools/index.html)
 - The [xtable package](http://cran.r-project.org/web/packages/xtable/index.html)
 - [Bootstrap](http://getbootstrap.com/) cascading stylesheets

### Walkthrough

When you become familiar with the method, You can achieve this in just one operation, but let's have a detailed walkthrough on how to generate and visualize an _html_ report with **summarytools**.

- First, generate a _summarytools_ object using one of `descr()`, `freq()` or `dfSummary()`:
```r
> my.freq.table <- freq(iris$Species)
```

- Next, use `print()`, specifying the `method` argument which can take one of the following values:
  + `method='browser'` This creates an _html_ report on-the-fly and makes it fire up in your system's default browser. The path to the report is returned.
  + `method='viewer'` Same as "browser", except the report opens up in _RStudio_'s Viewer pane (as demonstrated at the top of this page.)
  + `method='pander'` This is the **default** value for `method` and will **not** produce an _html_ file. It will rather direct output to the console.

```r
> print(my.freq.table, method="browser")
```

- Since many of us like to stay in _RStudio_ as much as possible, a wrapper function called `view()` calls `print()` specifying `method="viewer"`. You can stick to `print()` altogether if you prefer.

### An alternative way to produce html (or text) reports

There is another way to generate output right at the first function call to `descr()`, `dfSummary()` or `freq()`; it is to supply the argument "file" to any of those. For instance, the two following function calls will generate a markdown report, and then an _html_ report from `dfSummary()`:

```r
> dfSummary(iris, style="grid", file="~/iris_dfSummary.md", escape.pipes=TRUE)
Output successfully written to file D:\Documents\iris_dfSummary.md
> dfSummary(iris, file="~/iris_dfSummary.html") # With html files, most of the other arguments are omitted.
Output successfully written to file D:\Documents\iris_dfSummary.html
```

**Note** The "escape.pipes=TRUE" argument makes it so that **Pandoc**, in converting to alternative formats, handles correctly multiline cells in `dfSummary()` reports.

## Customizing output

Some attributes attached to **summarytools** objects can be modified in order to change one of the elements displayed -- this is most usefull when generating _html_ reports. In particular, you may want to change "df.name", "var.name" or "date". To do so, you would use _R_'s `attr()` function in the following manner:

```r
> attr(my.freq.table, "df.name") <- "The IRIS Dataframe"
> my.freq.table

Frequencies

Dataframe: The IRIS Dataframe  
Variable: Species  

                   N   %Valid   %Cum.Valid   %Total   %Cum.Total
---------------- --- -------- ------------ -------- ------------
          setosa  50    33.33        33.33    33.33        33.33
      versicolor  50    33.33        66.67    33.33        66.67
       virginica  50    33.33          100    33.33          100
            <NA>   0       NA           NA        0          100
           Total 150      100          100      100          100

```

## Tables customization

When displaying **summarytools** objects in the console (as opposed to generating _html_ reports), many other arguments can be specified so you get the format that you want. The most common are:

- `style` one of "simple" (default), "grid", "rmarkdown" and "multiline"
- `justify` one of "left", "center", and "right"
- `round.digits` how many decimals to show. This argument is also used for _html_ reports
- `plain.ascii` when `TRUE`, no markdown tags are used
- `...` and any of the other [pander options](https://github.com/Rapporter/pander#pander-options)

## What else?

### Weighted statistics
Versions 0.5 and above support weights for `freq()` and `descr()`. 

### Function what.is() helps you figure out quickly what an object is by...

- Putting together the object's class(es), type (typeof), mode, storage mode, length, dim and object.size, all in a single table;
- Extending the `is()` function in a way that the object is tested against __all__ functions starting with `is.` -- see [this post on StackOverflow](http://stackoverflow.com/questions/8855589/a-comprehensive-survey-of-the-types-of-things-in-r-mode-and-class-and-type/26435993#26435993) for details;
- Giving a list of the object's attributes names and length (c.g. rownames, dimnames, labels, etc.)

#### Some examples

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

## Final notes

Visit my professionnal site to learn more about what I do and services I offer: [www.statconseil.com](http://www.statconseil.com)

The package comes with no guarantees. It is a work in progress and feedback / feature requests are most welcome. Just write me an email at dominic.comtois (at) gmail.com, or open an [Issue](https://github.com/dcomtois/summarytools/issues) if you find a bug.

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

Using the `view()` function, you can now generate html reports for objects created with `by()` 
Using `view()` or `print()`, append content to existing html files using `append = TRUE`. 
 
## Improvements to existing functions

In previous versions, **variable names** were problematic when `descr()` or `freq()` were called
 - via `by()` 
 - via `with()` 
 - via a combination of `by()` and `with()` 

Now all variable names should be displayed properly.

## Final notes

The package comes with no guarantees. It is a work in progress and feedback / feature requests are most welcome. Just write me an email at dominic.comtois (at) gmail.com, or open an [Issue](https://github.com/dcomtois/summarytools/issues) if you find a bug.



For a vignette which complements this introduction, see http://rpubs.com/dcomtois/summarytools_vignette
