# summarytools

*summarytools* is an [R](http://r-project.org) package providing tools to _neatly and quickly summarize data_. Its main purpose is to provide hassle-free functions that every R programmer once wished it was included in base R:

- Painless **frequency tables** with proportions and missing data information
- Painless **descriptive statistics** with all common univariate statistics for numerical vectors
- Painless **dataframe summaries** that facilitate data cleaning and firsthand evaluation

It also aims at making R _a little_ easier to use for newcomers.

All three functions:

- Display neatly formatted tables in plain text or [markdown](http://daringfireball.net/projects/markdown/) text thanks to Gergely Daróczi's [pander](https://github.com/Rapporter/pander) package
- Optionnaly redirect their output to text files rather than to console
- Support `Hmisc` and `rapportools` variable labels
- Return table (matrix) objects for further manipulation if needed


# Installation

To install the **latest stable version** of `summarytools`, just type into your R console: 

```r
> install.packages("summarytools")
```

For the most **up-to-date version** that has all the latest features **but** might also contain bugs, I invite you to first install the `devtools` package and then install it through `install_github`:

```r
> install.packages("devtools")
> library(devtools)
> install_github('dcomtois/summarytools')
```

You can also see the source code and documentation on the official R site [here](http://cran.r-project.org/web/packages/summarytools/).

## Frequency tables with <u>freq()</u>

`freq()` will generate a table of frequencies with counts and percentages (raw and cumulative).

```r
> library(summarytools)
> data(iris)
> # We'll insert some NA values for illustration purposes
> is.na(iris) <- matrix(sample(x = c(TRUE,FALSE), size = 150*5, 
+                              replace = T, prob = c(.1,.9)),nrow = 150)
> # ... and a variable label for the Species column
> rapportools::label(iris$Species) <- "The Species (duh)"
> freq(iris$Species)
```

```r
Variable name:  iris$Species
Variable label: The Species (duh)

Frequencies

                   N   %Valid   %Cum.Valid   %Total   %Cum.Total
---------------- --- -------- ------------ -------- ------------
          setosa  46    33.58        33.58    30.67        30.67
      versicolor  45    32.85        66.42       30        60.67
       virginica  46    33.58          100    30.67        91.33
            <NA>  13       NA           NA     8.67          100
           Total 150      100          100      100          100
```


## Descriptive (univariate) statistics with <u>desc()</u>
`desc()` generates common central tendency statistics and measures of dispersion for numerical data. It can handle single vectors as well as dataframes, in which case it will just ignore non-numerical data.

#### desc() on the iris dataframe

```r
> data(iris)
> desc(iris)
```

```r
Descriptive Statistics

                    Sepal.Length   Sepal.Width   Petal.Length   Petal.Width
----------------- -------------- ------------- -------------- -------------
             Mean           5.88          3.05           3.75           1.2
          Std.Dev           0.84          0.44           1.76          0.77
              Min            4.3             2              1           0.1
              Max            7.9           4.4            6.9           2.5
           Median            5.8             3            4.4           1.3
              MAD           1.04          0.37           1.78          1.04
              IQR           1.38           0.5            3.5           1.5
               CV           6.98          6.99           2.13          1.56
         Skewness           0.26          0.31           -0.3          -0.1
      SE.Skewness           0.21          0.21           0.21          0.21
         Kurtosis          -0.68          0.17          -1.45         -1.37


Observations

              Sepal.Length   Sepal.Width   Petal.Length   Petal.Width
----------- -------------- ------------- -------------- -------------
      Valid   134 (89.33%)     138 (92%)   134 (89.33%)  134 (89.33%)
       <NA>    16 (10.67%)       12 (8%)    16 (10.67%)   16 (10.67%)
      Total            150           150            150           150
```


#### desc() has a "transpose" option

If your eyes (or brain?) prefer seeing things the other way around, you just set `transpose` to `TRUE`:

```r
> desc(iris, transpose=TRUE)
```

```r
Descriptive Statistics

                     Mean   Std.Dev   Min   Max   Median   MAD   IQR   CV   Skewness   SE.Skewness   Kurtosis
------------------ ------ --------- ----- ----- -------- ----- ----- ---- ---------- ------------- ----------
      Sepal.Length   5.88      0.84   4.3   7.9      5.8  1.04  1.38 6.98       0.26          0.21      -0.68
       Sepal.Width   3.05      0.44     2   4.4        3  0.37   0.5 6.99       0.31          0.21       0.17
      Petal.Length   3.75      1.76     1   6.9      4.4  1.78   3.5 2.13       -0.3          0.21      -1.45
       Petal.Width    1.2      0.77   0.1   2.5      1.3  1.04   1.5 1.56       -0.1          0.21      -1.37


Observations

                          Valid        <NA>   Total
------------------ ------------ ----------- -------
      Sepal.Length 134 (89.33%) 16 (10.67%)     150
       Sepal.Width    138 (92%)     12 (8%)     150
      Petal.Length 134 (89.33%) 16 (10.67%)     150
       Petal.Width 134 (89.33%) 16 (10.67%)     150
```


## Dataframe summaries

`dfSummary()` generates a table containing variable information (class and type), labels if any, common statistics for numerical data and frequency counts (as long as there are not too many distinct values -- and yes, you can specify the limit in the function call). Number and proportion of valid (non-missing) values is also added.


```r
> dfSummary(iris)
```

```r
----------------------------------------------------------------------------------------------------
num   variable.name   properties    factor.levels.or.stats            frequencies        n.valid    
----- --------------- ------------- --------------------------------- ------------------ -----------
1     Sepal.Length    type:double   mean (sd) = 5.88 (0.84)           35 distinct values 134 (89.3%)
                      class:numeric min < med < max = 4.3 < 5.8 < 7.9                               
                                    IQR (CV) = 1.38 (0.14)                                          

2     Sepal.Width     type:double   mean (sd) = 3.05 (0.44)           23 distinct values 138 (92.0%)
                      class:numeric min < med < max = 2 < 3 < 4.4                                   
                                    IQR (CV) = 0.5 (0.14)                                           

3     Petal.Length    type:double   mean (sd) = 3.75 (1.76)           43 distinct values 134 (89.3%)
                      class:numeric min < med < max = 1 < 4.4 < 6.9                                 
                                    IQR (CV) = 3.5 (0.47)                                           

4     Petal.Width     type:double   mean (sd) = 1.2 (0.77)            22 distinct values 134 (89.3%)
                      class:numeric min < med < max = 0.1 < 1.3 < 2.5                               
                                    IQR (CV) = 1.5 (0.64)                                           

5     Species         type:integer  1. setosa                         1: 46 (33.6%)      137 (91.3%)
                      class:factor  2. versicolor                     2: 45 (32.8%)                 
                                    3. virginica                      3: 46 (33.6%)                 
----------------------------------------------------------------------------------------------------
```


# All functions markdown-compatible

All functions will printout [markdown](http://daringfireball.net/projects/markdown/) text when setting option `style="rmarkdown"`. That is useful for instance here on GitHub, where `.md` files are actually displayed as _html_. Thanks to John MacFarlane's [Pandoc](http://johnmacfarlane.net/pandoc/), you can also convert markdown text files into a wide range of common formats such as _.pdf_, _.docx_, _.odt_, _.html_, among others. 

Here is an **example** of a markdown table, as processed by GitHub, using `freq()`:


```r
> freq(iris$Species, style="rmarkdown", plain.ascii=FALSE, missing="---")
```

Variable name:  iris$Species  
Variable label: The Species (duh)

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

#### Multiline-cell tables

Multiline-cell tables such as `dfSummary`'s are a bit trickier to convert to other formats; this part is still under development. And maybe more interesting is the soon-to-come `view()` function which will directly generate _html_ files using the [htmltools](http://cran.r-project.org/web/packages/htmltools/index.html) package, supporting custom _css_ (see _What's coming next_ section below).

#### More customization

All functions also accept additionnal parameters which will be passed on to _pander_. See the [pander package](https://github.com/Rapporter/pander) documentation for details.

## What's coming next?

Under development is a function to make _summarytools_ tables appear in RStudio's Viewer, in an _html_ version with a minimal [bootstrap css](http://getbootstrap.com/) header. Of course, the function will also allow one to save the resulting html file.

## Final notes

The package comes with no guarantees. It is a work in progress and feedback / feature requests are most welcome.
