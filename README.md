# summarytools

*summarytools* is an [R](http://r-project.org) package providing tools to _neatly and quickly summarize data_. Its main purpose is to provide hassle-free functions that every R programmer once wished it was included in base R:

- Painless **frequency tables** with proportions and missing data information
- Painless **descriptive statistics** with all common univariate statistics for numerical vectors
- Painless **dataframe summaries** that facilitate data cleaning and firsthand evaluation

It also aims at making R a little easier to use for newcomers. With just a few lines of code, one can get a pretty good picture of the data at hand.

### An example? 

Well, with just 2 lines of code, get a summary report of a dataframe, displayed directly in [_RStudio_](http://www.rstudio.com/)'s Viewer pane:

```r
> library(summarytools)
> view(dfSummary(iris))
```

`view(dfSummary(iris))`
![alt text](inst/includes/screenshots/dfSummary_in_RStudio_Viewer.png)

Building on the strengths of several packages and tools, the tables produced by _summarytools_ can be routed to either:

- The R console
- Plain text files, or markdown text files (credits to Gergely Daróczi's [pander](https://github.com/Rapporter/pander) package.)
- Stylized _html_  files that fire up in [_RStudio_](http://www.rstudio.com/)'s Viewer pane or in your system's default browser.

Also, all functions:
- Support `Hmisc` and `rapportools` _variable labels_
- Return table or dataframe objects for further manipulation if needed

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

`freq()` generates a table of frequencies with counts and percentages (including cumulative).

```r
> library(summarytools)
> data(iris)
> # We'll insert some NA's for illustration purposes
> is.na(iris) <- matrix(sample(x = c(TRUE,FALSE), size = 150*5, 
+                              replace = T, prob = c(.1,.9)),nrow = 150)
> # and add a variable label for the Species column
> rapportools::label(iris$Species) <- "The Species (duh)"
> freq(iris$Species)

 Variable name: Species
Dataframe name: iris
Variable label: The Species (duh)
          Date: 2014-11-30
          
Frequencies

                   N   %Valid   %Cum.Valid   %Total   %Cum.Total
---------------- --- -------- ------------ -------- ------------
          setosa  46    33.58        33.58    30.67        30.67
      versicolor  45    32.85        66.42       30        60.67
       virginica  46    33.58          100    30.67        91.33
            <NA>  13       NA           NA     8.67          100
           Total 150      100          100      100          100
```


## Descriptive (univariate) statistics with <u>descr()</u>
`descr()` generates common central tendency statistics and measures of dispersion for numerical data. It can handle single vectors as well as dataframes, in which case it will just ignore non-numerical data.

#### descr() on the iris dataframe

```r
> data(iris)
> descr(iris)

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


#### descr() has a "transpose" option

If your eyes/brain prefer seeing things the other way around, you just set `transpose` to `TRUE`:

```r
> descr(iris, transpose=TRUE)

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

`dfSummary()` generates a table containing variable information (class and type), common statistics for numerical data and frequency counts (as long as there are not too many distinct values -- and yes, you can specify the limit in the function call). Number and proportion of valid (non-missing) values is also added, and variable labels can optionnaly be included as well.


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

All functions can printout [markdown](http://daringfireball.net/projects/markdown/) text when setting option `style="rmarkdown"`. That is useful for instance here on GitHub, where `.md` files are actually displayed as _html_. Thanks to John MacFarlane's [Pandoc](http://johnmacfarlane.net/pandoc/), you can also convert markdown text files into a wide range of common formats such as _.pdf_, _.docx_, _.odt_, _.html_, among others. 

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

## Create and view html reports

Version 0.3 of _summarytools_ combines the strengths of the following packages and tools to generate basic html reports:

 - _RStudio_'s [htmltools](http://cran.r-project.org/web/packages/htmltools/index.html) package
 - The [xtable](http://cran.r-project.org/web/packages/xtable/index.html) package
 - [Bootstrap](http://getbootstrap.com/) cascading stylesheets

### How it works

You can achieve this in just one operation, but let's have a detailed walkthrough on how to generate and visualize an _html_ report with _summarytools_.

- First, generate a _summarytools_ object using one of `descr()`, `freq()` or `dfSummary()`:
```r
> my.freq.table <- freq(iris$Species)
```

- Next, use `print()`, specifying the `method` argument which can take one of the following values:
  + "browser": this creates an _html_ report on-the-fly and makes it fire up in your system's default browser. The path to the report is returned.
  + "viewer": same as "browser", except the report opens up in _RStudio_'s Viewer pane (as demonstrated at the top of this page.)
  + "pander": this is _not_ going to produce an _html_ file. It will direct output to the console and is the **default** value.

```r
> print(my.freq.table, method="browser")
```

- Since many of us like to stay in _RStudio_ as much as possible, a wrapper function called `view()` is included; it just calls `print()` specifying `method="viewer"`. It does also allow you to change the `method`, in which case it behaves exactly like `print()`. You can stick to `print()` altogether if you prefer.

## Customizing output

Some attributes attached to _summarytools_ objects can be modified in order to change one of the elements displayed -- this is most usefull when generating _html_ reports. In particular, you may want to change `df.name`, `var.name` or `date`. To do so, you would use _R_'s `attr()` function in the following manner:

```r
> attr(my.freq.table, "df.name") <- "The IRIS Dataframe"
> my.freq.table

Frequencies

 Variable name: Species  
Dataframe name: The IRIS Dataframe  
          Date: 2014-11-30 00:33:50 

                   N   %Valid   %Cum.Valid   %Total   %Cum.Total
---------------- --- -------- ------------ -------- ------------
          setosa  50    33.33        33.33    33.33        33.33
      versicolor  50    33.33        66.67    33.33        66.67
       virginica  50    33.33          100    33.33          100
            <NA>   0       NA           NA        0          100
           Total 150      100          100      100          100

```

## Tables customization

When displaying _summarytools_ objects in the console (as opposed to generating _html_ reports), many other arguments can be specified so you get the format that you want. The most common are:

- "style": one of "simple" (default), "grid", "rmarkdown" and "multiline"
- "justify": one of "left", "center", and "right"
- "round.digits": how many decimals to show. This argument is also used for _html_ reports
- "plain.ascii": whether to use markdown tags (always `FALSE` by default)
- ... and any of the other numerous [pander options](https://github.com/Rapporter/pander#pander-options)

### Example


to you can use a whole slew of also accept additionnal parameters which will be passed on to _pander_. See the [pander package](https://github.com/Rapporter/pander) documentation for details.

## What's coming next?

Under development is a function to make _summarytools_ tables appear in RStudio's Viewer, in an _html_ version with a minimal [bootstrap css](http://getbootstrap.com/) header. Of course, the function will also allow one to save the resulting html file.

## Final notes

The package comes with no guarantees. It is a work in progress and feedback / feature requests are most welcome.



## _summarytools_ builds on these great tools
 - John MacFarlane's [Pandoc](http://johnmacfarlane.net/pandoc/) and its _R_ implementation, Gergely Daróczi's [pander](https://github.com/Rapporter/pander) package.
 - The [markdown](http://daringfireball.net/projects/markdown/) markup language
 
