
# [summarytools: An *R* Package For Descriptive Statistics](https://github.com/dcomtois/summarytools)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/summarytools)](https://cran.r-project.org/package=summarytools)
[![](http://cranlogs.r-pkg.org/badges/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html)
[![](http://cranlogs.r-pkg.org/badges/grand-total/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html)
[![Rdoc](http://www.rdocumentation.org/badges/version/summarytools)](http://www.rdocumentation.org/packages/summarytools)

The following vignette complements this page: [Recommendations for Using
summarytools With
Rmarkdown](https://cran.r-project.org/web/packages/summarytools/vignettes/Recommendations-rmarkdown.html)

## A Big Step Forward in 0.9.0: Say ¡Hola\! to Translations\!

![A Russian version of freq)](img/Russian-freq.png)

### Contribute to summarytools…

… by submitting your own language file\! Go to [this
page](https://github.com/dcomtois/summarytools/tree/dev-current/translations),
copy the template and turn it into a new set of translations, and do a
pull request\! If you don’t have a GitHub account (and don’t want to
become one, even though it’s awesome), just send me an email (see bottom
of this page). Danke in advance\! :)

To download 0.9.0:  
**`devtools::install_github("dcomtois/summarytools", ref =
"dev-current")`**

To switch languages, simply use: **`st_options(lang = "fr")`**.

# What is summarytools?

*summarytools* is an [R](http://r-project.org) package providing tools
to *neatly and quickly summarize data*. It can also make *R* a little
easier to learn and use. Four functions are at the core of the package:

  - `freq()` : **frequency tables** with proportions, cumulative
    proportions and missing data information.
  - `ctable()` : **cross-tabulations** between two factors or any
    discrete data, with total, rows or columns proportions, as well as
    marginal totals.
  - `descr()` : **descriptive (univariate) statistics** for numerical
    vectors.
  - `dfSummary()` : Extensive **data frame summaries** that facilitate
    data cleaning and firsthand evaluation.

An emphasis has been put on both *what* and *how* results are presented,
so that the package can serve both as a data exploration *and* reporting
tool, which can be used either on its own for minimal reports, or along
with larger sets of tools such as RStudio’s for
[rmarkdown](http://rmarkdown.rstudio.com/), and
[knitr](https://yihui.name/knitr/).

**Building on the strengths of
[pander](https://github.com/Rapporter/pander) and
[htmltools](https://CRAN.R-project.org/package=htmltools)**, the outputs
produced by summarytools can be:

  - Displayed in plain text in the *R* console (default behaviour)
  - Used in *Rmarkdown* documents and *knitted* along with other text
    and *R* output
  - Written to *html* files that fire up in
    [*RStudio*](http://www.rstudio.com/)’s Viewer pane or in your
    system’s default browser
  - Written to plain text files / *Rmarkdown* text files

Some people have successfully included some of the package’s functions
in *shiny apps*, too\!

### Latest Improvements and changes

Version 0.8.9 brings several improvements to *summarytools*, notably:

  - Easier management of global settings (customizable defaults) with
    `st_options()`; each option has its own parameter, so you can set as
    many as you need in just one function call. The legacy way of
    setting options is still supported
  - `dfSummary()` went through somw changes:
      - number of culumns is shown in heading, as well as number of
        duplicated rows (credits to [Paul
        Feitsma](https://github.com/paulfeitsma) for the good idea)
      - UPC and EAN codes are now detected as well as sequences of
        integers (thanks to Paul one more)
      - barplots more accurately reflect counts as they are no more
        “stretched” to the width of the table “cells”; this allows
        comparing counts across variables
      - Binary and unary columns no more show irrelevant statistics (IQR
        / CV)
  - `descr()` allows two new values for the `stats` parameter: “fivenum”
    and “common”, the latter being a shortcut for `c("mean", "sd",
    "min", "med",` `"max", "n.valid", "pct.valid")`
  - Many More options can be overridden when printing / viewing
    summarytools objects; refer to `print.summarytools()`’s
    documentation for details

**Other notable changes:**

  - The `omit.headings` parameter has been replaced by the more
    straightforward `headings`. `omit.heandings` is still supported in
    this version but will be deprecated in the future
  - Finally, because it was subject to errors, the *Rows Subset* heading
    element has been removed. In case of a massive public outry, I’ll
    bring it back ;)
  - Under the hood, much has been going; the lengthier functions have
    been split into more manageable parts, facilitating maintenance and
    improving readability

# How to install

To benefit from all the latest fixes, install it from GitHub:

``` r
install.packages("devtools")
library(devtools)
install_github('dcomtois/summarytools')
```

To install the most recent version on the *R-CRAN* repository:

``` r
install.packages("summarytools")
```

For enthusiastic users willing to contribute to *summarytools*’
development, I encourage you to go for the **development** version,
which is the most up-to-date, but also a *work-in-progress*. Bugs may
show up, but if you report them I can generally fix them quickly.

``` r
install.packages("devtools")
library(devtools)
install_github('dcomtois/summarytools', ref='dev-current')
```

You can see the source code and documentation on the official *R* site
[here](http://cran.r-project.org/web/packages/summarytools/).

# Four Core Functions

## 1 - freq() : Frequency Tables

The `freq()` function generates a table of frequencies with counts and
proportions. Since this page use *markdown* rendering, we’ll set `style
= 'rmarkdown'` to take advantage of it.

``` r
library(summarytools)
freq(iris$Species, style = "rmarkdown")
```

### Frequencies

**iris$Species**  
**Type:**
Factor

|                | Freq | % Valid | % Valid Cum. | % Total | % Total Cum. |
| -------------: | ---: | ------: | -----------: | ------: | -----------: |
|     **setosa** |   50 |   33.33 |        33.33 |   33.33 |        33.33 |
| **versicolor** |   50 |   33.33 |        66.67 |   33.33 |        66.67 |
|  **virginica** |   50 |   33.33 |       100.00 |   33.33 |       100.00 |
|     **\<NA\>** |    0 |         |              |    0.00 |       100.00 |
|      **Total** |  150 |  100.00 |       100.00 |  100.00 |       100.00 |

If we do not worry about missing data, we can set `report.nas =
FALSE`:

``` r
freq(iris$Species, report.nas = FALSE, style = "rmarkdown", headings = FALSE)
```

|                | Freq |      % | % Cum. |
| -------------: | ---: | -----: | -----: |
|     **setosa** |   50 |  33.33 |  33.33 |
| **versicolor** |   50 |  33.33 |  66.67 |
|  **virginica** |   50 |  33.33 | 100.00 |
|      **Total** |  150 | 100.00 | 100.00 |

We could furthermore omit the *Totals* row by setting `totals = FALSE`.

## 2 - ctable() : Cross-Tabulations

We’ll now use a sample data frame called *tobacco*, which is included in
the package. We want to cross-tabulate the two categorical variables
`smoker` and `diseased`. By default, `ctable()` gives row proportions,
but we’ll include the full syntax anyway.

Since *markdown* has not support (yet) for multi-line headings, we’ll
show an image of the resulting html table.

``` r
with(tobacco, view(ctable(smoker, diseased)))
```

![Example of ctable() output](img/ctable-with-row-props.png)

Notice that instead of `ctable(tobacco$smoker, tobacco$diseased, ...)`,
we used the `with()` function, making the syntax less redundant.

It is possible to display *column*, *total*, or no proportions at all.
We can also omit the marginal totals to have a simple *2 x 2* table.

``` r
with(tobacco, 
     print(ctable(smoker, diseased, prop = 'n', totals = FALSE),
           headings = FALSE, method = 'render'))
```

<!--html_preserve-->

<div class="container st-container">

<table class="table table-bordered st-table st-table-bordered st-cross-table ">

<thead>

<tr>

<th>

</th>

<th colspan="2">

diseased

</th>

</tr>

<tr>

<td align="center">

<strong>smoker</strong>

</td>

<th align="center">

Yes

</th>

<th align="center">

No

</th>

</tr>

</thead>

<tbody>

<tr>

<td align="center">

<strong>Yes</strong>

</td>

<td>

<span>125</span>

</td>

<td>

<span>173</span>

</td>

</tr>

<tr>

<td align="center">

<strong>No</strong>

</td>

<td>

<span>99</span>

</td>

<td>

<span>603</span>

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

## 3 - descr() : Descriptive Univariate Stats

The `descr()` function generates common central tendency statistics and
measures of dispersion for numerical data. It can handle single vectors
as well as data frames, in which case it just ignores non-numerical
columns (and displays a message to that effect).

``` r
descr(iris, style = "rmarkdown")
```

    ## Non-numerical variable(s) ignored: Species

### Descriptive Statistics

**iris**  
**N:**
150

|                 | Petal.Length | Petal.Width | Sepal.Length | Sepal.Width |
| --------------: | -----------: | ----------: | -----------: | ----------: |
|        **Mean** |         3.76 |        1.20 |         5.84 |        3.06 |
|     **Std.Dev** |         1.77 |        0.76 |         0.83 |        0.44 |
|         **Min** |         1.00 |        0.10 |         4.30 |        2.00 |
|          **Q1** |         1.60 |        0.30 |         5.10 |        2.80 |
|      **Median** |         4.35 |        1.30 |         5.80 |        3.00 |
|          **Q3** |         5.10 |        1.80 |         6.40 |        3.30 |
|         **Max** |         6.90 |        2.50 |         7.90 |        4.40 |
|         **MAD** |         1.85 |        1.04 |         1.04 |        0.44 |
|         **IQR** |         3.50 |        1.50 |         1.30 |        0.50 |
|          **CV** |         0.47 |        0.64 |         0.14 |        0.14 |
|    **Skewness** |       \-0.27 |      \-0.10 |         0.31 |        0.31 |
| **SE.Skewness** |         0.20 |        0.20 |         0.20 |        0.20 |
|    **Kurtosis** |       \-1.42 |      \-1.36 |       \-0.61 |        0.14 |
|     **N.Valid** |       150.00 |      150.00 |       150.00 |      150.00 |
|     **% Valid** |       100.00 |      100.00 |       100.00 |      100.00 |

### Transposing and selecting only the stats you need

If your eyes/brain prefer seeing things the other way around, just use
`transpose = TRUE`. Here, we also select only the statistics we wish to
see, and specify `headings = FALSE` to avoid reprinting the same
information as above.

You can specify the stats you wish to report with the `stats` argument,
which also accepts special values `all`, `fivenum`, and
`common`.

``` r
descr(iris, stats = c("mean", "sd", "min", "med", "max"), transpose = TRUE, 
      headings = FALSE, style = "rmarkdown")
```

    ## Non-numerical variable(s) ignored: Species

|                  | Mean | Std.Dev |  Min | Median |  Max |
| ---------------: | ---: | ------: | ---: | -----: | ---: |
| **Petal.Length** | 3.76 |    1.77 | 1.00 |   4.35 | 6.90 |
|  **Petal.Width** | 1.20 |    0.76 | 0.10 |   1.30 | 2.50 |
| **Sepal.Length** | 5.84 |    0.83 | 4.30 |   5.80 | 7.90 |
|  **Sepal.Width** | 3.06 |    0.44 | 2.00 |   3.00 | 4.40 |

## 4 - dfSummary() : Data Frame Summaries

`dfSummary()` collects information about all variables in a data frame
and displays it in a singe, legible table.

#### Examples

With the following tiny bit of code, we’ll generate a summary report for
the *iris* data frame and have it displayed in
[*RStudio*](http://www.rstudio.com/)’s Viewer pane:

``` r
# Load the package
library(summarytools)

# Generate the summary
view(dfSummary(iris))
```

![Example of dfSummary Output displayed in RStudio’s
viewer](img/dfSummary_in_RStudio_Viewer.png)

It is also possible to use `dfSummary()` in *Rmarkdown* documents. In
this next example, note that due to rmarkdown compatibility issues,
histograms are not shown. We’re working on this. Further down, we’ll see
how to use *html* rendering to go around this problem.

``` r
dfSummary(tobacco, plain.ascii = FALSE, style = "grid")
```

### Data Frame Summary

**tobacco**  
**Dimensions:** 1000 x 9  
**Duplicates:** 2

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 14%" />
<col style="width: 28%" />
<col style="width: 19%" />
<col style="width: 15%" />
<col style="width: 8%" />
<col style="width: 8%" />
</colgroup>
<thead>
<tr class="header">
<th>No</th>
<th>Variable</th>
<th>Stats / Values</th>
<th>Freqs (% of Valid)</th>
<th>Text Graph</th>
<th>Valid</th>
<th>Missing</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1</td>
<td>gender<br />
[factor]</td>
<td>1. F<br />
2. M</td>
<td>489 (50.0%)<br />
489 (50.0%)</td>
<td>IIIIIIIIII<br />
IIIIIIIIII</td>
<td>978<br />
(97.8%)</td>
<td>22<br />
(2.2%)</td>
</tr>
<tr class="even">
<td>2</td>
<td>age<br />
[numeric]</td>
<td>Mean (Std.Dev) :49.6 (18.29)<br />
min &lt; med &lt; max:<br />
18 &lt; 50 &lt; 80<br />
IQR (CV) :32 (0.37)</td>
<td>63 distinct values</td>
<td></td>
<td>975<br />
(97.5%)</td>
<td>25<br />
(2.5%)</td>
</tr>
<tr class="odd">
<td>3</td>
<td>age.gr<br />
[factor]</td>
<td>1. 18-34<br />
2. 35-50<br />
3. 51-70<br />
4. 71 +</td>
<td>258 (26.5%)<br />
241 (24.7%)<br />
317 (32.5%)<br />
159 (16.3%)</td>
<td>IIIII<br />
IIII<br />
IIIIII<br />
III</td>
<td>975<br />
(97.5%)</td>
<td>25<br />
(2.5%)</td>
</tr>
<tr class="even">
<td>4</td>
<td>BMI<br />
[numeric]</td>
<td>Mean (Std.Dev) :25.73 (4.49)<br />
min &lt; med &lt; max:<br />
8.83 &lt; 25.62 &lt; 39.44<br />
IQR (CV) :5.72 (0.17)</td>
<td>974 distinct values</td>
<td></td>
<td>974<br />
(97.4%)</td>
<td>26<br />
(2.6%)</td>
</tr>
<tr class="odd">
<td>5</td>
<td>smoker<br />
[factor]</td>
<td>1. Yes<br />
2. No</td>
<td>298 (29.8%)<br />
702 (70.2%)</td>
<td>IIIII<br />
IIIIIIIIIIIIII</td>
<td>1000<br />
(100%)</td>
<td>0<br />
(0%)</td>
</tr>
<tr class="even">
<td>6</td>
<td>cigs.per.day<br />
[numeric]</td>
<td>Mean (Std.Dev) :6.78 (11.88)<br />
min &lt; med &lt; max:<br />
0 &lt; 0 &lt; 40<br />
IQR (CV) :11 (1.75)</td>
<td>37 distinct values</td>
<td></td>
<td>965<br />
(96.5%)</td>
<td>35<br />
(3.5%)</td>
</tr>
<tr class="odd">
<td>7</td>
<td>diseased<br />
[factor]</td>
<td>1. Yes<br />
2. No</td>
<td>224 (22.4%)<br />
776 (77.6%)</td>
<td>IIII<br />
IIIIIIIIIIIIIII</td>
<td>1000<br />
(100%)</td>
<td>0<br />
(0%)</td>
</tr>
<tr class="even">
<td>8</td>
<td>disease<br />
[character]</td>
<td>1. Hypertension<br />
2. Cancer<br />
3. Cholesterol<br />
4. Heart<br />
5. Pulmonary<br />
6. Musculoskeletal<br />
7. Diabetes<br />
8. Hearing<br />
9. Digestive<br />
10. Hypotension<br />
[ 3 others ]</td>
<td>36 (16.2%)<br />
34 (15.3%)<br />
21 ( 9.5%)<br />
20 ( 9.0%)<br />
20 ( 9.0%)<br />
19 ( 8.6%)<br />
14 ( 6.3%)<br />
14 ( 6.3%)<br />
12 ( 5.4%)<br />
11 ( 5.0%)<br />
21 ( 9.5%)</td>
<td>III<br />
III<br />
I<br />
I<br />
I<br />
I<br />
I<br />
I<br />
I<br />
<br />
I</td>
<td>222<br />
(22.2%)</td>
<td>778<br />
(77.8%)</td>
</tr>
<tr class="odd">
<td>9</td>
<td>samp.wgts<br />
[numeric]</td>
<td>Mean (Std.Dev) :1 (0.08)<br />
min &lt; med &lt; max:<br />
0.86 &lt; 1.04 &lt; 1.06<br />
IQR (CV) :0.19 (0.08)</td>
<td>0.86!: 267 (26.7%)<br />
1.04!: 249 (24.9%)<br />
1.05!: 324 (32.4%)<br />
1.06!: 160 (16.0%)<br />
! rounded</td>
<td>IIIII<br />
IIII<br />
IIIIII<br />
III<br />
<br />
</td>
<td>1000<br />
(100%)</td>
<td>0<br />
(0%)</td>
</tr>
</tbody>
</table>

## The print() and view() Functions

*summarytools* has a generic `print` method, `print.summarytools()`. By
default, its `method` argument is set to `'pander'`. One of the ways in
which `view()` is useful is that we can use it to easily display *html*
outputs in *RStudio*’s Viewer. In this case, the `view()` function
simply acts as a wrapper around the generic `print()` function,
specifying `method = 'viewer'`. When used outside *RStudio*, the
`method` falls back on `'browser'` and the generated file is opened in
the system’s default browser.

## Using by() to Show Results By Groups

With `freq()` and `descr()`, you can use *R*’s base function `by()` to
show statistics split by a ventilation / categorical variable. *R*’s
`by()` function returns a `list` containing as many *summarytools*
objects as there are categories in our ventilation variable.

To properly display the content present in that list, **we use the
`view()` function**. Using `print()`, while technically possible, will
not give as much satisfactory results.

#### Example

Using the *iris* data frame, we will display descriptive statistics
broken down by Species.

``` r
# First save the results
iris_stats_by_species <- by(data = iris, 
                            INDICES = iris$Species, 
                            FUN = descr, stats = c("mean", "sd", "min", "med", "max"), 
                            transpose = TRUE)
# Then use view(), like so:
view(iris_stats_by_species, method = "pander", style = "rmarkdown")
```

### Descriptive Statistics

**iris**  
**Group:** Species = setosa  
**N:** 50

|                  | Mean | Std.Dev |  Min | Median |  Max |
| ---------------: | ---: | ------: | ---: | -----: | ---: |
| **Petal.Length** | 1.46 |    0.17 | 1.00 |   1.50 | 1.90 |
|  **Petal.Width** | 0.25 |    0.11 | 0.10 |   0.20 | 0.60 |
| **Sepal.Length** | 5.01 |    0.35 | 4.30 |   5.00 | 5.80 |
|  **Sepal.Width** | 3.43 |    0.38 | 2.30 |   3.40 | 4.40 |

**Group:** Species = versicolor  
**N:** 50

|                  | Mean | Std.Dev |  Min | Median |  Max |
| ---------------: | ---: | ------: | ---: | -----: | ---: |
| **Petal.Length** | 4.26 |    0.47 | 3.00 |   4.35 | 5.10 |
|  **Petal.Width** | 1.33 |    0.20 | 1.00 |   1.30 | 1.80 |
| **Sepal.Length** | 5.94 |    0.52 | 4.90 |   5.90 | 7.00 |
|  **Sepal.Width** | 2.77 |    0.31 | 2.00 |   2.80 | 3.40 |

**Group:** Species = virginica  
**N:** 50

|                  | Mean | Std.Dev |  Min | Median |  Max |
| ---------------: | ---: | ------: | ---: | -----: | ---: |
| **Petal.Length** | 5.55 |    0.55 | 4.50 |   5.55 | 6.90 |
|  **Petal.Width** | 2.03 |    0.27 | 1.40 |   2.00 | 2.50 |
| **Sepal.Length** | 6.59 |    0.64 | 4.90 |   6.50 | 7.90 |
|  **Sepal.Width** | 2.97 |    0.32 | 2.20 |   3.00 | 3.80 |

To see an *html* version of these results, we’d simply do this (results
not shown):

``` r
view(iris_stats_by_species)
```

### Special Case - Using descr() With by() For A Single Variable

Instead of showing several tables having only one column each, the
`view()` function will assemble the results into a single table:

``` r
BMI_by_age <- with(tobacco, 
                   by(BMI, age.gr, descr, 
                      stats = c("mean", "sd", "min", "med", "max")))
view(BMI_by_age, "pander", style = "rmarkdown")
```

### Descriptive Statistics

**tobacco$BMI by age.gr**

|             | 18-34 | 35-50 | 51-70 |  71 + |
| ----------: | ----: | ----: | ----: | ----: |
|    **Mean** | 23.84 | 25.11 | 26.91 | 27.45 |
| **Std.Dev** |  4.23 |  4.34 |  4.26 |  4.37 |
|     **Min** |  8.83 | 10.35 |  9.01 | 16.36 |
|  **Median** | 24.04 | 25.11 | 26.77 | 27.52 |
|     **Max** | 34.84 | 39.44 | 39.21 | 38.37 |

The transposed version looks like this:

``` r
BMI_by_age <- with(tobacco, 
                   by(BMI, age.gr, descr,  transpose = TRUE,
                      stats = c("mean", "sd", "min", "med", "max")))
view(BMI_by_age, "pander", style = "rmarkdown", headings = FALSE)
```

|           |  Mean | Std.Dev |   Min | Median |   Max |
| --------: | ----: | ------: | ----: | -----: | ----: |
| **18-34** | 23.84 |    4.23 |  8.83 |  24.04 | 34.84 |
| **35-50** | 25.11 |    4.34 | 10.35 |  25.11 | 39.44 |
| **51-70** | 26.91 |    4.26 |  9.01 |  26.77 | 39.21 |
|  **71 +** | 27.45 |    4.37 | 16.36 |  27.52 | 38.37 |

## Using lapply() to Show Several freq() tables at once

As is the case for `by()`, the `view()` function is essential for making
results nice and tidy.

``` r
tobacco_subset <- tobacco[ ,c("gender", "age.gr", "smoker")]
freq_tables <- lapply(tobacco_subset, freq)
view(freq_tables, footnote = NA, file = 'freq-tables.html')
```

## Using summarytools in Rmarkdown documents

As we have seen, *summarytools* can generate both text (including
rmarkdown) and html results. Both can be used in Rmarkdown, according to
your preferences. The vignette mentioned at the top of this page is
dedicated to showing examples, but if you’re in a hurry, here are a few
tips to get started:

  - Always set the `knitr` chunk option `results = 'asis'`. You can do
    this on a chunk-by-chunk basis, but here is how to do it globally:

<!-- end list -->

``` r
    knitr::opts_chunk$set(echo = TRUE, results = 'asis')
```

        Refer to [this page](https://yihui.name/knitr/options/) for more
on *knitr*’s options.

  - To get better results when using html (with `method = 'render'`),
    set up your .Rmd document so it includes *summarytool*’s css.

#### Example

    # ---
    # title: "RMarkdown using summarytools"
    # output: 
    #   html_document: 
    #     css: C:/R/win-library/3.4/summarytools/includes/stylesheets/summarytools.css
    # ---
    
    # ```{r, results='asis'}
    # library(summarytools)  
    # 
    # print(dfSummary(tobacco, style = 'grid', plain.ascii = FALSE, graph.magnif = 0.85), 
    #       method = 'render', headings = FALSE)
    # ```

![Example of rendered output](img/dfSummary-render-in-markdown.png)

## Writing Output to Files

The console will always tell you the location of the temporary *html*
file that is created in the process. However, you can specify the name
and location of that file explicitly if you need to reuse it later on:

``` r
view(iris_stats_by_species, file = "~/iris_stats_by_species.html")
```

Based on the file extension you provide (\_.html\_ vs others),
*summarytools* will use the appropriate method; there is no need to
specify the `method` argument.

### Appending output files

There is also an `append =` logical argument for adding content to
existing reports, both text/Rmarkdown and html. This is useful if you
want to quickly include several statistical tables in a single file. It
is fast alternative to creating an *.Rmd* document if you don’t need the
extra content that the latter allows.

## Global options

The following options are customizable with `st_options()`:

**General
options**

|        Option name |   Default | Note                                         |
| -----------------: | --------: | :------------------------------------------- |
|              style |  “simple” | Set to “rmarkdown” when necessary            |
|        plain.ascii |      TRUE | Set to FALSE when doing rmarkdown            |
|       round.digits |         2 | Number of decimals to show                   |
|      headings (\*) |      TRUE | Formerly ‘omit.headings’                     |
|           footnote | “default” | Personalize, or set to NA to omit            |
|     display.labels |      TRUE | Show variable / data frame labels in heading |
| bootstrap.css (\*) |      TRUE | Include Bootstrap 4 css in html              |
|         custom.css |        NA | Path to your own css file                    |
|        escape.pipe |     FALSE | Useful with some Pandoc conversions          |

(\*) In rmarkdown or in Shiny apps, better set to FALSE

**Function-specific
options**

|            Option name | Default | Note                                    |
| ---------------------: | ------: | :-------------------------------------- |
|            freq.totals |    TRUE | Display totals row in freq()            |
|        freq.report.nas |    TRUE | Display <NA> row and “valid” columns    |
|            ctable.prop |     “r” | Display **r**ow proportions by default  |
|          ctable.totals |    TRUE | Show marginal totals                    |
|            descr.stats |   “all” | “fivenum”, “common” or list of stats    |
|        descr.transpose |   FALSE |                                         |
|   dfSummary.varnumbers |    TRUE | Show variable numbers in 1st col.       |
|   dfSummary.labels.col |    TRUE | Show variable labels when present       |
|    dfSummary.graph.col |    TRUE |                                         |
|    dfSummary.valid.col |    TRUE |                                         |
|       dfSummary.na.col |    TRUE |                                         |
| dfSummary.graph.magnif |       1 | Zoom factor for barplots and histograms |

#### Examples

``` r
st_options()                      # display all global options values
st_options('round.digits')        # display the value of a specific option
st_options(style = 'rmarkdown')   # change one or several options' values
st_options(footnote = NA)         # Turn off the footnote on all outputs.
                                  # This option was used prior to generating
                                  # the present document.
```

## Overriding formatting attributes

When a *summarytools* object is stored, its formatting attributes are
stored within it. However, you can override most of them when using the
`print()` and `view()`
functions.

#### Example

``` r
age_stats <- freq(tobacco$age.gr)  # age_stats contains a regular output for freq 
                                   # including headings, NA counts, and Totals
print(age_stats, style = "rmarkdown", report.nas = FALSE, 
                 totals = FALSE, headings = FALSE)
```

|           | Freq |     % | % Cum. |
| --------: | ---: | ----: | -----: |
| **18-34** |  258 | 26.46 |  26.46 |
| **35-50** |  241 | 24.72 |  51.18 |
| **51-70** |  317 | 32.51 |  83.69 |
|  **71 +** |  159 | 16.31 | 100.00 |

Note that the overridden attributes are stil part of the *age\_stats*
object.

## Order of Priority for options

1.  Options overridden explicitly with `print()` or `view()` have
    precendence
2.  options specified as explicit arguments to `freq() / ctable() /
    descr() / dfSummary()` come second
3.  Global options, which can be set with `st_options`, come third

## Customizing looks with CSS

Version 0.8 of *summarytools* uses *RStudio*’s [htmltools
package](https://CRAN.R-project.org/package=htmltools) and version 4 of
[Bootstrap](https://getbootstrap.com/)’s cascading stylesheets.

It is possible to include your own *css* if you wish to customize the
look of the output tables. See the documentation for the package’s
`print.summarytools()` function for details, but here is a quick example
to give you the gist of it.

#### Example

Say you need to make the font size really, really small. For this, you  
create a CSS file - let’s call it “custom.css” - containing the
following class:

``` css
.table-condensed {
  font-size: 8px;
}
```

Then, to apply it to a *summarytools* object and display it in your
browser:

``` r
view(dfSummary(tobacco), custom.css = 'path/to/custom.css', 
     table.classes = 'table-condensed')
```

To display a smaller table that is not **that** smaller, you can use the
provided css class `st-small`.

## Working with *shiny* apps

To include *summarytools* functions into *shiny* apps, it is recommended
that you:

  - set `bootstrap.css` to `FALSE` to avoid interacting with the app’s
    layout  
  - adjust the size of the graphs in `dfSummary()`  
  - omit headings

#### Example:

``` r
print(dfSummary(somedata, graph.magnif = 0.8), 
      method = 'render',
      headings = FALSE,
      bootstrap.css = FALSE)
```

# Final notes

The package comes with no guarantees. It is a work in progress and
feedback / feature requests are welcome. Just send me an email
(dominic.comtois (at) gmail.com), or open an
[Issue](https://github.com/dcomtois/summarytools/issues) if you find a
bug.

Also, the package grew significantly larger, and maintaining it all by
myself is time consuming. If you would like to contribute, please get in
touch, I’d greatly appreciate the help.
