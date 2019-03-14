
# summarytools <a href='https://github.com/dcomtois/summarytools'><img src='img/logo.png' align="right" height="145" /></a>

<!-- badges -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/summarytools)](https://cran.r-project.org/package=summarytools)
[![](http://cranlogs.r-pkg.org/badges/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html)
[![](http://cranlogs.r-pkg.org/badges/grand-total/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html)
[![Rdoc](http://www.rdocumentation.org/badges/version/summarytools)](http://www.rdocumentation.org/packages/summarytools)

The following vignettes complement this page:

[Recommendations for Using summarytools With
Rmarkdown](https://cran.r-project.org/web/packages/summarytools/vignettes/Recommendations-rmarkdown.html)  
[Introduction to
summarytools](https://cran.r-project.org/web/packages/summarytools/vignettes/Introduction.html)
– Contents similar to this page (minus installation instructions), with
fancier table stylings.

# What is summarytools?

**summarytools** is an [R](https://r-project.org) package providing
tools to *neatly and quickly summarize data*. It can also make *R* a
little easier to learn and to use, especially for data cleaning and
preliminary analysis. Four functions are at the core of the package:

  - `freq()` : **frequency tables** with proportions, cumulative
    proportions and missing data information
  - `ctable()` : **cross-tabulations** between two factors or any
    discrete data, with total, rows or columns proportions, as well as
    marginal totals
  - `descr()` : **descriptive (univariate) statistics** for numerical
    data
  - `dfSummary()` : Extensive **data frame summaries** that facilitate
    data cleaning and firsthand evaluation

An emphasis has been put on both *what* and *how* results are presented,
so that the package can serve both as an exploration *and* reporting
tool, used on its own for minimal reports, or with other sets of tools
such as [rmarkdown](http://rmarkdown.rstudio.com/), and
[knitr](https://yihui.name/knitr/).

**Building on the strengths of
[pander](https://github.com/Rapporter/pander) and
[htmltools](https://CRAN.R-project.org/package=htmltools)**, the outputs
produced by **summarytools** can be:

  - Displayed in plain text in the *R* console (default behaviour)
  - Used in *Rmarkdown* documents and *knitted* along with other text
    and *R* output
  - Written to *html* files that fire up in
    [*RStudio*](https://www.rstudio.com/)’s Viewer pane or in the
    default browser
  - Written to plain or *Rmarkdown* text files

It is also possible to include **summarytools**’ functions in *Shiny
apps*. Notably, [radiant](https://CRAN.R-project.org/package=radiant), a
Shiny-based package for business analytics, uses `dfSummary()` to
describe imported data frames.

### Latest Improvements

Version 0.9 brings **many** changes and improvements. A summary of those
changes can be found [near the end of this page](#latest-changes).

## How to install

### From GitHub

This is the recommended method, as some minor fixes are made available
between CRAN releases.

**[Magick++](https://imagemagick.org/Magick++/) Dependency on Linux and
Mac OS**

    Before proceeding, you must install Magick++
    
     - deb: 'libmagick++-dev' (Debian, Ubuntu)
     - rpm: 'ImageMagick-c++-devel' (Fedora, CentOS, RHEL)
     - csw: 'imagemagick_dev' (Solaris)
    
    On MacOS it is recommended to use install ImageMagick-6 from homebrew
    with extra support for fontconfig and rsvg rendering:
       brew reinstall imagemagick@6 --with-fontconfig --with-librsvg
    
    For older Ubuntu versions Trusty (14.04) and Xenial (16.04) use the PPA:
       sudo add-apt-repository -y ppa:opencpu/imagemagick
       sudo apt-get update
       sudo apt-get install -y libmagick++-dev

After this is done, proceed with the installation:

``` r
install.packages("devtools")
library(devtools)
install_github("rapporter/pander") # Necessary for optimal results!
install_github("dcomtois/summarytools")
```

### From CRAN

Simply install it with `install.packages()`:

``` r
install.packages("summarytools")
```

The official documentation can be found
[here](https://CRAN.R-project.org/package=summarytools).

# The Four Core Functions

## 1 - freq() : Frequency Tables

The `freq()` function generates a table of frequencies with counts and
proportions. Since GitHub uses *markdown* rendering, we’ve set the
`style` argument to “rmarkdown”. When creating *Rmd* documents,
**knitr** takes care of converting the generated markup characters into
actual *html*.

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

We can simplify the results further and omit the *Totals* row by
specifying `totals = FALSE`.

To get familiar with the various output styles, try different values for
`style` – “simple”, “rmarkdown” or “grid”, and see how this affects the
results in the console.

#### Generating Several Frequency Tables at Once

There is more than one way to do this, but the best approach is to
simply pass the data frame object (subsetted if needed) to `freq()`:
(results not shown)

``` r
freq(tobacco[ ,c("gender", "age.gr", "smoker")])
```

We can without fear pass a whole data frame to `freq()`; it will figure
out which variables to ignore (continuous numerical variables, or
variables having many, many distinct values).

## 2 - ctable() : Cross-Tabulations

We’ll now use a sample data frame called *tobacco*, which is included in
**summarytools**. We want to cross-tabulate two categorical variables:
`smoker` and `diseased`.

Since *markdown* does not support multiline headings, we’ll show a
rendered *html* version of the
results:

``` r
print(ctable(tobacco$smoker, tobacco$diseased, prop = "r"), method = "render")
```

<img src="img/ctable-with-row-props.png" style="border:0"/>

Note that we have to set the **knitr** chunk option `results` to “asis”
for the results to appear as they should.

By default, `ctable()` shows row proportions. To show column or total
proportions, use `prop = "c"` or `prop = "t"`, respectively. To omit
proportions, use `prop = "n"`.

In the next example, we’ll create a simple “2 x 2” table:

``` r
with(tobacco, 
     print(ctable(smoker, diseased, prop = 'n', totals = FALSE),
     headings = FALSE, method = "render"))
```

<img src="img/ctable-barebones-2.png" style="border:0"/>

## 3 - descr() : Descriptive Univariate Stats

The `descr()` function generates common central tendency statistics and
measures of dispersion for numerical data. It can handle single vectors
as well as data frames, in which case it will ignore non-numerical
columns (and display a message to that effect).

``` r
descr(iris, style = "rmarkdown")
```

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

### Transposing, Selecting Statistics

If your eyes/brain prefer seeing things the other way around, just use
`transpose = TRUE`. Here, we also select only the statistics we wish to
see, and specify `headings = FALSE` to avoid reprinting the same
information as above.

We specify the stats we wish to report with the `stats` argument, which
also accepts values “all”, “fivenum”, and “common”. See `?descr` for a
complete list of available
statistics.

``` r
descr(iris, stats = "common", transpose = TRUE, headings = FALSE, style = "rmarkdown")
```

|                  | Mean | Std.Dev |  Min | Median |  Max | N.Valid | % Valid |
| ---------------: | ---: | ------: | ---: | -----: | ---: | ------: | ------: |
| **Petal.Length** | 3.76 |    1.77 | 1.00 |   4.35 | 6.90 |  150.00 |  100.00 |
|  **Petal.Width** | 1.20 |    0.76 | 0.10 |   1.30 | 2.50 |  150.00 |  100.00 |
| **Sepal.Length** | 5.84 |    0.83 | 4.30 |   5.80 | 7.90 |  150.00 |  100.00 |
|  **Sepal.Width** | 3.06 |    0.44 | 2.00 |   3.00 | 4.40 |  150.00 |  100.00 |

## 4 - dfSummary() : Data Frame Summaries

`dfSummary()` collects information about all variables in a data frame
and displays it in a singe, legible table.

To generate a summary report and have it displayed in RStudio’s Viewer
pane (or in the default Web browser if working outside RStudio), we
simply do as follows:

``` r
library(summarytools)
view(dfSummary(iris))
```

![dfSummary Output displayed in RStudio’s
viewer](img/dfSummary_in_RStudio_Viewer.png)

Of course, it is also possible to use `dfSummary()` in *Rmarkdown*
documents. It is usually a good idea to exclude a column or two,
otherwise the table might be a bit too wide. For instance, since the
*Valid* and *NA* columns are redundant, we can drop one of them.

``` r
dfSummary(tobacco, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")
```

![dfSummary-rendered-markdown](img/dfSummary-render-in-markdown.png)

While rendering *html* tables with `view()` doesn’t require it, here it
is essential to specify `tmp.img.dir`. We’ll explain why [further
below](#tmp-img-dir).

## The print() and view() Functions

**summarytools** has a generic `print` method, `print.summarytools()`.
By default, its `method` argument is set to “pander”. One of the ways in
which `view()` is useful is that we can use it to easily display *html*
outputs in *RStudio*’s Viewer. The `view()` function simply acts as a
wrapper around `print.summarytools()`, specifying `method = 'viewer'`.
When used outside *RStudio*, `method` falls back to “browser” and the
report is shown in the system’s default browser.

## Using stby() to Ventilate Results

We can use `stby()` the same way as *R*’s base function `by()` with the
four core summarytools functions. It returns a list-type object
containing as many elements as there are categories in the grouping
variable.

**Why not just use `by()`?** The reason is that `by()` creates objects
of class “by()”, which have a dedicated `print()` method conflicting
with summarytools’ way of printing list-type objects. Since `print.by()`
can’t be redefined (as of CRAN policies), the sensible solution was to
introduce a function that is essentially a clone of `by()`, except that
the objects it creates have the class “stby”, allowing the desired
flexibility.

Using the *iris* data frame, we will now display descriptive statistics
by Species.

``` r
(iris_stats_by_species <- stby(data = iris, 
                               INDICES = iris$Species, 
                               FUN = descr, stats = c("mean", "sd", "min", "med", "max"), 
                               transpose = TRUE))
```

    ## Non-numerical variable(s) ignored: Species

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

To see an *html* version of these results, we simply use `view()` (also
possible is to use `print()` with `method = "viewer"`): (results not
shown)

``` r
view(iris_stats_by_species)
# or
print(iris_stats_by_species, method = "viewer")
```

A special situation occurs when we want grouped statistics *for one
variable only*. Instead of showing several tables, each having one
column, **summarytools** assembles everything into a single table:

``` r
data(tobacco)
with(tobacco, stby(BMI, age.gr, descr, 
                        stats = c("mean", "sd", "min", "med", "max")))
```

### Descriptive Statistics

**BMI by age.gr**  
**Data Frame:** tobacco  
**N:** 258

|             | 18-34 | 35-50 | 51-70 |  71 + |
| ----------: | ----: | ----: | ----: | ----: |
|    **Mean** | 23.84 | 25.11 | 26.91 | 27.45 |
| **Std.Dev** |  4.23 |  4.34 |  4.26 |  4.37 |
|     **Min** |  8.83 | 10.35 |  9.01 | 16.36 |
|  **Median** | 24.04 | 25.11 | 26.77 | 27.52 |
|     **Max** | 34.84 | 39.44 | 39.21 | 38.37 |

The transposed version looks like this:

|           |  Mean | Std.Dev |   Min | Median |   Max |
| --------: | ----: | ------: | ----: | -----: | ----: |
| **18-34** | 23.84 |    4.23 |  8.83 |  24.04 | 34.84 |
| **35-50** | 25.11 |    4.34 | 10.35 |  25.11 | 39.44 |
| **51-70** | 26.91 |    4.26 |  9.01 |  26.77 | 39.21 |
|  **71 +** | 27.45 |    4.37 | 16.36 |  27.52 | 38.37 |

### Using stby() With ctable()

This is a little trickier – the working syntax is as
follows:

``` r
stby(list(x = tobacco$smoker, y = tobacco$diseased), tobacco$gender, ctable)
# or equivalently
with(tobacco, stby(list(x = smoker, y = diseased), gender, ctable))
```

## Using summarytools in Rmarkdown Documents

As we have seen, **summarytools** can generate both text/*markdown* and
*html* results. Both types of outputs can be used in Rmarkdown
documents. The vignette [Recommendations for Using summarytools With
Rmarkdown](https://cran.r-project.org/web/packages/summarytools/vignettes/Recommendations-rmarkdown.html)
provides good guidelines, but here are a few tips to get started:

  - Always set the `knitr` chunk option `results = 'asis'`. You can do
    this on a chunk-by-chunk basis, but it is easier to just set it
    globally in a “setup” chunk:

<!-- end list -->

``` r
    knitr::opts_chunk$set(echo = TRUE, results = 'asis')
```

        Refer to [this page](https://yihui.name/knitr/options/) for more
*knitr*’s options.

  - To get better results when generating *html* output with `method =
    'render'`, set up your *.Rmd* document so that it includes
    **summarytools**’ css. The `st_css()` function makes this very easy.

#### Initial Setup – Example

    # ---
    # title: "RMarkdown using summarytools"
    # output: html_document
    # ---
    #
    # ```{r setup, include=FALSE}
    # library(knitr)
    # opts_chunk$set(comment = NA, prompt = FALSE, cache = FALSE, results = 'asis')
    # library(summarytools)
    # st_options(plain.ascii = FALSE,          # This is a must in Rmd documents
    #            style = "rmarkdown",          # idem
    #            dfSummary.varnumbers = FALSE, # This keeps results narrow enough
    #            dfSummary.valid.col = FALSE)  # idem
    #```
    #
    # ```{r, echo=FALSE}
    # st_css()
    # ```

### Managing Lengthy dfSummary() Outputs in Rmarkdown Documents

For data frames containing numerous variables, we can use the
`max.tbl.height` argument to wrap the results in a scrollable window
having the specified height, in pixels. For instance:

``` r
print(dfSummary(tobacco, valid.col = FALSE, graph.magnif = 0.75), 
      max.tbl.height = 300, method = "render")
```

![dfSummary-scroll-window](img/dfSummary-scroll-window.png)

## Writing Output to Files

We can use the `file` argument with `print()` or `view()` to indicate
that we want to save the results in a file, be it *html*, *Rmd*, *md*,
or just plain text (*txt*). The file extension indicates to
**summarytools** what type of file should be generated.

``` r
view(iris_stats_by_species, file = "~/iris_stats_by_species.html")
```

### Appending Output Files

The `append` argument allows adding content to existing files generated
by **summarytools**. This is useful if you want to include several
statistical tables in a single file. It is a quick alternative to
creating an *.Rmd* document.

## Global options

The following options can be set with
`st_options()`:

### General Options

|        Option name |   Default | Note                                          |
| -----------------: | --------: | :-------------------------------------------- |
|              style |  “simple” | Set to “rmarkdown” in .Rmd documents          |
|        plain.ascii |      TRUE | Set to FALSE in .Rmd documents                |
|       round.digits |         2 | Number of decimals to show                    |
|           headings |      TRUE | Formerly “omit.headings”                      |
|           footnote | “default” | Personalize, or set to NA to omit             |
|     display.labels |      TRUE | Show variable / data frame labels in headings |
| bootstrap.css (\*) |      TRUE | Include Bootstrap 4 css in *html* outputs     |
|         custom.css |        NA | Path to your own css file                     |
|        escape.pipe |     FALSE | Useful for some Pandoc conversions            |
|  subtitle.emphasis |      TRUE | Controls headings formatting                  |
|               lang |      “en” | Language (always 2-letter, lowercase)         |

(\*) Set to FALSE in Shiny
apps

### Function-Specific Options

|            Option name | Default | Note                                                |
| ---------------------: | ------: | :-------------------------------------------------- |
|            freq.totals |    TRUE | Display totals row in freq()                        |
|        freq.report.nas |    TRUE | Display <NA> row and “valid” columns                |
|            ctable.prop |     “r” | Display **r**ow proportions by default              |
|          ctable.totals |    TRUE | Show marginal totals                                |
|            descr.stats |   “all” | “fivenum”, “common” or vector of stats              |
|        descr.transpose |   FALSE |                                                     |
|           descr.silent |   FALSE | Hide console messages                               |
|   dfSummary.varnumbers |    TRUE | Show variable numbers in 1st col.                   |
|   dfSummary.labels.col |    TRUE | Show variable labels when present                   |
|    dfSummary.graph.col |    TRUE | Show graphs                                         |
|    dfSummary.valid.col |    TRUE | Include the Valid column in the output              |
|       dfSummary.na.col |    TRUE | Include the Missing column in the output            |
| dfSummary.graph.magnif |       1 | Zoom factor for bar plots and histograms            |
|       dfSummary.silent |   FALSE | Hide console messages                               |
|            tmp.img.dir |      NA | Directory to store [temporary images](#tmp-img-dir) |

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

When a **summarytools** object is created, its formatting attributes are
stored within it. However, you can override most of them when using the
`print()` method or the `view()` function.

### Overriding Function-Specific Arguments

|       Argument | freq | ctable | descr | dfSummary |
| -------------: | :--: | :----: | :---: | :-------: |
|          style |  x   |   x    |   x   |     x     |
|   round.digits |  x   |   x    |   x   |           |
|    plain.ascii |  x   |   x    |   x   |     x     |
|        justify |  x   |   x    |   x   |     x     |
|       headings |  x   |   x    |   x   |     x     |
| display.labels |  x   |   x    |   x   |     x     |
|     varnumbers |      |        |       |     x     |
|     labels.col |      |        |       |     x     |
|      graph.col |      |        |       |     x     |
|      valid.col |      |        |       |     x     |
|         na.col |      |        |       |     x     |
|     col.widths |      |        |       |     x     |
|         totals |  x   |   x    |       |           |
|     report.nas |  x   |        |       |           |
|   display.type |  x   |        |       |           |
|        missing |  x   |        |       |           |
|   split.tables |  x   |   x    |   x   |     x     |
|        caption |  x   |   x    |   x   |     x     |

### Overriding Headings Content

|         Argument | freq | ctable | descr | dfSummary |
| ---------------: | :--: | :----: | :---: | :-------: |
|       Data.frame |  x   |   x    |   x   |     x     |
| Data.frame.label |  x   |   x    |   x   |     x     |
|         Variable |  x   |   x    |   x   |           |
|   Variable.label |  x   |   x    |   x   |           |
|            Group |  x   |   x    |   x   |     x     |
|             date |  x   |   x    |   x   |     x     |
|          Weights |  x   |        |   x   |           |
|        Data.type |  x   |        |       |           |
|     Row.variable |      |   x    |       |           |
|     Col.variable |      |   x    |       |           |

#### Example

Here’s an example in which we override 3 function-specific arguments,
and one element of the heading:

``` r
(age_stats <- freq(tobacco$age.gr)) 
```

### Frequencies

**tobacco$age.gr**  
**Type:** Factor

|            | Freq | % Valid | % Valid Cum. | % Total | % Total Cum. |
| ---------: | ---: | ------: | -----------: | ------: | -----------: |
|  **18-34** |  258 |   26.46 |        26.46 |   25.80 |        25.80 |
|  **35-50** |  241 |   24.72 |        51.18 |   24.10 |        49.90 |
|  **51-70** |  317 |   32.51 |        83.69 |   31.70 |        81.60 |
|   **71 +** |  159 |   16.31 |       100.00 |   15.90 |        97.50 |
| **\<NA\>** |   25 |         |              |    2.50 |       100.00 |
|  **Total** | 1000 |  100.00 |       100.00 |  100.00 |       100.00 |

``` r
print(age_stats, report.nas = FALSE, totals = FALSE, display.type = FALSE,
      Variable.label = "Age Group")
```

### Frequencies

**tobacco$age.gr**  
**Label:** Age Group

|           | Freq |     % | % Cum. |
| --------: | ---: | ----: | -----: |
| **18-34** |  258 | 26.46 |  26.46 |
| **35-50** |  241 | 24.72 |  51.18 |
| **51-70** |  317 | 32.51 |  83.69 |
|  **71 +** |  159 | 16.31 | 100.00 |

Note that the original attributes are still part of the *age\_stats*
object, left unchanged.

## Order of Priority for Options / Parameters

1.  Options overridden explicitly in `print()` or `view()` have
    precedence
2.  Options specified as explicit arguments to `freq() / ctable() /
    descr() / dfSummary()` come second
3.  Global options set with `st_options` come third

## Customizing looks with CSS

**summarytools** uses *RStudio*’s [htmltools
package](https://CRAN.R-project.org/package=htmltools) and version 4 of
[Bootstrap](https://getbootstrap.com/)’s cascading stylesheets.

It is possible to include your own *css* if you wish to customize the
look of the output tables. See `?print.summarytools` for all the
details, but here is a quick example.

Say you need to make the font size really really small. For this, you
would create a *.css* file - let’s call it “custom.css” - containing a
class definition such as the following:

``` css
.tiny-text {
  font-size: 8px;
}
```

Then, to apply it to a **summarytools** object and display it in your
browser:

``` r
view(dfSummary(tobacco), custom.css = 'path/to/custom.css', 
     table.classes = 'tiny-text')
```

To display a smaller table that is not **that** small, you can use the
**provided css class `st-small`**.

## Working with *Shiny* apps

To include **summarytools** functions in *Shiny* apps, it is recommended
that you:

  - set `bootstrap.css = FALSE` to avoid interacting with the app’s
    layout  
  - omit headings by setting the global option `headings = FALSE`
  - adjust the size of the graphs in `dfSummary()` using the
    `dfSummary.graph.magnif` global option
  - if `dfSummary()` outputs are too wide, try omitting a column or two
    (`valid.col` and `varnumbers`, for instance)
  - if needed, set the column widths manually with the `col.widths`
    parameter of the `print()` method or the `view()` function

<!-- end list -->

``` r
print(dfSummary(somedata, graph.magnif = 0.8), 
      method = 'render',
      headings = FALSE,
      bootstrap.css = FALSE)
```

## <a id="tmp-img-dir"></a>Graphs in Markdown dfSummaries

When generating *markdown* (as opposed to *html*) summaries in an *.Rmd*
document, three elements are needed to display proper *png* graphs:

1 - `plain.ascii` is FALSE  
2 - `style` is “grid”  
3 - `tmp.img.dir` is defined

Why the third element? Although *R* makes it really easy to create
temporary files and directories, they do have long pathnames, especially
on Windows. Combine this with the fact that Pandoc currently determines
the final (rendered) column widths by counting characters, including
those of pathnames pointing to images. What we get is… some issues of
proportion (\!).

At this time, there seems to be only one solution around this problem:
cut down on characters in pathnames. So instead of
    this:

    +-----------+-------------------------------------------------------------------------+---------+
    | Variable  | Graph                                                                   | Valid   |
    +===========+=========================================================================+=========+
    | gender\   | ![](C:/Users/johnny/AppData/Local/Temp/RtmpYRgetx/file5aa4549a4d71.png) | 978\    |
    | [factor]  |                                                                         | (97.8%) |
    +----+---------------+----------------------------------------------------------------+---------+

…we aim for this:

    +---------------+----------------------+---------+
    | Variable      | Graph                | Valid   |
    +===============+======================+=========+
    | gender\       | ![](/tmp/ds0001.png) | 978\    |
    | [factor]      |                      | (97.8%) |
    +---------------+----------------------+---------+

Now CRAN policies are really strict when it comes to writing content in
the user directories, or anywhere outside *R’s temporary zone* (for good
reasons). So we need to let the users set this location themselves,
therefore implicitly consenting to content being written outside *R’s
temporary zone*.

On Mac OS and Linux, using “/tmp” makes a lot of sense: it’s short, and
it’s self-cleaning. On Windows, there is no such convenient directory,
so we need to pick one – be it absolute (“/tmp”) or relative (“img”, or
simply “.”). Two things are to be kept in mind: it needs to be short (5
characters max) and we need to clean it up manually.

## Translations

It is now possible to switch the language used in the outputs. So far,
the following languages are available: French (fr), Spanish (es),
Portuguese (pt), and Turkish (tr). With the community’s involvement, I
hope we can gather a lot more.

### Switching Languages

To switch languages, simply use

``` r
st_options(lang = "fr")
```

Any function will now produce outputs using that language:

``` r
view(freq(iris$Species))
```

<img style="float: center;" src="img/freq-fr.png" width=500px />

The language used for producing the object is stored within it as an
attribute. This is to avoid problems when switching languages between
the moment the object is stored, and the moment at which it is printed.

### Non-UTF-8 Locales

On most Windows systems, it will be necessary to change the `LC_CTYPE`
element of the locale settings if the character set is not included in
the current locale. For instance, in order to get good results – or
rather, any results at all – with the Russian language in a “latin1”
environment, we’ll need to do this:

``` r
Sys.setlocale("LC_CTYPE", "russian")
st_options(lang = 'ru')
```

Then, to go back to default settings:

``` r
Sys.setlocale("LC_CTYPE", "")
st_options(lang = "en")
```

Note that russian translations are not currently available, but should
be in the next release.

### Defining and Using Custom Translations

With the new function `useTranslations()`, you can add your own set of
translations. For this, download the template *csv* file from [this
page](https://raw.githubusercontent.com/dcomtois/summarytools/master/translations/language_template.csv).

After you’re done translating the +/- 70 items, simply call the
`useTranslations()` function, giving it as sole argument the path to the
*csv* file you’ve just created. Note that such custom translations will
not persist across R sessions. This means that we should always have
handy this *csv* file if we’re to print objects created with it.

## <a id="latest-changes"></a>Latest Changes and Improvements

As stated earlier, version 0.9 brings **many** improvements to
**summarytools**. Here are the key elements:

  - Translations
  - Improved printing of list objects
      - Objects of class “stby” are automatically printed in the console
        with optimal results; no more need for `view(x, method =
        "pander")`; simply use `stby()` instead of `by()`
      - Regular lists containing **summarytools** objects can also be
        printed with optimal results simply by calling `print(x)` (as
        opposed to “stby” objects, their automatic printing will **not**
        be optimal; that being said, `freq()` now accepts data frames as
        its first argument, so the need for `lapply()` is greatly
        diminished)
  - Easier management of global settings with `st_options()`
      - `st_options()` now has as many parameters as there are options
        to set, making it possible to set all options with only one
        function call; legacy way of setting options is still supported
      - Several global options were added, with a focus on simplifying
        *Rmarkdown* document creation
  - Changes to `freq()`
      - As mentioned earlier, the function now accepts data frames as
        its main argument; this makes practically obsolete the use of
        `lapply()` with it
  - Improved outputs when using `stby()`
  - Changes to `ctable()`
      - Fully supports `stby()`
      - Improved number alignment
  - Changes to `descr()`
      - For the `stats` argument, Values “fivenum” and “common” are now
        allowed, the latter representing the collection of *mean*, *sd*,
        *min*, *med*, *max*, *n.valid*, and *pct.valid*
      - Improved outputs when using `stby()`
      - The variable used for weights (if any) is removed automatically
        from the data so no stats are produced for it
  - Changes to `dfSummary()`
      - Now fully compatible with *Rmarkdown*
      - Number of columns is now included in the heading section
      - Number of duplicated rows is also shown in the heading section
      - Bar plots now more accurately reflect counts, as they are not
        stretched across table cells (this allows the comparison of
        frequencies across variables)
      - Columns with particular content (unary/binary, integer
        sequences, UPC/EAN codes) are treated differently; more relevant
        information is displayed, while irrelevant information is hidden
      - For *html* outputs, a new parameter `col.widths` can be used to
        set the width of the resulting table’s columns; this addresses
        an issue with some graphs not being shown at the desired
        magnification level (although much effort has been put into
        improving this as well)
      - `max.tbl.height` parameter added

### Other Notable Changes

  - The `omit.headings` parameter has been replaced by the more
    straightforward (and still boolean) `headings`. `omit.heandings` is
    still supported but will be deprecated in future releases
  - Because it was subject to errors, the *Rows Subset* heading element
    has been removed. If there is a strong need for it, I can bring it
    back in a future release (just let me known by email or on GitHub if
    you’d like to have it back)
  - Under the hood, much has been going on; the lengthier functions have
    been split into more manageable parts, and several normalizing
    operations were performed, facilitating maintenance and improving
    code readability

### Backward Compatibility

No changes break backward compatibility, but at least one legacy feature
will be abandoned in some further release. Namely, the boolean parameter
`omit.headings`, which has been replaced by the more straightforward
`headings`. For now, a message is shown whenever the “old” parameter
name is used, encouraging users to switch to the newer one.

## Stay Up-to-date

For a preview of what’s coming in the next release, see the [development
branch](https://github.com/dcomtois/summarytools/tree/dev-current).

## Final notes

The package comes with no guarantees. It is a work in progress and
feedback / feature requests are welcome. Just send me an email
(dominic.comtois (at) gmail.com), or open an
[Issue](https://github.com/dcomtois/summarytools/issues) if you find a
bug or wish to submit a feature request.

Also, the package grew significantly larger, and maintaining it all by
myself is time consuming. If you would like to contribute, please get in
touch, I’d greatly appreciate the help.
