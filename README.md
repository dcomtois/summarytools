
# [summarytools: An *R* Package For Descriptive Statistics](https://github.com/dcomtois/summarytools)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/summarytools)](https://cran.r-project.org/package=summarytools)
[![](http://cranlogs.r-pkg.org/badges/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html)
[![](http://cranlogs.r-pkg.org/badges/grand-total/summarytools)](http://cran.rstudio.com/web/packages/summarytools/index.html)
[![Rdoc](http://www.rdocumentation.org/badges/version/summarytools)](http://www.rdocumentation.org/packages/summarytools)

## Latest News

Version 0.8.3 brings several improvements to summarytools. I’m making it
available here on GitHub before submitting it to CRAN. For details on
the changes, please see the NEWS file, as well as the rest of this
document.

Two vignettes complement information found on this page:  
\- [Introduction to
summarytools](https://cdn.rawgit.com/dcomtois/summarytools/dev-current/inst/doc/Introduction.html)  
\- [Recommendations for Using summarytools With
Rmarkdown](https://cdn.rawgit.com/dcomtois/summarytools/dev-current/inst/doc/Recommendations-rmarkdown.html)

## What is summarytools?

*summarytools* is an [R](http://r-project.org) package providing tools
to *neatly and quickly summarize data*. Its main purpose is to provide
hassle-free functions that every *R* programmer once wished were
included in base R:

  - `descr()` : **descriptive statistics** with all common univariate
    statistics for numerical vectors.
  - `freq()` : **frequency tables** with proportions, cumulative
    proportions and missing data information.
  - `ctable()` : **cross-tabulations** between two factors or any
    discrete data, with total, rows or columns proportions.
  - `dfSummary()` : Extensive **data frame summaries** that facilitate
    data cleaning and firsthand evaluation.

Most commercial statistical software suites provide a wide range of
functions and procedures out-of-the-box, making it very simple to
create, with code or with a few point-and-click actions, well-formatted
reports. For most tasks not relying on advanced statistical methods,
*summarytools* allows you to do just that.

# How to install

To benefit from all the latests fixes, install it from GitHub:

``` r
install.packages("devtools")
library(devtools)
install_github('dcomtois/summarytools')
```

To install the most recent version on the *R-CRAN* repository, just type
into your *R* console:

``` r
install.packages("summarytools")
```

For the most **up-to-date version** that has all the latest features
**but** might also contain bugs (which can be fixed rapidly in most
cases):

``` r
install.packages("devtools")
library(devtools)
install_github('dcomtois/summarytools', ref='dev-current')
```

You can also see the source code and documentation on the official *R*
site [here](http://cran.r-project.org/web/packages/summarytools/).

# A First Example

Using the *iris* sample data frame, we’ll jump right to the most popular
function in the package, `dfSummary` (*Data Frame Summary*).

With the following 2 lines of code, we’ll generate a summary report for
’’iris\`\` and have it displayed in
[*RStudio*](http://www.rstudio.com/)’s Viewer pane:

``` r
library(summarytools)
view(dfSummary(iris), method = 'render')
```

<!--html_preserve-->

<div class="container st-container">

<h3>

Data Frame Summary

</h3>

<h4>

iris

</h4>

<strong>N</strong>:
150

<table class="table table-striped table-bordered st-table st-table-striped st-table-bordered st-multiline ">

<thead>

<tr>

<th align="center">

<strong>No</strong>

</th>

<th align="center">

<strong>Variable</strong>

</th>

<th align="center">

<strong>Stats / Values</strong>

</th>

<th align="center">

<strong>Freqs (% of Valid)</strong>

</th>

<th align="center">

<strong>Graph</strong>

</th>

<th align="center">

<strong>Valid</strong>

</th>

<th align="center">

<strong>Missing</strong>

</th>

</tr>

</thead>

<tbody>

<tr>

<td align="center">

1

</td>

<td align="left">

Sepal.Length \[numeric\]

</td>

<td align="left">

mean (sd) : 5.84 (0.83) min \< med \< max : 4.3 \< 5.8 \< 7.9 IQR (CV) :
1.3 (0.14)

</td>

<td align="left">

35 distinct
val.

</td>

<td align="center" border="0">

<img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAJYAAABkCAMAAABThTnCAAAADFBMVEX9/v2mpqby8vL9/v28xacEAAAABHRSTlP///8AQCqp9AAAAJlJREFUaIHt2ssKgCAQQFGr///nlkFGD1FniHP35sFFili2lJVowHVYX+rIWupSsNZzWIeiKgerQmBhYWFFsi42uKe/5xTW46RYWFhYWFhYWFhYWFhYWD9hNV89j2U1Lx8WFhYWFhYWFhYWFhYWFhYWFhYW1hDWiyJYL8bkZ7U8GpvB6vFFLCwsLCwsrGGsRN2et4LD+lJS1g7jOo4yUNMCcwAAAABJRU5ErkJggg==">

</td>

<td align="center">

150 (100%)

</td>

<td align="center">

0 (0%)

</td>

</tr>

<tr>

<td align="center">

2

</td>

<td align="left">

Sepal.Width \[numeric\]

</td>

<td align="left">

mean (sd) : 3.06 (0.44) min \< med \< max : 2 \< 3 \< 4.4 IQR (CV) : 0.5
(0.14)

</td>

<td align="left">

23 distinct
val.

</td>

<td align="center" border="0">

<img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAJYAAABkCAMAAABThTnCAAAADFBMVEX9/v2mpqby8vL9/v28xacEAAAABHRSTlP///8AQCqp9AAAANxJREFUaIHt2csKhDAMRmEv7//OwmyMWGb6J6kNzjkrKUg/4qbqspdsmQ1oB0vJw1pt6aJPLtZ2ButXsJRgKcFSgqUESwmWEiwlWEqwlGApwVKCpQRLCZbShTXmm1KYNWZw3Sw7lkqsJgUWLFiwYMGC1WAlHnIyWXYZFixYsGDB8rHW9jvrdFbH/rBgwYKlsoLn+lGs4OT+mOV4nk+wHIOD9UpWx3+BKSzv/rBgwQqyAm9eQ1kZ+8PKZvUech5m2eWirG+Dm8gyl7CUO28PtAbLLltWoQyrXLCUirIOKk6bE8VcSO8AAAAASUVORK5CYII=">

</td>

<td align="center">

150 (100%)

</td>

<td align="center">

0 (0%)

</td>

</tr>

<tr>

<td align="center">

3

</td>

<td align="left">

Petal.Length \[numeric\]

</td>

<td align="left">

mean (sd) : 3.76 (1.77) min \< med \< max : 1 \< 4.35 \< 6.9 IQR (CV) :
3.5 (0.47)

</td>

<td align="left">

43 distinct
val.

</td>

<td align="center" border="0">

<img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAJYAAABkCAMAAABThTnCAAAADFBMVEX9/v2mpqby8vL9/v28xacEAAAABHRSTlP///8AQCqp9AAAAJlJREFUaIHt2TsOgCAQQEHU+9/ZwsYCSCDirnHeAWCg4lOOlJVoQD2skS7W1iiatVfDwsLCwsLCwlrNap2OZhb3JKs+yNSeY2FhYWFhYWHlZE0cw95gTQyOhYWFhYWFhYWFhYWFhYWFhYW1jNV6R4tmDU2PhYWFhfVLVu8rKJDVmxMLCwsLCwvrw6yh6/MCVqJurHRhjZSUdQJhgJMslluacAAAAABJRU5ErkJggg==">

</td>

<td align="center">

150 (100%)

</td>

<td align="center">

0 (0%)

</td>

</tr>

<tr>

<td align="center">

4

</td>

<td align="left">

Petal.Width \[numeric\]

</td>

<td align="left">

mean (sd) : 1.2 (0.76) min \< med \< max : 0.1 \< 1.3 \< 2.5 IQR (CV) :
1.5 (0.64)

</td>

<td align="left">

22 distinct
val.

</td>

<td align="center" border="0">

<img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAJYAAABkCAMAAABThTnCAAAADFBMVEX9/v2mpqby8vL9/v28xacEAAAABHRSTlP///8AQCqp9AAAAI9JREFUaIHt17kOgCAQQEGP//9nCxtNFMKxWYp5LQEmNMB2LtmWDfgOq6WbtZdKZB3/YWFhYWFhYWHNZYW9hwZZUYeMhYWFhYWFhYWFhYX1njb0K4pjdS6JhYWFhYUVw6relzms6nZYWFhYWFhYWFhYWFhYWFiBrFKJrL4xLCwsLCwsrImshXqwlgurpUVZFzxAjeSoCeYjAAAAAElFTkSuQmCC">

</td>

<td align="center">

150 (100%)

</td>

<td align="center">

0 (0%)

</td>

</tr>

<tr>

<td align="center">

5

</td>

<td align="left">

Species \[factor\]

</td>

<td align="left">

1.  setosa
2.  versicolor
3.  virginica
    </td>
    <td align="left">
    50 (33.3%) 50 (33.3%) 50
    (33.3%)
    </td>
    <td align="center" border="0">
    <img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAJYAAABOCAMAAAD1sh+SAAAADFBMVEX9/v2mpqb39/f9/v0TNkn1AAAABHRSTlP///8AQCqp9AAAAINJREFUaIHt16EBw0AQxEAn7r/npIAng36BpgKBO7DPO+m5HXA2m/VZ9M/67ilLlCXKEmWJskRZoixRlihLDGctmh1kk2azbp/R0e4n3m44KEuUJcoSZYmyRFmiLFGWKEs0X0lZYva2Vj/xdsNBWaIsUZYoS5QlyhJlibJEWaL5SsoSPwWqXr0bCZNHAAAAAElFTkSuQmCC">
    </td>
    <td align="center">
    150 (100%)
    </td>
    <td align="center">
    0
(0%)
    </td>
    </tr>
    </tbody>
    </table>
    </div>
    <!--/html_preserve-->

<!-- ![Example of dfSummary Output displayed in RStudio's viewer](img/dfSummary_in_RStudio_Viewer.png) -->

##### Building on the strengths of [pander](https://github.com/Rapporter/pander) and [htmltools](http://cran.r-project.org/web/packages/htmltools/index.html), the outputs produced by summarytools can be:

  - Displayed in plain text in the *R* console (default behaviour)
  - Used in *Rmardown* documents and *knitted* along with other text and
    *R* output
  - Written to *html* files that fire up in
    [*RStudio*](http://www.rstudio.com/)’s Viewer pane or in your
    system’s default browser
  - Written to plain text files / *Rmarkdown* text files

# Four Core Functions

## 1 - Frequency tables with `freq()`

The `freq()` function generates a table of frequencies with counts and
proportions.

``` r
library(summarytools)
freq(iris$Species, style = "rmarkdown")
```

## Frequencies

**Species **  
**Data frame:** iris  
**Type:** Factor
(unordered)

|                | Freq | % Valid | % Valid Cum. | % Total | % Total Cum. |
| -------------: | ---: | ------: | -----------: | ------: | -----------: |
|     **setosa** |   50 |   33.33 |        33.33 |   33.33 |        33.33 |
| **versicolor** |   50 |   33.33 |        66.67 |   33.33 |        66.67 |
|  **virginica** |   50 |   33.33 |       100.00 |   33.33 |       100.00 |
|     **\<NA\>** |    0 |         |              |    0.00 |       100.00 |
|      **Total** |  150 |  100.00 |       100.00 |  100.00 |       100.00 |

## 2 - Descriptive (univariate) statistics with `descr()`

The `descr()` function generates common central tendency statistics and
measures of dispersion for numerical data. It can handle single vectors
as well as dataframes, in which case it just ignores non-numerical
columns.

We’ll use the *rmarkdown* style for the next example:

``` r
descr(iris, style = "rmarkdown")
```

    ## Non-numerical variable(s) ignored: Species

### Descriptive Statistics

**Data Frame:** iris  
**N:**
150

|                 | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width |
| --------------: | -----------: | ----------: | -----------: | ----------: |
|        **Mean** |         5.84 |        3.06 |         3.76 |        1.20 |
|     **Std.Dev** |         0.83 |        0.44 |         1.77 |        0.76 |
|         **Min** |         4.30 |        2.00 |         1.00 |        0.10 |
|          **Q1** |         5.10 |        2.80 |         1.60 |        0.30 |
|      **Median** |         5.80 |        3.00 |         4.35 |        1.30 |
|          **Q3** |         6.40 |        3.30 |         5.10 |        1.80 |
|         **Max** |         7.90 |        4.40 |         6.90 |        2.50 |
|         **MAD** |         1.04 |        0.44 |         1.85 |        1.04 |
|         **IQR** |         1.30 |        0.50 |         3.50 |        1.50 |
|          **CV** |         7.06 |        7.01 |         2.13 |        1.57 |
|    **Skewness** |         0.31 |        0.31 |       \-0.27 |      \-0.10 |
| **SE.Skewness** |         0.20 |        0.20 |         0.20 |        0.20 |
|    **Kurtosis** |       \-0.61 |        0.14 |       \-1.42 |      \-1.36 |
|     **N.Valid** |       150.00 |      150.00 |       150.00 |      150.00 |
|   **Pct.Valid** |       100.00 |      100.00 |       100.00 |      100.00 |

#### Transposing and selecting the stats you need

If your eyes/brain prefer seeing things the other way around, just use
“transpose=TRUE”. Here, we also select only the statistics we wish to
see:

``` r
descr(iris, stats = c("mean", "sd", "min", "med", "max"), transpose = TRUE, 
      omit.headings = TRUE, style = "rmarkdown")
```

    ## Non-numerical variable(s) ignored: Species

|                  | Mean | Std.Dev |  Min | Median |  Max |
| ---------------: | ---: | ------: | ---: | -----: | ---: |
| **Sepal.Length** | 5.84 |    0.83 | 4.30 |   5.80 | 7.90 |
|  **Sepal.Width** | 3.06 |    0.44 | 2.00 |   3.00 | 4.40 |
| **Petal.Length** | 3.76 |    1.77 | 1.00 |   4.35 | 6.90 |
|  **Petal.Width** | 1.20 |    0.76 | 0.10 |   1.30 | 2.50 |

## 3 - Cross-tabulations with `ctable()`

Here we’ll use a sample data frame included in the package (*tobacco*),
which contains simulated data. Say we want to cross-tabulate variables
`smoker` and `diseased`. By default, `ctable()` gives row proportions,
so we don’t need to specify any additionnal parameter.

Here, instead of `ctable(tobacco$smoker, tobacco$diseased)`, we’ll make
use of the `with()` (R-base) function. Also, since *markdown* has not
support (yet) for multi-line headings, we’ll use html rendering.

``` r
with(tobacco, view(ctable(smoker, diseased), method = "render"))
```

<!--html_preserve-->

<div class="container st-container">

<h3>

Cross-Tabulation / Row Proportions

</h3>

<h4>

smoker \* diseased

</h4>

<strong>Data Frame</strong>:
tobacco

<table class="table table-bordered st-table st-table-bordered st-cross-table ">

<thead>

<tr>

<th>

</th>

<th colspan="2">

diseased

</th>

<th>

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

<th align="center">

Total

</th>

</tr>

</thead>

<tbody>

<tr>

<td align="center">

<strong>Yes</strong>

</td>

<td>

<span>125 (41.95%)</span>

</td>

<td>

<span>173 (58.05%)</span>

</td>

<td>

<span> 298 (100.00%)</span>

</td>

</tr>

<tr>

<td align="center">

<strong>No</strong>

</td>

<td>

<span> 99 (14.10%)</span>

</td>

<td>

<span>603 (85.90%)</span>

</td>

<td>

<span> 702 (100.00%)</span>

</td>

</tr>

<tr>

<td align="center">

<strong>Total</strong>

</td>

<td>

<span>224 (22.40%)</span>

</td>

<td>

<span>776 (77.60%)</span>

</td>

<td>

<span>1000 (100.00%)</span>

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

It is possible to display *column*, *total*, or no proportions at all.
We can also omit the marginal totals to have a simple 2x2 table.

``` r
with(tobacco, view(ctable(smoker, diseased, 
                          prop = 'n', totals = FALSE, omit.headings = TRUE),
                   method = "render"))
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

## 4 - Data Frame Summaries

As seen earlier, the `dfSummary()` function gives information for all
variables in a singe table. Version 0.8.0 introduced graphs for both
ascii and *html* tables.

  - Note that due to rmarkdown compatibility issues, the text graphs
    being histograms are not shown. We’re working on this.

<!-- end list -->

``` r
dfSummary(tobacco, plain.ascii = FALSE, style = "grid")
```

## Data Frame Summary

**tobacco**  
**N:** 1000

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 14%" />
<col style="width: 25%" />
<col style="width: 19%" />
<col style="width: 18%" />
<col style="width: 9%" />
<col style="width: 9%" />
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
<td>IIIIIIIIIIIIIIII<br />
IIIIIIIIIIIIIIII</td>
<td>978<br />
(97.8%)</td>
<td>22<br />
(2.2%)</td>
</tr>
<tr class="even">
<td>2</td>
<td>age<br />
[numeric]</td>
<td>mean (sd) : 49.6 (18.29)<br />
min &lt; med &lt; max :<br />
18 &lt; 50 &lt; 80<br />
IQR (CV) : 32 (0.37)</td>
<td>63 distinct val.</td>
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
<td>IIIIIIIIIIIII<br />
IIIIIIIIIIII<br />
IIIIIIIIIIIIIIII<br />
IIIIIIII</td>
<td>975<br />
(97.5%)</td>
<td>25<br />
(2.5%)</td>
</tr>
<tr class="even">
<td>4</td>
<td>BMI<br />
[numeric]</td>
<td>mean (sd) : 25.73 (4.49)<br />
min &lt; med &lt; max :<br />
8.83 &lt; 25.62 &lt; 39.44<br />
IQR (CV) : 5.72 (0.17)</td>
<td>974 distinct val.</td>
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
<td>IIIIII<br />
IIIIIIIIIIIIIIII</td>
<td>1000<br />
(100%)</td>
<td>0<br />
(0%)</td>
</tr>
<tr class="even">
<td>6</td>
<td>cigs.per.day<br />
[numeric]</td>
<td>mean (sd) : 6.78 (11.88)<br />
min &lt; med &lt; max :<br />
0 &lt; 0 &lt; 40<br />
IQR (CV) : 11 (1.75)</td>
<td>37 distinct val.</td>
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
IIIIIIIIIIIIIIII</td>
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
21 ( 9.4%)</td>
<td>IIIIIIIIIIIIIIII<br />
IIIIIIIIIIIIIII<br />
IIIIIIIII<br />
IIIIIIII<br />
IIIIIIII<br />
IIIIIIII<br />
IIIIII<br />
IIIIII<br />
IIIII<br />
IIII<br />
IIIIIIIII</td>
<td>222<br />
(22.2%)</td>
<td>778<br />
(77.8%)</td>
</tr>
<tr class="odd">
<td>9</td>
<td>samp.wgts<br />
[numeric]</td>
<td>mean (sd) : 1 (0.08)<br />
min &lt; med &lt; max :<br />
0.86 &lt; 1.04 &lt; 1.06<br />
IQR (CV) : 0.19 (0.08)</td>
<td>0.86!: 267 (26.7%)<br />
1.04!: 249 (24.9%)<br />
1.05!: 324 (32.4%)<br />
1.06!: 160 (16.0%)<br />
! rounded</td>
<td>IIIIIIIIIIIII<br />
IIIIIIIIIIII<br />
IIIIIIIIIIIIIIII<br />
IIIIIII</td>
<td>1000<br />
(100%)</td>
<td>0<br />
(0%)</td>
</tr>
</tbody>
</table>

## Using summarytools in Rmarkdown documents

*summarytools* uses the [pander](https://github.com/Rapporter/pander)
package to generate plain-text content, and *htmltools* to generate
*html*. Both types of outputs can be used in Rmarkdown, according to our
preferences. See [this
vignette](https://cran.r-project.org/web/packages/summarytools/vignettes/Recommendations-rmarkdown.html)
to get all the details, but if you’re in a hurry, here are a few tips to
get good results:

  - Always set the `knitr` chunk option `results = 'asis'`. You can do
    this globally or on a chunk-by-chunk basis. See [this
    page](https://yihui.name/knitr/options/) for more information.
  - To use the ‘render’ method, set up your .Rmd document so it includes
    summarytool’s css (see example)

### Example (result not shown)

    # ---
    # title: "RMarkdown using summarytools"
    # output: 
    #   html_document: 
    #     css: C:/R/win-library/3.4/summarytools/includes/stylesheets/summarytools.css
    # ---
    
    # ```{r, results='asis'}
    # library(summarytools)  
    # freq(tobacco$smoker, style='rmarkdown')  
    # 
    # print(dfSummary(tobacco, style = 'grid', plain.ascii = FALSE, graph.magnif = 0.85), 
    #       method = 'render', omit.headings = TRUE)
    # ```

## The print() and view() functions

*summarytools* has a generic `print()` method, `print.summarytools`. By
default, its `method` argument is set to `'pander'`. To easily display
*html* outputs in *RStudio*’s Viewer, we use the `view()` function,
which acts as a wrapper around the generic `print()` function, this time
using `method = 'viewer'`. When used outside *RStudio*, the `method`
falls back on `'browser'` and the report is opened with your system’s
default browser.

## Using by() to Split Results By Sub-Groups

With `freq()` and `descr()` you can use *R*’s base function `by()` to
have statistics split by a ventilation variable. *R* returns a `list`
containing *summarytools* objects. Using the `view()` function with
those objects is necessary in order to have non-redundant and clean
section headings.

Example: Using the *iris* data frame, we will display descriptive
statistics broken down by Species.

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

**Data Frame:** iris  
**Group:** Species = setosa  
**N:** 50

|                  | Mean | Std.Dev |  Min | Median |  Max |
| ---------------: | ---: | ------: | ---: | -----: | ---: |
| **Sepal.Length** | 5.01 |    0.35 | 4.30 |   5.00 | 5.80 |
|  **Sepal.Width** | 3.43 |    0.38 | 2.30 |   3.40 | 4.40 |
| **Petal.Length** | 1.46 |    0.17 | 1.00 |   1.50 | 1.90 |
|  **Petal.Width** | 0.25 |    0.11 | 0.10 |   0.20 | 0.60 |

**Group:** Species = versicolor  
**N:** 50

|                  | Mean | Std.Dev |  Min | Median |  Max |
| ---------------: | ---: | ------: | ---: | -----: | ---: |
| **Sepal.Length** | 5.94 |    0.52 | 4.90 |   5.90 | 7.00 |
|  **Sepal.Width** | 2.77 |    0.31 | 2.00 |   2.80 | 3.40 |
| **Petal.Length** | 4.26 |    0.47 | 3.00 |   4.35 | 5.10 |
|  **Petal.Width** | 1.33 |    0.20 | 1.00 |   1.30 | 1.80 |

**Group:** Species = virginica  
**N:** 50

|                  | Mean | Std.Dev |  Min | Median |  Max |
| ---------------: | ---: | ------: | ---: | -----: | ---: |
| **Sepal.Length** | 6.59 |    0.64 | 4.90 |   6.50 | 7.90 |
|  **Sepal.Width** | 2.97 |    0.32 | 2.20 |   3.00 | 3.80 |
| **Petal.Length** | 5.55 |    0.55 | 4.50 |   5.55 | 6.90 |
|  **Petal.Width** | 2.03 |    0.27 | 1.40 |   2.00 | 2.50 |

To see an *html* version of these results, we’d simply do this (results
not shown):

``` r
view(iris_stats_by_species)
```

## Special Case - Using descr() With by() For One Variable Only

Instead of showing several tables having one column each, the `view()`
will assemble the results into a single table:

``` r
BMI_by_age <- with(tobacco, 
                   by(BMI, age.gr, descr, 
                      stats = c("mean", "sd", "min", "med", "max")))
view(BMI_by_age, "pander", style = "rmarkdown")
```

### Descriptive Statistics

**BMI, split by age.gr**  
**Data Frame:** tobacco

|             | age.gr = 18-34 | 35-50 | 51-70 |  71 + |
| ----------: | -------------: | ----: | ----: | ----: |
|    **Mean** |          23.84 | 25.11 | 26.91 | 27.45 |
| **Std.Dev** |           4.23 |  4.34 |  4.26 |  4.37 |
|     **Min** |           8.83 | 10.35 |  9.01 | 16.36 |
|  **Median** |          24.04 | 25.11 | 26.77 | 27.52 |
|     **Max** |          34.84 | 39.44 | 39.21 | 38.37 |

…and the transposed version would look like this:

``` r
BMI_by_age <- with(tobacco, 
                   by(BMI, age.gr, descr, 
                      stats = c("mean", "sd", "min", "med", "max")))
view(BMI_by_age, "pander", style = "rmarkdown", omit.headings = TRUE)
```

|             | age.gr = 18-34 | 35-50 | 51-70 |  71 + |
| ----------: | -------------: | ----: | ----: | ----: |
|    **Mean** |          23.84 | 25.11 | 26.91 | 27.45 |
| **Std.Dev** |           4.23 |  4.34 |  4.26 |  4.37 |
|     **Min** |           8.83 | 10.35 |  9.01 | 16.36 |
|  **Median** |          24.04 | 25.11 | 26.77 | 27.52 |
|     **Max** |          34.84 | 39.44 | 39.21 | 38.37 |

## Using lapply() to Show Several freq() tables at once

As is the case for `by()`, the `view()` function is useful in making
results nice and tidy.

``` r
tobacco_subset <- tobacco[ ,c("gender", "age.gr", "smoker")]
freq_tables <- lapply(tobacco_subset, freq)
view(freq_tables, method = "pander", style = "rmarkdown")
```

## Frequencies

**Type:** Factor (unordered)

|            | Freq | % Valid | % Valid Cum. | % Total | % Total Cum. |
| ---------: | ---: | ------: | -----------: | ------: | -----------: |
|      **F** |  489 |   50.00 |        50.00 |   48.90 |        48.90 |
|      **M** |  489 |   50.00 |       100.00 |   48.90 |        97.80 |
| **\<NA\>** |   22 |         |              |    2.20 |       100.00 |
|  **Total** | 1000 |  100.00 |       100.00 |  100.00 |       100.00 |

**Type:** Factor (unordered)

|            | Freq | % Valid | % Valid Cum. | % Total | % Total Cum. |
| ---------: | ---: | ------: | -----------: | ------: | -----------: |
|  **18-34** |  258 |   26.46 |        26.46 |   25.80 |        25.80 |
|  **35-50** |  241 |   24.72 |        51.18 |   24.10 |        49.90 |
|  **51-70** |  317 |   32.51 |        83.69 |   31.70 |        81.60 |
|   **71 +** |  159 |   16.31 |       100.00 |   15.90 |        97.50 |
| **\<NA\>** |   25 |         |              |    2.50 |       100.00 |
|  **Total** | 1000 |  100.00 |       100.00 |  100.00 |       100.00 |

**Type:** Factor (unordered)

|            | Freq | % Valid | % Valid Cum. | % Total | % Total Cum. |
| ---------: | ---: | ------: | -----------: | ------: | -----------: |
|    **Yes** |  298 |   29.80 |        29.80 |   29.80 |        29.80 |
|     **No** |  702 |   70.20 |       100.00 |   70.20 |       100.00 |
| **\<NA\>** |    0 |         |              |    0.00 |       100.00 |
|  **Total** | 1000 |  100.00 |       100.00 |  100.00 |       100.00 |

## Writing to files

The console will always tell you the location of the temporary *html*
file that is created in the process. However, you can specify the name
and location of that file explicitly if you need to reuse it later on:

``` r
view(iris_stats_by_species, file = "~/iris_stats_by_species.html")
```

There is also an `append =` boolean parameter for adding content to
existing reports.

## Global options

Version 0.8.3 introduced the following set of global options:

  - `round.digits` = `2`
  - `plain.ascii` = `TRUE`
  - `omit.headings` = `FALSE` (if using in a markdown document or a
    shiny app, setting this to `TRUE` might be preferable
  - `footnote` = `'default'` (set to empty string or `NA` to omit
    footnote)
  - `display.labels` = `TRUE`
  - `freq.totals` = `TRUE`
  - `freq.display.nas` = `TRUE`
  - `ctable.totals` = `TRUE`
  - `ctable.prop` = `'r'` (display *r*ow proportions by default)
  - `descr.stats` = `'all'`
  - `descr.transpose` = `FALSE`
  - `bootstrap.css` = `TRUE` (if using in a markdown document or a shiny
    app, setting this to `FALSE` might be preferable
  - `custom.css` = `NA`
  - `escape.pipe` = `FALSE`

Examples:

``` r
st_options()                      # display all global options' values
st_options('round.digits')        # display only one option
st_options('omit.headings', TRUE) # change an option's value
```

## Bootstrap CSS

Version 0.8.0 of *summarytools* uses *RStudio*’s [htmltools
package](http://cran.r-project.org/web/packages/htmltools/index.html)
and version 4 of [Bootstrap](http://getbootstrap.com/)’s cascading
stylesheets.

It is also possible to include your own *css* if you wish to customize
the look of the output tables. See the documentation for the package’s
`print.summarytools()` function for details.

## Overriding formatting attributes

When a *summarytools* object is stored, its formatting attributes are
stored with it. However, you can override most of them when using the
`print()` and `view()`
functions.

Example:

``` r
age_stats <- freq(tobacco$age.gr)  # age_stats contains a regular output for freq including 
                                   # headings, NA reporting and a Totals row.
print(age_stats, style = "rmarkdown", report.nas = FALSE, 
                 totals = FALSE, omit.headings = TRUE)
```

|           | Freq |     % | % Cum. |
| --------: | ---: | ----: | -----: |
| **18-34** |  258 | 26.46 |  26.46 |
| **35-50** |  241 | 24.72 |  51.18 |
| **51-70** |  317 | 32.51 |  83.69 |
|  **71 +** |  159 | 16.31 | 100.00 |

### Order of Priority of Options / Parameters

1.  Options over-ridden explicitly with `print()` or `view()` have
    precendence
2.  options specified as explicit arguments to `freq() / ctable() /
    descr() / dfSummary()` come second
3.  Global options, which can be set with `st_options`, come third

# News

  - To support shiny apps, it is now possible to adjust the size of the
    graphs in `dfSummary()`, as well as omit the core Bootstrap CSS from
    the outputs.
  - *summarytools* now has **global options** (see `?st_options`)
  - `dfSummary()` now supports Date / POSIX data
  - in `descr()`, Q1 and Q3 are now included. Also, the order of the
    statistics specified with `stats =` is retained for the output.

## Final notes

The package comes with no guarantees. It is a work in progress and
feedback / feature requests are welcome. Just send me an email
(dominic.comtois (at) gmail.com), or open an
[Issue](https://github.com/dcomtois/summarytools/issues) if you find a
bug.

Also, the package grew significantly larger, and maintaining it all by
myself is time consuming. If you would like to contribute, please get in
touch, I’d greatly appreciate the help.
