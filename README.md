<!-- readme: start -->
<!-- title: start -->
<br>

<img src = "https://github.com/EnochKang/rankinma/blob/main/vignettes/rankinma_logo.png?raw=true" align = "left" width = "120" />
<br>

# &nbsp; **rankinma** 

#### &nbsp; &nbsp; *Rank in Network Meta-Analysis*

[Chiefeng Chen](https://orcid.org/0000-0002-1595-6553), [Enoch
Kang](https://orcid.org/0000-0002-4903-942X), [Wen-Hsuan
Hou](https://orcid.org/0000-0002-4376-6298), [Jin-Hua
Chen](https://orcid.org/0000-0002-3130-4125), [Yu-Chieh
Chuang](https://orcid.org/0000-0002-7124-6556), & [Edwin
Chan](https://www.duke-nus.edu.sg/core/about/people-leadership/core-visiting-experts/edwin-chan-shih-yen)
<br>
<br>

<!-- title: end -->
<!-- badges: start -->

&nbsp; [![CRAN](https://img.shields.io/cran/v/rankinma?color=blue&label=CRAN&logo=r&logoColor=skyblue)](https://cran.r-project.org/package=rankinma)
&nbsp; [![Update date](https://img.shields.io/badge/Update%20date-2023.09.01-blue.svg?logo=r&logoColor=skyblue)](https://cran.r-project.org/web/packages/rankinma/news/news.html)
&nbsp; [![Shiny](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=gray25&logo=RStudio&logoColor=skyblue)](https://rankinma.shinyapps.io/rankinma/)
&nbsp; [![Licence](https://img.shields.io/badge/licence-GPL--3-brightgreen.svg?color=blue&label=Licence&logo=gnu&logoColor=skyblue)](https://www.gnu.org/licenses/gpl-3.0.en.html)

&nbsp; [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg?color=green&label=Lifecycle&logo=r&logoColor=green)](https://lifecycle.r-lib.org/articles/stages.html#stable)
&nbsp; [![Functions](https://img.shields.io/badge/Functions-7-green.svg?logo=r&logoColor=green)](https://cran.r-project.org/web/packages/rankinma/rankinma.pdf)
&nbsp; [![Dependencis](https://tinyverse.netlify.com/badge/rankinma)](https://cran.r-project.org/package=rankinma)
&nbsp; [![Monthly Downloads](https://cranlogs.r-pkg.org:443/badges/rankinma?color=orange)](https://cranlogs.r-pkg.org:443/badges/rankinma)
&nbsp; [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/aides?color=orange)](https://cran.r-project.org/package=rankinma)

<!-- badges: end -->
<!-- content: start -->
<br>

-   [About rankinma](#about-rankinma)
-   [Feature](#features)
-   [Installation](#installation)
-   [Examples](#examples)
-   [Coding conventions](#coding-conventions)
-   [License](#license)
-   [To do list](#to-do-list)

<!-- content: end -->
<!-- about: start -->
<br>

## About *rankinma*

Package *rankinma* supports users to easily obtain and visualize various metrics of treatment ranking from network meta-analysis without major concerns regarding heterogeneity, intransitivity, and incoherence. This package not only accepts manual-prepared data set of treatment ranking metrics from users, but also can help users to gather various treatment ranking metrics in network meta-analysis. 

<!-- content: end -->
<!-- features: start -->
<br>


## Feature

*rankinma* allows users to visualize various treatment ranking metrics in network meta-analysis based either common-effect model or random-effects model no matter using frequentist or Bayesian approach. The current version includes three common metrics of treatment ranking.

-   **Probabilities:** probabilities of every available treatment on each possible rank.
-   **SUCRA:** the value of surface under the cumulative ranking curve using Bayesian approach.
-   **P-score:** the value of SUCRA using frequentist approach.

<br>

Briefly, *rankinma* can be used for visualization of both detailed metrics of probabilities and global metrics (i.e. SUCRA and P-score). Besides, *rankinma* provides users multiple types of plots to illustrate aforementioned treatment ranking metrics, and current version consists of five types of plots with six sub-types.

-   **Beading plot:** a novel graphics for displaying global metrics of treatment ranking (i.e. SUCRA and P-score) based on numeric line plot.
-   **Bar chart:** a classic graphics for most metrics of treatment ranking (i.e. probabilities, SUCRA, and P-score), and *rankinma* supports two sub-type of bar chart in terms of side-by-side bar chart and cumulative bar chart.
-   **Line chart:** a classic graphics for most metrics of treatment ranking (i.e. probabilities, SUCRA, and P-score), and *rankinma* supports two sub-type of line chart in terms of simple line chart (a line on a chart) and composite line chart (multiple lines on a chart).
-   **Heat plot:** a new graphics for showing global metrics of treatment ranking (i.e. SUCRA and P-score) for each outcome, and *rankinma* supports to gather all heat plots of outcomes with interests on a plot.
-   **Spie chart:** a new graphics proposed in 2020 for displaying multiple global metrics of treatment ranking (i.e. SUCRA and P-score) from outcomes with interests by each treatment, and *rankinma* supports to place all spie charts on a plot.

<!-- features: end -->
<!-- installation: start -->
<br>


## Installation

Package *rankinma* has been formally released on the [CRAN](https://cran.r-project.org/web/packages/rankinma/index.html), and users can install *rankinma* in R using following syntax:

```{r}
install.packages("rankinma")
```

<br>

If users have tabulated global metrics of treatment ranking in a csv file, they can illustrate beading plot without installation of package *rankinma* by using online platform [**RankiNMA** (click here)](https://rankinma.shinyapps.io/rankinma/).
    
<!-- installation: end -->
<!-- examples: start -->
<br>


## Examples

### Example 1

To depict global metrics of treatment ranking using bar chart:

<p align = "center" width = "100%">
<img src = "https://github.com/EnochKang/rankinma/blob/main/vignettes/Figure%201A.png?raw=true">
</p>
<br>

### Example 2

To depict probabilities of treatments on each possible rank using multi-line chart:

<p align = "center" width = "100%">
<img src = "https://github.com/EnochKang/rankinma/blob/main/vignettes/Figure%202A.png?raw=true">
</p>
<br>

### Example 3

To depict probabilities of treatments on each possible rank using stacked bar chart:

<p align = "center" width = "100%">
<img src = "https://github.com/EnochKang/rankinma/blob/main/vignettes/Figure%202B.png?raw=true">
</p>
<br>

### Example 4 

To depict global metrics of treatment ranking using beading plot:

<p align = "center" width = "100%">
<img src = "https://github.com/EnochKang/rankinma/blob/main/vignettes/Figure%203.png?raw=true">
</p>
<br>
    
<!-- examples: end -->
<!-- coding conventions: start -->
<br>


## Coding conventions

This package is mainly written according to [Google's R style](https://web.stanford.edu/class/cs109l/unrestricted/resources/google-style.html). For readers, details of naming rules are listed as follows:

1. **.R file** is named using lower case with underscore "_" between words (*e.g. get_metrics.R*). 

2. **function** is named using verb or verb with noun, and the first character of each word is written in capital letter (e.g. `GetMetrics()`).

3. **object** is named using noun with the first word in lower case, but the first character of rest words is written using capital letter (e.g. `dataBeads`).

4. **variable** is named using noun written in lower case. Words of variable name are separated by "." if a variable name consists of more than two words (e.g. `dataRankinma$metrics.name`).

<br>


Version number consists of three integers with a period between them (eg. version 1.0.0).

1. Updating the first integer refers to a modification with new methodological impact. 

2. Changing the second integer refers to an update with a new function without new methodological impact. 

3. Updating the third integer refers to a modification in a function.

<!-- coding conventions: end -->
<!-- license: start -->
<br>


## License

This package is licensed under the [GPL-3 License](https://cran.r-project.org/web/licenses/GPL-3).

<!-- license: end -->
<!-- to to list: start -->
<br>


## To do list

Task force will keep update package *rankinma* for following issues:

-   Scatter plot
-   User-designed color gradient for heat plot.

<!-- to to list: end -->
