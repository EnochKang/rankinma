<img src="https://github.com/EnochKang/rankinma/blob/main/vignettes/rankinma_logo.png?raw=true" width="15%" />

# *rankinma*: Ranking in network meta-analysis

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/rankinma)](https://cran.r-project.org/package=rankinma)
[![License: GPL (>=3)](https://img.shields.io/badge/license-GPL-blue)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Monthly
Downloads](http://cranlogs.r-pkg.org/badges/rankinma)](http://cranlogs.r-pkg.org/badges/rankinma)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rankinma)](https://cran.r-project.org/package=rankinma)

Package *rankinma* supports users to easily obtain and visualize various
metrics of treatment ranking from network meta-analysis without major
concerns regarding heterogeneity, intransitivity, and incoherence. This
package not only accepts manual-prepared data set of treatment ranking
metrics from users, but also can help users to gather various treatment
ranking metrics in network meta-analysis no matter using frequentist or
Bayesian approach. *rankinma* has been formally released on the CRAN, 
and will be updated directly on the CRAN. Users can install *rankinma* 
in R using following syntax:

    install.packages("rankinma")


## Authors

[Chiefeng Chen](https://orcid.org/0000-0002-1595-6553),
[Enoch Kang](https://orcid.org/0000-0002-4903-942X),
[Wen-Hsuan Hou](https://orcid.org/0000-0002-4376-6298),
[Jin-Hua Chen](https://orcid.org/0000-0002-3130-4125),
[Yu-Chieh Chuang](https://orcid.org/0000-0002-7124-6556), &
[Edwin Chan](https://www.duke-nus.edu.sg/core/about/people-leadership/core-visiting-experts/edwin-chan-shih-yen)

-   [Feature](#features)
-   [Flow and functions](#flow-and-functions)
-   [Usage and examples](#usage-and-examples)
-   [To do list](#to-do-list)


## Features

*rankinma* allows users to visualize various treatment ranking metrics
in network meta-analysis based either common-effect model or
random-effects model no matter using frequentist or Bayesian approach.
The current version includes three common metrics of treatment ranking.

-   **Probabilities:** probabilities of every available treatment on
    each possible rank.
-   **SUCRA:** the value of surface under the cumulative ranking curve
    using Bayesian approach.
-   **P-score:** the value of SUCRA using frequentist approach.

Briefly, *rankinma* can be used for visualization of both detailed
metrics of probabilities and global metrics (i.e. SUCRA and P-score).
Besides, *rankinma* provides users multiple types of plots to illustrate
aforementioned treatment ranking metrics, and current version consists
of five types of plots.

-   **Beading plot:** a novel graphics for displaying global metrics of
    treatment ranking (i.e. SUCRA and P-score) based on numeric line
    plot.
-   **Bar chart:** a classic graphics for most metrics of treatment
    ranking (i.e. probabilities, SUCRA, and P-score), and *rankinma*
    supports two sub-type of bar chart in terms of side-by-side bar
    chart and cumulative bar chart.
-   **Line chart:** a classic graphics for most metrics of treatment
    ranking (i.e. probabilities, SUCRA, and P-score), and *rankinma*
    supports two sub-type of line chart in terms of simple line chart (a
    line on a chart) and composite line chart (multiple lines on a
    chart).
-   **Heat plot:** a new graphics for showing global metrics of
    treatment ranking (i.e. SUCRA and P-score) for each outcome, and
    *rankinma* supports to gather all heat plots of outcomes with
    interests on a plot.
-   **Spie chart:** a new graphics proposed in 2020 for displaying
    multiple global metrics of treatment ranking (i.e. SUCRA and
    P-score) from outcomes with interests by each treatment, and
    *rankinma* supports to place all spie charts on a plot.


## Flow and functions

Users can visualize treatment ranking after network meta-analysis in
five steps, but have to check condition before using *rankinma*.

       **Situation 1:** Users have treatment ranking metrics of
outcome(s).  
       **Situation 2:** Users have data for network meta-analysis of **a
single outcome** but do not get treatment ranking metrics yet.  
       **Situation 3:** Users have data for network meta-analysis of
**various outcomes** but do not get treatment ranking metrics yet.

  

#### For situation 1:

       **Step 1.** Build or load data of treatment ranking metrics.

       **Step 2.** Setup data in *rankinma* format using function
`SetMetrics()`.

       **Step 3.** Visualization using function `PlotBeads()`,
`PlotBar()`, `PlotLine()`, `PlotHeat()`, or `PlotSpie()`.

#### For situation 2:

       **Step 1.** Load data and do network meta-analysis.

       **Step 2.** Get treatment ranking metrics from the network
meta-analysis using function `GetMetrics()`.

       **Step 3.** Setup data in *rankinma* format using function
`SetMetrics()`.

       **Step 4.** Visualization using function `PlotBeads()`,
`PlotBar()`, `PlotLine()`, or `PlotHeat()`.

#### For situation 3:

       **Step 1.** Load data and do network meta-analysis.

       **Step 2.** Get treatment ranking metrics from the network
meta-analysis using function `GetMetrics()`.

       — — — Repeat step 1 and 2 for each outcome, and keep output of
them for the further steps. — — —

  

       **Step 3.** Combine treatment ranking metrics using function
`rbind()` in R *base*.

       **Step 4.** Setup data in *rankinma* format using function
`SetMetrics()`.

       **Step 5.** Visualization using function `PlotBeads()`,
`PlotBar()`, `PlotLine()`, `PlotHeat()`, or `PlotSpie()`.

## Usage and examples

The following steps and syntax demonstrate how user can illustrate a
summary of treatment ranking metrics on various outcomes from network
meta-analysis.

Example 1 for illustrating bar chart when users already have treatment
ranking metrics (e.g. P-score).

> **STEP 1.** Build data
>
>     data <- data.frame(tx = c("A", "B", "C", "A", "B", "C"),
>                        outcome = c("mortality", "mortality", "mortality", "recurrent", "recurrent", "recurrent"), 
>                        SUCRA = c(0.8, 0.7, 0.5, 0.9, 0.5, 0.8))
>
> **STEP 2.** Set data for rankinma
>
>     dataRankinma <- SetMetrics(data, 
>                                tx = tx, 
>                                outcome = outcome, 
>                                metrics = SUCRA, 
>                                metrics.name = "SUCRA")
>
> **STEP 3.** Illustrate bar chart
>
>     PlotBar(data = dataRankinma)
>
> Output:
>
> <img src="https://github.com/EnochKang/rankinma/blob/main/vignettes/Figure%201A.png?raw=true" style="display: block; margin: auto;" />
>
> <img src="https://github.com/EnochKang/rankinma/blob/main/vignettes/Figure%201B.png?raw=true" alt="Figure 1. examples of bar chart for SUCRA on two outcomes."  />
> <p class="caption">
> Figure 1. examples of bar chart for SUCRA on two outcomes.
> </p>

Example 2 for illustrating line chart when users have data for network
meta-analysis of a single outcome but do not get treatment ranking
metrics yet.

> **STEP 1.** Load data
>
>     library(netmeta)
>     data(Senn2013)
>     nmaOutput <- netmeta(TE, 
>                          seTE, 
>                          treat1, 
>                          treat2, 
>                          studlab, 
>                          data = Senn2013, 
>                          sm = "SMD")
>
> **STEP 2.** Get Probabilities
>
>     dataMetrics <- GetMetrics(nmaOutput, 
>                               outcome = "HbA1c.random", 
>                               prefer = "small", 
>                               metrics = "Probabilities", 
>                               model = "random", 
>                               simt = 1000)
>
> **STEP 3.** Set data for rankinma
>
>     dataRankinma <- SetMetrics(dataMetrics, 
>                                tx = tx, 
>                                outcome = outcome, 
>                                metrics.name = "Probabilities")
>
> **STEP 4.** Illustrate line chart
>
>     PlotLine(data = dataRankinma, compo = TRUE)
>
> Output:
>
>     ## Loading 'meta' package (version 6.0-0).
>     ## Type 'help(meta)' for a brief overview.
>     ## Readers of 'Meta-Analysis with R (Use R!)' should install
>     ## older version of 'meta' package: https://tinyurl.com/dt4y5drs
>
>     ## Loading 'netmeta' package (version 2.6-0).
>     ## Type 'help("netmeta-package")' for a brief overview.
>     ## Readers of 'Meta-Analysis with R (Use R!)' should install
>     ## older version of 'netmeta' package: https://tinyurl.com/kyz6wjbb
>
> <img src="https://github.com/EnochKang/rankinma/blob/main/vignettes/Figure%202A.png?raw=true" alt="Figure 2A. an exmaple of composite line chart for probabilities of treatments on each rank."  />
> <p class="caption">
> Figure 2A. an exmaple of composite line chart for probabilities of
> treatments on each rank.
> </p>
>
> or
>
> <img src="https://github.com/EnochKang/rankinma/blob/main/vignettes/Figure%202B.png?raw=true" alt="**Figure 2B**. an example of accumulative bar chart for probabilities of treatments on each rank."  />
> <p class="caption">
> **Figure 2B**. an example of accumulative bar chart for probabilities
> of treatments on each rank.
> </p>

Example 3 for illustrating beading plot when users have data for network
meta-analysis of multiple outcomes but do not get treatment ranking
metrics yet.

> **STEP 1.** Load data
>
>     library(netmeta)
>     data(Senn2013)
>     nmaOutput <- netmeta(TE, 
>                          seTE, 
>                          treat1, 
>                          treat2, 
>                          studlab, 
>                          data = Senn2013, 
>                          sm = "SMD")
>
> **STEP 2.** Get SUCRA
>
>     nmaRandom <- GetMetrics(nmaOutput, 
>                             outcome = "HbA1c.random", 
>                             prefer = "small", 
>                             metrics = "P-score", 
>                             model = "random", 
>                             simt = 1000)
>     nmaCommon <- GetMetrics(nmaOutput, 
>                             outcome = "HbA1c.common", 
>                             prefer = "small", 
>                             metrics = "P-score", 
>                             model = "common", 
>                             simt = 1000)
>
> **STEP 3.** Combine metrics from multiple outcomes
>
>     dataMetrics <- rbind(nmaRandom, nmaCommon)
>
> **STEP 4.** Set data for rankinma
>
>     dataRankinma <- SetMetrics(dataMetrics, 
>                                tx = tx, 
>                                outcome = outcome, 
>                                metrics = P.score,
>                                metrics.name = "P-score")
>
> **STEP 5.** Illustrate beading plot
>
>     PlotBeads(data = dataRankinma)
>
> Output:
>
>     ## Check variables:
>     ##  Inherit -------------------------------------------------- V
>     ##  Outcome -------------------------------------------------- V
>     ##  Prefer  -------------------------------------------------- V
>     ##  Metrics -------------------------------------------------- V
>     ##  Model   -------------------------------------------------- V
>     ## Summary of metrics:
>     ##  Metrics:     P-score 
>     ##  Outcomes:    1 
>     ##  Treatments:  10 
>     ## 
>     ## List of treatments:
>     ##   1 acar
>     ##   2 benf
>     ##   3 metf
>     ##   4 migl
>     ##   5 piog
>     ##   6 plac
>     ##   7 rosi
>     ##   8 sita
>     ##   9 sulf
>     ##   10 vild
>
>     ## Check variables:
>     ##  Inherit -------------------------------------------------- V
>     ##  Outcome -------------------------------------------------- V
>     ##  Prefer  -------------------------------------------------- V
>     ##  Metrics -------------------------------------------------- V
>     ##  Model   -------------------------------------------------- V
>     ## Summary of metrics:
>     ##  Metrics:     P-score 
>     ##  Outcomes:    1 
>     ##  Treatments:  10 
>     ## 
>     ## List of treatments:
>     ##   1 acar
>     ##   2 benf
>     ##   3 metf
>     ##   4 migl
>     ##   5 piog
>     ##   6 plac
>     ##   7 rosi
>     ##   8 sita
>     ##   9 sulf
>     ##   10 vild
>
>     ## Check variables:
>     ##  Treatment -------------------------------------------------- V
>     ##  Outcome   -------------------------------------------------- V
>     ##  Metrics name  ---------------------------------------------- V
>     ##  Metrics   -------------------------------------------------- V
>     ## 
>     ## 
>     ## Summary of metrics:
>     ##  Metrics:  P-score 
>     ##  Outcomes:    2 
>     ##  Treatments:  10 
>     ## 
>     ## List of outcomes:
>     ##   1 HbA1c.random
>     ##   2 HbA1c.common
>     ## List of treatments:
>     ##   1 acar
>     ##   2 benf
>     ##   3 metf
>     ##   4 migl
>     ##   5 piog
>     ##   6 plac
>     ##   7 rosi
>     ##   8 sita
>     ##   9 sulf
>     ##   10 vild
>     ##  Inherit -------------------------------------------------- V
>     ##  Metrics -------------------------------------------------- V
>     ##  Color ---------------------------------------------------- V
>
> <img src="https://github.com/EnochKang/rankinma/blob/main/vignettes/Figure%203.png?raw=true" alt="Figure 3. an example of beading plot for P-score on two outcomes"  />
> <p class="caption">
> Figure 3. an example of beading plot for P-score on two outcomes
> </p>

## To do list

Task force will keep update package *rankinma* for following issues:

-   Merged line plot.
-   User-designed color gradient for heat plot.
-   Scatter plot
