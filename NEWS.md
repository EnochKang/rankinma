# Change of package `rankinma`


## rankinma 0.1.9
### Date: August 31, 2023

**Modify document:** improve *rankinma* package README file.
\

### Date: August 20, 2023

**Modify document:** improve *rankinma* package manual by detailing returned values from function `SetMetrics()`.
\

### Date: August 08, 2023

**Modify document:** improve *rankinma* package manual by displaying description and details.
\

**Modify function:** improve function `GetMetrics()` in order to get SUCRA consistently and reproducibly.
\

### Date: July 31, 2023

**Modify function:** improve function `PlotBeads()` with newly added parameter `szPnt`, `szFntTtl`, `szFntTtlX`, `szFntX`, `szFntY`, and `szFntLgnd` for allowing user to change size of beads, font size of main title for beading plot, font size of title on X-axis, font size of numeric scale on X-axis, font size of outcome name(s), and legend font size. Minor modification was also done for sorting legend by alphabet.
\

**Modify function:** improve function `PlotHeat()` with a newly added parameter `szFntY` for allowing user to change font size of outcome name(s).
\

### Date: July 30, 2023

**Modify function:** improve function `PlotLine()` and `PlotBar()` to illustrate line chart or bar chart according to descending order of global metrics of treatment ranking on single outcome.
\

**Modify function:** improve function `PlotLine()`, `PlotBar()`, and `PlotHeat()` with a newly added parameter `rotateX` in order to rotate labels on x-axis.
\



## rankinma 0.1.8
### Date: July 03, 2023

**Update to CRAN**  
\

**Modify function:** remove routinely reported processing information from all functions in the *rankinma* package. The information only appear when users use function(s) inappropriately.
\

**Modify function:** improve function `PlotLine()` in order to produce cumulative line charts, and merged line chart.
\

**Modify function:** improve function `PlotLine()` in order to produce cumulative line charts, and merged line chart.
\

### Date: June 23, 2023

**Modify function:** improve data setting in function `SetMetrics()` for scatter plot.
\

**Modify function:** improve data setting in both `GetMetrics()` and `SetMetrics()` functions for cumulative probability of rank.
\


## rankinma 0.1.7
### Date: April 23, 2023

**Update to CRAN**  
\

### Date: April 22, 2023

**Modify function:** add transparency in both functions of `SetMetrics()` and `PlotBeads()`.
\

**Modify function:** modify function `ShowColor()` based on the updated function `SetMetrics()` on April 20, 2023.
\

**Modify function:** improve function `PlotBar()`, `PlotLine()`, and `PlotSpie()` to show user-defined colors appropriately.
\

**Modify function:** improve function `PlotHeat()` to show heat plot with order of metrics or treatment appropriately.
\


## rankinma 0.1.6
### Date: April 20, 2023

**Add function:** `PlotSpie()` is added to the *rankinma* in order to illustrate Spie plot of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\

**Modify function:** improve data setting and color in both functions of `SetMetrics()` and `PlotBeads()`.
\

**Update to CRAN**  
\


## rankinma 0.1.5
### Date: April 4, 2023

**Modify function:** keep user's options of par (*'par(no.readonly)'*) after illustrating plots with *rankinma* by using function `on.exit()`.
\

**Re-submit to CRAN**  
\

### Date: April 1, 2023

**Add function:** `PlotHeat()` is added to the *rankinma* in order to illustrate heat plot of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\


## rankinma 0.1.4
### Date: March 27, 2023

**Modify function:** remove global assignment (*'<<-'*) from package.
\

**Submit to CRAN**  
\

### Date: February 19, 2023

**Push to github**  
\

### Date: February 10, 2023

**Modify function:** all functions were modified due to warnings regarding *... invisible binding for '<<-' assignment ...* .
\


## rankinma 0.1.3
### Date: February 06, 2023

**Add function:** `PlotLine()` is added to the *rankinma* in order to illustrate line chart of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\


## rankinma 0.1.2
### Date: January 31, 2023

**Add function:** `PlotBar()` is added to the *rankinma* in order to illustrate bar chart of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\


## rankinma 0.1.1 
### Date: January 30, 2023

**Add function:** `GetMetrics()` is added to the *rankinma* in order to get metrics of treatment ranking from object of class *netmeta*.
\


## rankinma 0.1.0
### Date: January 29, 2023

Build rankinma with first two functions including `SetMetrics()` and `PlotBeads()`.

> 1. `SetMetrics()` is to set imported data in *rankinma* format.
>
> 2. `PlotBeads()` is to illustrate beading plot using object of class *rankinma* set by function `SetMetrics()`.
>
