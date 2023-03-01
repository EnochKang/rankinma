# Change of package `rankinma`

## rankinma 0.1.4

Date: Feb. 19, 2023

**Push to github**  
\

Date: Feb. 10, 2023

**Modify function:** all functions were modified due to warnings regarding *... invisible binding for '<<-' assignment ...* .
\

## rankinma 0.1.3

Date: Feb. 06, 2023

**New function:** `PlotLine()` is planned to be added for illustrate line chart of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\

## rankinma 0.1.2

Date: Jan. 31, 2023

**New function:** `PlotBar()` is added for illustrate bar chart of global metrics of treatment ranking (i.e. SUCRA and P-score) using object of class *rankinma* set by function `SetMetrics()`.
\

## rankinma 0.1.1 

Date: Jan. 30, 2023

**New function:** `GetMetrics()` is added for getting metrics of treatment ranking from object of class *netmeta*.
\

## rankinma 0.1.0

Date: Jan. 29, 2023

Build rankinma with first two functions including `SetMetrics()` and `PlotBeads()`.


> 1. `SetMetrics()` is to set imported data in *rankinma* format.
>
> 2. `PlotBeads()` is to illustrate beading plot using object of class *rankinma* set by function `SetMetrics()`.


### Writing style of *rankinma*

This package is written according to Google's R style. For readers, details of naming rules are listed as follows:
> 1. **.R file** is named using lower case with underscore "_" between words (*e.g. get_metrics.R*). 
> 2. **function** is named using verb or verb with noun, and the first character of each word is written in capital letter (e.g. `GetMetrics()`).
> 3. **object** is named using noun with the first word in lower case, but the first character of rest words is written using capital letter (e.g. `argOutcome`).
> 4. **variable** is named using noun written in lower case. Words of variable name are separated by "." if a variable name consists of more than two words (e.g. `data$metrics.name`).

- Added a `NEWS.md` file to track changes to the package.
