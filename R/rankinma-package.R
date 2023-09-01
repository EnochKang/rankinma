#' rankinma: Ranking in Network Meta-Analysis
#'
#' @description
#' \emph{rankinma} is an R package that supports users to easily obtain and
#' visualize various metrics of treatment ranking from network meta-analysis
#' no matter using either frequentist or Bayesian approach. Development of package
#' \emph{rankinma} is based on \bold{R version 4.2.2 (2022-10-31 ucrt)}.
#' Extra imported packages are as follows:
#'
#' \itemize{
#'  \item \href{https://cran.r-project.org/src/contrib/Archive/mvtnorm/mvtnorm_1.1-3.tar.gz}{\emph{mvtnorm} (version 1.1-3)}
#'  \item \href{https://cran.r-project.org/src/contrib/Archive/netmeta/netmeta_2.6-0.tar.gz}{\emph{netmeta} (version 2.6-0)}
#' }
#'
#'
#' @details
#' Current version consists of seven functions, including two functions for
#' data preparation (function \code{\link{GetMetrics}} and \code{\link{SetMetrics}})
#' and five functions for visualization of treatment ranking metrics (i.e.
#' \code{\link{PlotBeads}}, \code{\link{PlotLine}}, \code{\link{PlotBar}},
#' \code{\link{PlotHeat}}, and \code{\link{PlotSpie}}). Probabilities of treatments
#' on each possible rank can be visualized using \code{\link{PlotLine}} and \code{\link{PlotBar}}.
#' Due to concise information, \code{\link{PlotBeads}} is recommended to be used for
#' global metrics of treatment ranking, such as \bold{P-score} and \bold{SUCRA}. The other four
#' visualization functions can also generate graphics of the global metrics.
#'
#' @name rankinma-package
#'
#' @docType package
#'
#' @keywords package
#'
#'
## usethis namespace: start
#' @importFrom graphics axis
#' @importFrom graphics barplot
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics pie
#' @importFrom graphics points
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rainbow
#' @importFrom grDevices recordPlot
#' @importFrom grDevices rgb
#' @importFrom stats aggregate
#' @importFrom utils capture.output
## usethis namespace: end
NULL
