#' @title Illustrate line chart of treatment ranking metrics
#'
#' @author Enoch Kang
#'
#' @description
#' **PlotLine()** is a function for illustrating line chart in both simple
#' and composite styles.
#'
#' @param data DATA of metrics for treatment ranking.
#' @param compo LOGIC value for indicating whether use composite line chart.
#'        This parameter is only for probabilities but not global metrics of
#'        treatment ranking.
#' @param merge LOGIC value for indicating whether merge bar charts together.
#' @param color LIST of colors for treatments in a network meta-analysis.
#'
#' @return
#' **PlotLine()** returns a line chart.
#'
#' @references
#' Chaimani, A., Higgins, J. P., Mavridis, D., Spyridonos, P., &
#' Salanti, G. (2013). Graphical tools for network meta-analysis in STATA.
#' **PloS one**, *8(10)*, e76654.
#'
#' @seealso \code{\link{GetMetrics}} \code{\link{SetMetrics}}
#'          \code{\link{PlotBeads}} \code{\link{PlotBar}}
#'
#' @examples
#' library(netmeta)
#' data(Senn2013)
#' nma <- netmeta(TE, seTE, treat1, treat2,
#'   studlab, data = Senn2013, sm = "SMD")
#'
#' # Get SUCRA
#' dataMetrics <- GetMetrics(nma, outcome = "HbA1c", prefer = "small", metrics = "SUCRA",
#'   model = "random", simt = 1000)
#'
#' # Set data for rankinma
#' dataRankinma <- SetMetrics(dataMetrics, tx = tx, outcome = outcome,
#'   metrics = SUCRA, metrics.name = "SUCRA")
#'
#' # Illustrate bar plot
#' PlotLine(dataRankinma)
#'
#' @export PlotLine

PlotLine <- function(data,
                     compo = NULL,
                     merge = NULL,
                     color = NULL) {

  dataLine <- data$data

  lsMetrics <- c("Probabilities", "P-best", "SUCRA", "P-score")

  argCompo  <- ifelse(is.null(compo), FALSE,
                      ifelse(compo == FALSE, FALSE,
                             ifelse(compo == TRUE, TRUE,
                                    "ERROR")))

  argMerge  <- ifelse(is.null(merge), FALSE,
                      ifelse(merge == FALSE, FALSE,
                             ifelse(merge == TRUE, TRUE,
                                    "ERROR")))

  if (data$metrics.name == "Probabilities") {
    if (argCompo == TRUE) {
      if (isFALSE(is.null(color))) {
        if (length(color) == data$n.tx) {
          argColor  <- color
        } else {
          argColor  <- NULL
        }
      } else {
        argColor  <- NULL
      }
    } else if (length(color) == 1) {
      argColor  <- color
    } else {
      argColor  <- NULL
    }
  } else if (isFALSE(is.null(color))) {
    if (length(color) == 1) {
      argColor  <- color
    } else {
      argColor  <- NULL
    }
  } else {
    argColor  <- NULL
  }


  lgcInher  <- !inherits(data, "rankinma")

  lgcCompo  <- ifelse(argCompo == TRUE,
                      ifelse(data$metrics.name != "Probabilities",
                             TRUE, FALSE),
                      FALSE)

  lgcMerge  <- ifelse(is.null(merge), FALSE,
                      ifelse(argMerge == TRUE, FALSE,
                             ifelse(argMerge == FALSE, FALSE,
                                    TRUE)))

  lgcColor  <- ifelse(is.null(color), FALSE,
                     ifelse(data$metrics.name == "Probabilities",
                            ifelse(argCompo == TRUE,
                                   ifelse(length(color) != data$n.tx, TRUE, FALSE),
                                   ifelse(length(color) != 1, TRUE, FALSE)),
                            ifelse(length(color) != 1, TRUE, FALSE)))

  cat("Check arguments:\n")

  if (lgcInher) {
    cat(paste(" Inherit -------------------------------------------------- X\n",
              ' REQUIRE: Argument "data" must be an object of class \"rankinma\".'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Inherit -------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (lgcCompo) {
    cat(paste(" Composite ------------------------------------------------ !\n",
              ' INFORM: Composite line chart is available for metrics
         "Probabilities" only, and *rankinma* is producing line charts
         with default argument in terms of `compo = FALSE` now.'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Composite ------------------------------------------------ V"),
        fill = TRUE, sep = "")
  }

  if (lgcMerge) {
    cat(paste(" Merge ---------------------------------------------------- !\n",
              ' INFORM: Argument "merge" should be logit value, and *rankinma*
        is producing line charts with default argument in terms of `merge = FALSE`
        now.'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Merge ---------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (data$metrics.name == "Probabilities") {
    if (isFALSE(is.null(color))) {
      if (argCompo == TRUE) {
        if (length(color) != data$n.tx) {
          cat(paste(" Color ---------------------------------------------------- X\n",
                    ' REQUIRE: Argument of "color" must list colors for
              **EACH TREATMENT** when using "Probabilities" as metrics.'),
              fill = TRUE, sep = "")
        } else {
          cat(paste(" Color ---------------------------------------------------- V"),
              fill = TRUE, sep = "")
        }
      } else {
        cat(paste(" Color ---------------------------------------------------- V"),
            fill = TRUE, sep = "")
      }
    } else {
      cat(paste(" Color ---------------------------------------------------- V"),
          fill = TRUE, sep = "")
    }
  } else if (isFALSE(is.null(color))) {
    if (length(color) != 1) {
      cat(paste(" Color ---------------------------------------------------- X\n",
                ' REQUIRE: Argument of "color" must be only single color\n
                when using global metrics (e.g. SUCRA and P-score).'),
          fill = TRUE, sep = "")
    } else {
      cat(paste(" Color ---------------------------------------------------- V"),
          fill = TRUE, sep = "")
    }
  } else {
    cat(paste(" Color ---------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

    if (lgcInher | lgcColor)
      stop("Correct above mentioned problem(s).")

  txs      <- unique(dataLine$tx)
  outcomes <- unique(dataLine$outcome)

  argPlotRow <- ceiling((data$n.outcome)/3)
  argPlotClm <- ifelse((data$n.outcome) < 3,
                       (data$n.outcome), 3)

  if (data$metrics.name == "Probabilities") {

    if (argCompo == TRUE) {
      par(mar = c(5, 5, 5, 7), xpd = TRUE)
      for (outcome.i in c(1:length(outcomes))) {
        dataLinePlot <- dataLine[dataLine$outcomes == outcome.i, ]
      plot(dataLinePlot[, c(7)],
             type = "line",
             cex.names = 0.8,
             cex.axis = 0.8,
             xlim = c(1, max(dataLinePlot$txs)),
             ylim = c(0, 1),
             yaxt = "n", ylab = "Probability", xlab = "Rank",
             col = if (is.null(argColor)) {
               c(dataLinePlot$color.tx[1])
             } else {
               argColor[1]
               },
             legend = FALSE, frame.plot = FALSE)
        axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
        title(paste("Composite line chart of rank probability on ",
                    data$ls.outcome[outcome.i], sep = ""),
              cex.main = 1, font = 1, line = 0.5)

        for (tx.i in c(8:(data$n.tx+6))) {
          lines(dataLinePlot[, c(5)],
                dataLinePlot[, c(tx.i)],
                col = if (is.null(argColor)) {
                  c(dataLinePlot$color.tx[tx.i-6])
                } else {
                  argColor[tx.i-6]
                  }
          )
        }

        segments(rep(data$n.tx+0.2, nrow(dataLinePlot)),
                 (0.95) / nrow(dataLinePlot) * dataLinePlot$txs,
                 rep(data$n.tx+0.3, nrow(dataLinePlot)),
                 (0.95) / nrow(dataLinePlot) * dataLinePlot$txs,
                 lwd = 2,
                 col = if (is.null(argColor)) {
                   c(dataLinePlot$color.tx)
                 } else {
                   argColor}
        )

        text(rep(data$n.tx+0.4, nrow(dataLinePlot)),
             (0.95) / nrow(dataLinePlot) * dataLinePlot$txs,
             dataLinePlot$tx,
             pos = 4,
             cex = 0.8)

        plotTemp <-  grDevices::recordPlot()
        plotLine <<- plotTemp
      }
      par(mar = c(5, 5, 5, 5), xpd = TRUE)

    } else {

      par(mar = c(5, 5, 5, 5), xpd = TRUE)

      for (outcome.i in c(1:length(outcomes))) {
        for (tx.i in c(7:(data$n.tx+6))) {
          dataLinePlot <- dataLine[dataLine$outcomes == outcome.i, c(1:6, tx.i)]

          plot(dataLinePlot[, 7],
               type = "line",
               cex.names = 0.8,
               cex.axis = 0.8,
               xlim = c(1, max(dataLinePlot$txs)),
               ylim = c(0, 1),
               xaxt = "n", yaxt = "n",
               xlab = "Rank", ylab = "Probabilities",
               legend = FALSE,
               frame.plot = FALSE,
               col = ifelse(is.null(argColor), "dodgerblue3", argColor)
          )
          axis(side = 1, cex.axis = 0.8,
               at = c(1:max(dataLinePlot$txs)),
               labels = dataLinePlot$txs)
          axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
          title(paste("Line chart for rank probability of\n",
                      data$ls.tx[tx.i-7], " on ",
                      data$ls.outcome[outcome.i],
                      sep = ""),
                cex.main = 0.8, font = 1, line = 0.5)

          plotTemp <-  grDevices::recordPlot()
        }
      }
    }

  } else {
    par(mar = c(5, 5, 5, 7), xpd = TRUE)

    for (outcome.i in c(1:length(outcomes))) {
      dataLinePlot <- dataLine[dataLine$outcomes == outcome.i, ]
      plot(dataLinePlot[, "metrics"],
           type = "line",
           cex.names = 0.8,
           cex.axis = 0.8,
           xlim = c(1, max(dataLinePlot$txs)),
           ylim = c(0, 1),
           xaxt = "n", xlab = "Treatment",
           yaxt = "n", ylab = data$metrics.name,
           col = ifelse(is.null(argColor), "dodgerblue3", argColor[1]),
           legend = FALSE, frame.plot = FALSE)
      axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
      axis(side = 1, cex.axis = 0.8,
           at = dataLinePlot[, "txs"],
           labels = dataLinePlot[, "tx"])
      title(paste("Line chart of ", data$metrics.name, " on ",
                  outcomes[outcome.i], sep = ""),
            cex.main = 1, font = 1, line = 0.5)

      plotTemp <- grDevices::recordPlot()
      plotLine <- plotTemp
    }
  }

}
