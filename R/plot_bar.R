#' @title Illustrate bar chart of treatment ranking metrics
#'
#' @author Enoch Kang
#'
#' @description
#' **PlotBar()** is a function for illustrating bar chart in both separated and
#' accumulative styles.
#'
#' @param data DATA of metrics for treatment ranking.
#' @param accum LOGIC value for indicating whether use accumulative probabilities.
#'        This parameter is only for probabilities but not global metrics of
#'        treatment ranking.
#' @param merge LOGIC value for indicating whether merge bar charts together.
#' @param color LIST of colors for treatments in a network meta-analysis.
#'
#' @return
#' **PlotBar()** returns a bar chart.
#'
#' @references
#' Van Valkenhoef, G., Tervonen, T., Zwinkels, T., De Brock, B., &
#' Hillege, H. (2013). ADDIS: a decision support system for evidence-based
#' medicine. **Decision Support Systems**, *55(2)*, 459-475.
#'
#' @seealso \code{\link{GetMetrics}} \code{\link{SetMetrics}}
#'          \code{\link{PlotBeads}} \code{\link{PlotLine}}
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
#' PlotBar(dataRankinma)
#'
#' @export PlotBar

PlotBar <- function(data,
                    accum = NULL,
                    merge = NULL,
                    color = NULL) {

  dataBar <- data$data

  lsMetrics <- c("Probabilities", "P-best", "SUCRA", "P-score")

  argAccum  <- ifelse(is.null(accum), FALSE,
                      ifelse(accum == FALSE, FALSE,
                             ifelse(accum == TRUE, TRUE,
                                    "ERROR")))

  argMerge  <- ifelse(is.null(merge), FALSE,
                      ifelse(merge == FALSE, FALSE,
                             ifelse(merge == TRUE, TRUE,
                                    "ERROR")))

    if (data$metrics.name == "Probabilities") {
      if (argAccum == TRUE) {
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

  lgcInher   <- !inherits(data, "rankinma")

  lgcAccum  <- ifelse(argAccum == TRUE,
                      ifelse(data$metrics.name != "Probabilities",
                             TRUE, FALSE),
                      FALSE)

  lgcMerge  <- ifelse(is.null(merge), FALSE,
                      ifelse(argMerge == TRUE, FALSE,
                             ifelse(argMerge == FALSE, FALSE,
                                    TRUE)))

  lgcColor <- ifelse(is.null(color), FALSE,
                     length(color) != data$n.tx)

  cat("Check arguments:\n")

  if (lgcInher) {
    cat(paste(" Inherit -------------------------------------------------- X\n",
              ' REQUIRE: Argument "data" must be an object of class \"rankinma\".'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Inherit -------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (lgcAccum) {
    cat(paste(" Accumulative --------------------------------------------- !\n",
              ' INFORM: Accumulative line chart is available for metrics
        "Probabilities" only, and *rankinma* is producing bar charts
        with default argument in terms of `accum = FALSE` now.'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Accumulative --------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (lgcMerge) {
    cat(paste(" Merge ---------------------------------------------------- !\n",
              ' INFORM: Argument "merge" should be logit value, and *rankinma*
        is producing bar charts with default argument in terms of `merge = FALSE`
        now.'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Merge ---------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (isFALSE(is.null(color))) {
    if (length(color) != data$n.tx) {
      cat(paste(" Color ---------------------------------------------------- X\n",
                ' REQUIRE: Argument of "color" must list colors for
              **EACH TREATMENT** or **EACH POSSIBLE RANK**.'),
          fill = TRUE, sep = "")
    } else {
      cat(paste(" Color ---------------------------------------------------- V"),
          fill = TRUE, sep = "")
    }
  }

  if (lgcInher | lgcColor)
    stop("Correct above mentioned problem(s).")

  txs      <- unique(dataBar$tx)
  outcomes <- unique(dataBar$outcome)

  argPlotRow <- ceiling(data$n.outcome/3)
  argPlotClm <- ifelse(data$n.outcome < 3,
                       data$n.outcome, 3)

  if (data$metrics.name == "Probabilities") {

    par(mar = c(5, 5, 5, 7), xpd = TRUE)
    for (outcome.i in c(1:length(outcomes))) {

      dataBarPlot <- dataBar[dataBar$outcomes == outcome.i, ]

      barplot(as.matrix(dataBarPlot[, c(7:(length(unique(dataBarPlot$tx))+6))]),
              cex.names = 0.8,
              cex.axis = 0.8,
              xlim = c(0, length(unique(dataBarPlot$tx))),
              ylim = c(0, 1),
              names.arg = dataBarPlot$tx,
              yaxt = "n", ylab = "Probability", xlab = "Treatment",
              col = if (is.null(argColor)) {
                c(dataBarPlot$color.tx)
              } else {
                argColor
              },
              width = ifelse(argAccum == TRUE,
                             0.8,
                             (1/length(unique(dataBarPlot$tx)))
                             ),
              beside = ifelse(argAccum == TRUE, FALSE, TRUE),
              border = FALSE, legend = FALSE, frame.plot = FALSE)
      axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
      title(paste("Accumulative bar chart of rank probability on ",
                  data$ls.outcome[outcome.i], sep = ""),
            cex.main = 1, font = 1, line = 0.5)

      segments(rep(data$n.tx+0.2, nrow(dataBarPlot)),
               (0.95) / nrow(dataBarPlot) * dataBarPlot$txs,
               rep(data$n.tx+0.3, nrow(dataBarPlot)),
               (0.95) / nrow(dataBarPlot) * dataBarPlot$txs,
               lwd = 10,
               col = if (is.null(argColor)) {
                 c(dataBarPlot$color.tx)
               } else {
                 argColor}
      )

      text(rep(data$n.tx+0.4, nrow(dataBarPlot)),
           (0.95) / nrow(dataBarPlot) * dataBarPlot$txs,
           paste("Rank ", dataBarPlot$rank, sep = ""),
           pos = 4,
           cex = 0.8)

      plotTemp <- grDevices::recordPlot()
      plotBar  <- plotTemp
    }
  } else {

    for (outcome.i in c(1:length(outcomes))) {

      dataBarPlot <- dataBar[dataBar$outcomes == outcome.i, ]

      barplot(dataBarPlot[, "metrics"],
              cex.names = 0.8,
              cex.axis = 0.8,
              xlim = c(0, length(unique(dataBarPlot$tx))),
              ylim = c(0, 1),
              names.arg = dataBarPlot$tx,
              yaxt = "n", ylab = data$metrics.name, xlab = "Treatment",
              col = ifelse(is.null(argColor), "dodgerblue3", argColor),
              width = 0.8,
              beside = FALSE,
              border = FALSE, legend = FALSE, frame.plot = FALSE)
      axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
      title(paste("Bar chart of ", data$metrics.name, " on ",
                  data$ls.outcome[outcome.i], sep = ""),
            cex.main = 1, font = 1, line = 0.5)
    }
  }

  if (argMerge == TRUE) {
    par(mfrow = c(argPlotRow, argPlotClm),
        oma = c(4,4,4,0.5),
        mai = c(0.3,0.3,0.3,0.3))
    for (i.otcm in c(1:length(unique(dataBar$outcome)))) {
      par(mar = c(2, 4, 2, 4), xpd = TRUE)
      barplot(dataBar[dataBar$outcome == unique(dataBar$outcome)[i.otcm], "metrics"],
              cex.names = 0.8,
              cex.axis = 0.8,
              xlim = c(0, length(unique(dataBar[dataBar$outcome == unique(dataBar$outcome)[i.otcm], "tx"]))),
              ylim = c(0, 1),
              names.arg = dataBar[dataBar$outcome == unique(dataBar$outcome)[i.otcm], "tx"],
              yaxt = "n",
              col = c(rgb(dataBar$color.r, dataBar$color.g, dataBar$color.b, 0.8)),
              width = 1/length(unique(dataBar$tx)),
              legend = FALSE, beside = FALSE, border = FALSE)
      axis(side = 2, cex.axis = 0.8, at = seq(0, 1, by = 0.1), las = 1)
      title(data$ls.outcome[i.otcm], cex.main = 0.8, font = 1, line = 0.5)
    }
    mtext(paste("Bar chart of " ,
                ifelse(data$metrics.name %in% lsMetrics,
                       data$metrics.name, "???"),
                sep = ""),
          side = 3, font = 2, line = -0.5, outer = TRUE)
    mtext(ifelse(data$metrics.name %in% lsMetrics,data$metrics.name, "???"),
          side = 2, font = 1, line = 1, outer = TRUE)
    mtext("Treatment",
          side = 1, font = 1, line = 1, outer = TRUE)
    par(mfrow=c(1,1))
  }

}
