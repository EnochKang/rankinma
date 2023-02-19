#' @title Setup data of treatment ranking metrics for rankinma use
#'
#' @author Enoch Kang
#'
#' @description
#' **SetMetrics()** is a function for checking and preparing data set of metrics
#' for further ploting in *rankinma*.
#'
#' @param data DATAFRAME of treatment, metrics, and name of outcomes.
#' @param outcome VARIABLE string data for of outcome(s).
#' @param tx VARIABLE with string data for treatments.
#' @param metrics VARIABLE with numeric data for global metrics, but it should
#'        be "NULL" when using "Probabilities" as metrics.
#' @param metrics.name STRING for metrics of treatment ranking in terms of
#'        "SUCRA" and "P-score" for the value of surface under the cumulative
#'        ranking curve or P-score.
#'
#' @return
#' **SetMetrics()** returns a confirmed data.frame of treatment, metrics of
#' treatment ranking, and outcome name.
#'
#' @seealso \code{\link{GetMetrics}}
#'
#' @examples
#' library(netmeta)
#' data(Senn2013)
#' nma <- netmeta(TE, seTE, treat1, treat2,
#'   studlab, data = Senn2013, sm = "SMD")
#'
#' # Get SUCRA
#' nma.1 <- GetMetrics(nma, outcome = "HbA1c.random", prefer = "small", metrics = "SUCRA",
#'   model = "random", simt = 1000)
#' nma.2 <- GetMetrics(nma, outcome = "HbA1c.common", prefer = "small", metrics = "SUCRA",
#'   model = "common", simt = 1000)
#'
#' # Combine metrics of multiple outcomes
#' dataMetrics <- rbind(nma.1, nma.2)
#'
#' # Set data for rankinma
#' dataRankinma <- SetMetrics(dataMetrics, tx = tx, outcome = outcome,
#'   metrics = SUCRA, metrics.name = "SUCRA")
#'
#' @export SetMetrics

SetMetrics <- function(data,
                       outcome = NULL,
                       tx = NULL,
                       metrics = NULL,
                       metrics.name = NULL) {

  data <- data.frame(data)
  lsMetrics <- c("Probabilities", "P-best", "SUCRA", "P-score")

  namTx      <- deparse(substitute(tx))
  namMetrics <- deparse(substitute(metrics))
  namOutcome <- deparse(substitute(outcome))

  lgcTx      <- !(namTx %in% colnames(data))
  lgcOutcome <- !(namOutcome %in% colnames(data))
  lgcMetrics <- ifelse(metrics.name == "Probabilities",
                       FALSE,
                       !(namMetrics %in% colnames(data)))
  lgcDupl    <- ifelse(sum(table(data$tx, data$outcome)) == sum(table(data$tx, data$outcome) == 1),
                       FALSE, TRUE)

  cat("Check variables:\n")

  if (!(namTx %in% colnames(data))) {
    cat(paste(" Treatment -------------------------------------------------- X\n",
              ' REQUIRE: Argument "tx" must be specified.\n'),
        fill = TRUE, sep = "")
  } else {
    colnames(data)[which(colnames(data) == namTx)] <- "tx"
    cat(paste(" Treatment -------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (!(namOutcome %in% colnames(data))) {
    cat(paste(" Outcome   -------------------------------------------------- X\n",
              ' REQUIRE: Argument "outcome" must be specified.'),
        fill = TRUE, sep = "")
  } else {
    colnames(data)[which(colnames(data) == namOutcome)] <- "outcome"
  }

  if (isFALSE(sum(table(data$tx, data$outcome)) == sum(table(data$tx, data$outcome) == 1))) {
    cat(paste(" Outcome   -------------------------------------------------- X\n",
              ' REQUIRE: Duplicates of treatment in a same outcome.'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Outcome   -------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (is.null(metrics.name)) {
    cat(paste(" Metrics name  ---------------------------------------------- X\n",
              ' REQUIRE: Argument "metrics.name" must be "Probabilities,"
              "SUCRA," or "P-score."'),
        fill = TRUE, sep = "")
  } else if (!(metrics.name %in% lsMetrics)) {
    cat(paste(" Metrics name  ---------------------------------------------- X\n",
              ' REQUIRE: Argument "metrics.name" must be "Probabilities,"
              "SUCRA," or "P-score."'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Metrics name  ---------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (metrics.name != "Probabilities") {
    if (!(namMetrics %in% colnames(data))) {
      cat(paste(" Metrics   -------------------------------------------------- X\n",
                ' REQUIRE: Argument "metrics" must be specified for SUCRA or P-score.'),
          fill = TRUE, sep = "")
    } else {
      colnames(data)[which(colnames(data) == namMetrics)] <- "metrics"
      if (!is.numeric(data$metrics)) {
        cat(paste(" Metrics   -------------------------------------------------- X\n",
                  ' REQUIRE: Argument "metrics" must be numeric data for SUCRA
                  or P-score.'),
            fill = TRUE, sep = "")
      } else {
        cat(paste(" Metrics   -------------------------------------------------- V"),
            fill = TRUE, sep = "")
      }
    }
  } else if (isFALSE(is.null(namMetrics))) {
    cat(paste(" Metrics   -------------------------------------------------- !\n",
              ' INFORM: Argument "metrics" is not mandatory for "Probabilities,
              and *rankinma* ignores your argument "', namMetrics,  '".',
              sep = ""),
        fill = TRUE, sep = "")
  }

  if (metrics.name == "Probabilities") {
    if (lgcTx |
        lgcOutcome |
        !(metrics.name %in% lsMetrics) |
        lgcMetrics |
        lgcDupl) {
      lgcOverall <- TRUE
    } else {
      lgcOverall <- FALSE
    }
  } else if (lgcTx |
             lgcOutcome |
             !(metrics.name %in% lsMetrics) |
             lgcMetrics |
             !is.numeric(data$metrics) |
             lgcDupl) {
    lgcOverall <- TRUE
  } else {
    lgcOverall <- FALSE
  }

  if (lgcOverall)
    stop("Correct above mentioned problem(s).")

  txs      <- unique(data$tx)
  outcomes <- unique(data$outcome)

  for (i in c(1:nrow(data))) {
    data[i, "txs"] <- which(as.character(txs) == as.character(data[i, "tx"]))
  }
  for (i in seq(1, nrow(data))) {
    data[i, "outcomes"] <- which(as.character(outcomes) == as.character(data[i, "outcome"]))
  }

  data$importance <- max(data$outcomes) + 1 - data$outcomes

  data$color.r <- 0.2
  data$color.g <- 0.2
  data$color.b <- 0.2

  for (i in seq(1, nrow(data))){
    j <- c(1, 4:5); k <- c(6, 9:10); l <- c(11, 14:15); m <- c(16, 19:20); n <- c(21, 24:25)
    data[i, "color.r"] <-ifelse(data[i, "txs"] %in% c(j, j + 25, j + 50, j + 75), 1,
                                   ifelse(data[i, "txs"] %in% c(k, k + 25, k + 50, k + 75), 0.80,
                                          ifelse(data[i, "txs"] %in% c(l, l + 25, l + 50, l + 75), 0.50,
                                                 ifelse(data[i, "txs"] %in% c(m, m + 25, m + 50, m + 75), 0.65,
                                                        ifelse(data[i, "txs"] %in% c(n, n + 25, n + 50, n + 75), 0.55,
                                                               data[i, "color.r"])))))
  }

  for (i in seq(1, nrow(data))){
    j<- c(2,4);k<- c(7,9);l<- c(12,14);m<- c(17,19);n<- c(22,24)
    data[i, "color.g"] <-ifelse(data[i, "txs"] %in% c(j, j + 25, j + 50, j + 75), 1,
                                   ifelse(data[i, "txs"] %in% c(k, k + 25, k + 50, k + 75), 0.65,
                                          ifelse(data[i, "txs"] %in% c(l, l + 25, l + 50, l + 75), 0.80,
                                                 ifelse(data[i, "txs"] %in% c(m, m + 25, m + 50, m + 75), 0.45,
                                                        ifelse(data[i, "txs"] %in% c(n, n + 25, n + 50, n + 75), 0.55,
                                                               data[i, "color.g"])))))
  }

  for (i in seq(1, nrow(data))){
    j <- c(3, 5); k <- c(8, 10); l <- c(13, 15); m <- c(18, 20); n <- c(23, 25)
    data[i, "color.b"] <-ifelse(data[i, "txs"] %in% c(j, j + 25, j + 50, j + 75), 1,
                                   ifelse(data[i, "txs"] %in% c(k, k + 25, k + 50, k + 75), 0.50,
                                          ifelse(data[i, "txs"] %in% c(l, l + 25, l + 50, l + 75), 0.80,
                                                 ifelse(data[i, "txs"] %in% c(m, m + 25, m + 50, m + 75), 0.40,
                                                        ifelse(data[i, "txs"] %in% c(n, n + 25, n + 50, n + 75), 0.55,
                                                               data[i, "color.b"])))))
  }

  sink(tempfile())
  data$color.tx <- print(rgb(data$color.r,
                             data$color.g,
                             data$color.b, 0.8))
  sink()


  if (metrics.name == "Probabilities") {
    dataSet <- as.data.frame(cbind(
      data[, c("outcome", "outcomes", "tx", "txs", "rank", "color.tx")],
      data[, c(3:(length(unique(data$tx))+2))]))
  } else {

    dataSet <- data[, c("outcome", "outcomes", "importance", "tx", "txs", "color.tx", "metrics")]

    for (outcome.i in c(1:max(dataSet$outcomes))) {
      dataTempA <- dataSet[dataSet$outcomes == outcome.i, ]

      if (length(which(as.data.frame(table(dataTempA$metrics))$Freq > 1)) > 0) {
        for (tx.i in c(1:(nrow(dataTempA) - 1))) {
          if (dataTempA$importance[tx.i] %in% dataTempA$importance[c((tx.i + 1):nrow(dataTempA))]) {
            importanceAdj <- sum(dataTempA$metrics[c((tx.i + 1):nrow(dataTempA))] == dataTempA$metrics[tx.i]) / 20
            dataTempA$importance[tx.i] <- importanceAdj + dataTempA$importance[tx.i]
          }
        }
      }

      if (outcome.i == 1) {
        dataTempB <- dataTempA
      } else {
        dataTempB <- rbind(dataTempA, dataTempB)
      }
    }
    dataSet <- dataTempB

  }


  dataList              <- dataSet
  class(dataList)       <- "rankinma"
  dataList$metrics.name <- metrics.name
  dataList$ls.outcome   <- unique(data$outcome)
  dataList$ls.tx        <- unique(data$tx)
  dataList$n.outcome    <- length(unique(data$outcome))
  dataList$n.tx         <- length(unique(data$tx))
  dataList$data         <- dataSet
  dataRankinma          <- dataList

  cat(paste("\n"), fill = TRUE, sep = "")
  cat(paste("Summary of metrics:\n",
            "Metrics: ", dataList$metrics.name, "\n",
            "Outcomes:   ", dataList$n.outcome, "\n",
            "Treatments: ", dataList$n.tx, "\n"),
      fill = TRUE, sep = "")

  cat(paste("List of outcomes:"),
      fill = TRUE, sep = "")
  cat(paste(" ", c(1:dataList$n.outcome), dataList$ls.outcome, sep = " "),
      fill = TRUE, sep = "\n")

  cat(paste("List of treatments:"),
      fill = TRUE, sep = "")
  cat(paste(" ", c(1:dataList$n.tx), dataList$ls.tx, sep = " "),
      fill = TRUE, sep = "\n")

  dataRankinma  <- dataList

}


#' @title Display color of each treatment
#'
#' @description
#' ShowColor is a function for showing colors of every treatment on plot of
#' treatment rank metrics in *rankinma*.
#'
#' @param data DATA of *rankinma* class.
#'
#' @export ShowColor

ShowColor <- function(data) {

  if (!inherits(data, "rankinma"))
    stop('Argument "data" must be an object of class \"rankinma\".')

  plot(c(0, 1.5),
       c(0, 1),
       frame.plot = FALSE,
       xlab = "", ylab="", xaxt = "n", yaxt = "n",
       col = "white")
  segments(rep(0.5, max(data$txs)),
           (0.95) / max(data$txs) * unique(data$txs),
           rep(0.7, max(data$txs)),
           (0.95) / max(data$txs) * unique(data$txs),
           lwd = 6,
           col = data$color.tx)
  text(rep(0.8, max(data$txs)),
       (0.95) / max(data$txs) * unique(data$txs),
       data$tx, pos = 4)
}
