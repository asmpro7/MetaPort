#' Compute a Continuous Outcome Meta-Analysis Model
#'
#' Standalone helper that creates a `meta::metacont` object from Jamovi
#' options. Designed to be reused across multiple analysis classes.
#'
#' @param data A data frame (typically `self$data`).
#' @param options A Jamovi options object with the required fields.
#' @return A `meta::metacont` object, or `NULL` if required columns are
#'   missing.
#' @noRd
computeContModel <- function(data, options) {
  # Guard: all six required columns must be assigned

  # Extract and convert columns
  mean.e <- jmvcore::toNumeric(data[[options$meanE]])
  sd.e   <- jmvcore::toNumeric(data[[options$sdE]])
  n.e    <- jmvcore::toNumeric(data[[options$nE]])
  mean.c <- jmvcore::toNumeric(data[[options$meanC]])
  sd.c   <- jmvcore::toNumeric(data[[options$sdC]])
  n.c    <- jmvcore::toNumeric(data[[options$nC]])

  # Optional study labels
  studlab <- NULL
  if (!is.null(options$studyLabel)) {
    studlab <- data[[options$studyLabel]]
  }

  # Confidence / prediction level (shared)
  level <- options$confidenceLevel

  # Ad-hoc correction: map jamovi "none" to meta's ""
  adhoc <- if (options$adhocHaknCi == "none") "" else options$adhocHaknCi

  # Fit model
  meta::metacont(
    n.e             = n.e,
    mean.e          = mean.e,
    sd.e            = sd.e,
    n.c             = n.c,
    mean.c          = mean.c,
    sd.c            = sd.c,
    studlab         = studlab,
    sm              = options$sm,
    method.tau      = options$methodTau,
    method.smd      = options$methodSmd,
    common          = options$common,
    random          = options$random,
    prediction      = options$prediction,
    level.ma        = level,
    level.predict   = level,
    method.random.ci = options$methodRandomCi,
    adhoc.hakn.ci   = adhoc
  )
}
