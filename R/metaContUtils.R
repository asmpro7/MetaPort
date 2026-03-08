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

  required <- c("meanE", "sdE", "nE", "meanC", "sdC", "nC")
  for (opt in required) {
    if (is.null(options[[opt]])) return(NULL)
  }

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

  # Fit model
  meta::metacont(
    n.e        = n.e,
    mean.e     = mean.e,
    sd.e       = sd.e,
    n.c        = n.c,
    mean.c     = mean.c,
    sd.c       = sd.c,
    studlab    = studlab,
    sm         = options$sm,
    method.tau = options$methodTau,
    method.smd = options$methodSmd,
    common     = options$common,
    random     = options$random,
    prediction = options$prediction,
    level.ma   = options$confidenceLevel,
    label.e    = options$groupLabelE,
    label.c    = options$groupLabelC
  )
}

#' Build a Forest Plot Expression for a Continuous Outcome Model
#'
#' Returns a quoted expression ready to be `eval()`'d inside a graphics device.
#' The expression references `model` which must exist in the calling
#' environment.
#'
#' @param model A `meta::metacont` (or compatible) object.
#' @return A quoted `call` that produces a forest plot when evaluated.
#' @noRd
buildContForestExpr <- function(model) {
  quote(
    metafor::forest(
      model,
      new       = FALSE,
      leftcols  = c("studlab", "mean.e", "sd.e", "n.e",
                     "mean.c", "sd.c", "n.c"),
      leftlabs  = c("Study", "Mean", "SD", "Total",
                     "Mean", "SD", "Total"),
      col.diamond  = "black",
      col.subgroup = "gray30",
      digits.sd    = 2
    )
  )
}
