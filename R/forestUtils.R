#' Build a Forest Plot Expression
#'
#' Generic helper that constructs a quoted `meta::forest()` call from Jamovi
#' options. Reusable across all meta-analysis classes (metacont, metabin, etc.).
#'
#' @param model A `meta` object (e.g., from `meta::metacont`).
#' @param options A Jamovi options object with forest-related fields.
#' @return A quoted `call` that produces a forest plot when evaluated.
#' @noRd
buildForestExpr <- function(model, options) {
  args <- list(x = quote(model), new = FALSE)

  # Layout
  args$layout <- options$forestLayout

  # Group labels (column headers)
  args$label.e <- options$labelE
  args$label.c <- options$labelC

  # Graph labels (left/right of null effect)
  if (nzchar(options$labelLeft)) {
    args$label.left <- options$labelLeft
  }
  if (nzchar(options$labelRight)) {
    args$label.right <- options$labelRight
  }

  # Sort
  if (!is.null(options$sortBy) && options$sortBy != "none") {
    sortmap <- list(
      effectAsc = quote(model$TE),
      effectDesc = quote(-model$TE),
      weightAsc = quote(
        if (isTRUE(model$random)) model$w.random else model$w.common
      ),
      weightDesc = quote(
        -(if (isTRUE(model$random)) model$w.random else model$w.common)
      )
    )
    args$sortvar <- sortmap[[options$sortBy]]
  }

  as.call(c(quote(meta::forest), args))
}


#' Calculate Height for Forest Plot
#'
#' This function evaluates a forest plot function from the `meta` package in a
#' null device to calculate the required height for the plot without rendering
#' it.
#'
#' @param expr A quoted expression which calls a forest plot function from the
#'   `meta` package.
#' @return The calculated height in points.
#' @noRd
calcForestHeight <- function(expr) {
  oldDev <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  on.exit({
    grDevices::dev.off()
    if (oldDev > 1) grDevices::dev.set(oldDev)
  })

  res <- eval(expr, envir = parent.frame())
  res$figheight$total_height * 72
}


#' Initialize a Forest Plot Image (Shared .init() Helper)
#'
#' Handles the full sizing workflow for any forest-type image:
#' checks `isFilled()`, builds the expression, calculates height,
#' and calls `setSize()`. Designed to be called from `.init()` across
#' all meta-analysis types.
#'
#' @param image A jamovi Image result element (e.g., `self$results$plot`).
#' @param model A `meta` object. If `NULL`, the function returns early.
#' @param options A Jamovi options object with forest-related fields.
#' @param width Plot width in pixels (default 800).
#' @return `NULL` invisibly. Called for side effects (`setSize()`).
#' @noRd
initForestPlot <- function(image, model, options, width = 800) {
  if (image$isFilled()) {
    return()
  }
  if (!image$visible) {
    return()
  }
  if (is.null(model)) {
    return()
  }

  expr <- buildForestExpr(model, options)
  height <- calcForestHeight(expr)
  image$setSize(width = width, height = height)
}