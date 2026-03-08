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
  if (nzchar(options$labelLeft))
    args$label.left <- options$labelLeft
  if (nzchar(options$labelRight))
    args$label.right <- options$labelRight

  # Sort
  if (!is.null(options$sortBy) && options$sortBy != "none") {
    sortmap <- list(
      effectAsc  = quote(model$TE),
      effectDesc = quote(-model$TE),
      weightAsc  = quote(
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
