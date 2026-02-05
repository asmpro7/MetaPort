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
  old_dev <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  on.exit({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev)
  })

  # Evaluate the expression in the parent environment where variables are
  # defined
  res <- eval(expr, envir = parent.frame())

  res$figheight$total_height * 72
}