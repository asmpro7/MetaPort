#' Update Publication Bias Result Visibility
#'
#' Sets visibility of publication bias results based on per-output
#' checkboxes. Called from `.init()` to avoid flashing.
#'
#' @param options The `self$options` object.
#' @param results The `self$results` object.
#' @noRd
updatePubBiasVisibility <- function(options, results) {
  results$funnelPlotImage$setVisible(options$funnelPlot)
}


#' Render Funnel Plot for Publication Bias
#'
#' Draws a standard or contour-enhanced funnel plot using `meta::funnel()`.
#' When contour-enhanced, a legend is added with custom p-value labels
#' using correct statistical notation (strict `<` on lower bound,
#' `\u2264` on upper bound), including the white non-significant region.
#'
#' @param model A `meta` object.
#' @param options Jamovi options object.
#' @noRd
renderFunnelPlot <- function(model, options) {
  if (options$funnelContour) {
    fun <- meta::funnel(model,
      type     = "contour",
      studlab  = options$funnelStudyLabel
    )

    if (options$funnelLegend) {
      # Custom labels: strict < on lower bound, <= on upper bound,
      # matching meta's rendering (boundary -> more-significant band)
      contour_labels <- c(
        "p > 0.10",
        "0.05 < p \u2264 0.10",
        "0.01 < p \u2264 0.05",
        "p \u2264 0.01"
      )
      contour_fills <- c("white", fun$col.contour)

      legend(options$funnelLegendPos,
        legend = contour_labels,
        fill   = contour_fills,
        bg     = "white",
        cex    = options$funnelLegendCex / 100
      )
    }
  } else {
    meta::funnel(model,
      studlab = options$funnelStudyLabel
    )
  }
}
