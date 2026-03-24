#' Get Transformation Info for a Meta Summary Measure
#'
#' Returns the back-transformation function, reference line, and y-axis labels
#' matching bubble.R's exact logic. Works with any meta analysis type (metacont,
#' metabin, metaprop, metarate, metagen, etc.).
#'
#' Accepts a metareg model and extracts the original meta model from it (via
#' `x$.meta$x`), matching how bubble.metareg() works.
#'
#' @param x A `metareg` object (as returned by `meta::metareg()`).
#' @return A list with:
#' * `fn`: A single-argument back-transform function, or `NULL`.
#' * `refline`: The null-effect reference line (0, 1, or `NA`).
#' * `ylab`: Y-axis label on the transformed scale.
#' * `ylab_bt`: Y-axis label on the natural (back-transformed) scale.
#' @noRd
getSmTransfInfo <- function(x) {
  # Extract original meta model (same as bubble.R L197)
  m <- x$.meta$x
  sm <- m$sm

  # --- Back-transformation function ---
  # PFT/IRFT need n/time we don't have, skip them
  fn <- if (sm %in% c("PFT", "IRFT")) {
    NULL
  } else if (meta:::is_untransformed(sm)) {
    NULL
  } else {
    function(x) meta::backtransf(x, sm = sm)
  }

  # --- Reference line (bubble.R L219-226) ---
  refline <- if (
    meta:::is_prop(sm) || meta:::is_rate(sm) || meta:::is_mean(sm)
  ) {
    NA
  } else if (meta:::is_relative_effect(sm)) {
    1
  } else {
    0
  }

  # --- Y-axis labels (bubble.R L349-364) ---
  ylab <- meta:::xlab_meta(
    sm,
    backtransf = FALSE,
    func.transf = m$func.transf,
    func.backtransf = m$func.backtransf
  )

  ylab_bt <- meta:::xlab_meta(
    sm,
    backtransf = TRUE,
    func.transf = m$func.transf,
    func.backtransf = m$func.backtransf
  )

  # Patch empty labels (bubble.R L353-362)
  if (ylab_bt == "") {
    if (sm == "PRAW" || sm %in% c("PLN", "PAS", "PLOGIT")) {
      ylab_bt <- "Proportion"
    } else if (sm == "IR" || sm %in% c("IRLN", "IRS")) {
      ylab_bt <- "Incidence Rate"
    } else if (sm == "COR" || sm == "ZCOR") {
      ylab_bt <- "Correlation"
    }
  }

  list(fn = fn, refline = refline, ylab = ylab, ylab_bt = ylab_bt)
}