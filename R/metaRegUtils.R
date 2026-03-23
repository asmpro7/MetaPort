#' Compute a Meta-Regression Model
#'
#' Analysis-agnostic: works with any `meta` object (metacont, metabin, etc.).
#' Builds a formula from the terms list and calls `meta::metareg()`.
#' Moderator columns are expected to already exist in `model$data`
#' (ensured by passing `data=` when creating the meta object).
#'
#' @param model A `meta` object (must have been created with `data=`).
#' @param options The jamovi options object (needs `metaRegTerms`,
#'   `metaRegIntercept`).
#' @return A `metareg` object, or `NULL` if model or terms are empty.
#' @noRd
computeMetaRegModel <- function(model, options) {
  if (is.null(model)) {
    return()
  }

  terms <- options$metaRegTerms
  if (length(terms) == 0) {
    return()
  }

  composed <- jmvcore::composeTerms(terms)
  formula <- as.formula(paste("~", paste(composed, collapse = " + ")))
  meta::metareg(model, formula, intercept = options$metaRegIntercept)
}


#' Update Meta-Regression Result Visibility
#'
#' Sets visibility of meta-regression text based on whether the user
#' has assigned at least one covariate or factor and the summary
#' checkbox is on. Called from `.init()` to avoid flashing.
#'
#' @param options The `self$options` object.
#' @param results The `self$results` object.
#' @noRd
updateMetaRegVisibility <- function(options, results) {
  hasMetaRegVars <-
    length(options$metaRegCovs) > 0 || length(options$metaRegFactors) > 0
  results$metaRegText$setVisible(hasMetaRegVars && options$showMetaRegSummary)
}


#' Initialize the Meta-Regression Text Skeleton
#'
#' Called from `.run()` to show a titled HTML placeholder before the
#' model is available. Same pattern as `initLeaveOneOutText()`.
#'
#' @param textResult Html result element.
#' @param options The `self$options` object.
#' @param requiredVars Character vector of option names that must be assigned.
#' @noRd
initMetaRegText <- function(textResult, options, requiredVars) {
  if (!textResult$visible) {
    return()
  }
  if (textResult$isFilled()) {
    return()
  }
  if (!hasRequiredVars(options, requiredVars)) {
    textResult$setContent(asHtml(title = "Meta-Regression Summary"))
  }
}


#' Get Scale Label for Meta-Regression Output
#'
#' Returns a human-readable label describing the scale of the
#' regression estimates (e.g., "Log Odds Ratio", "Standardised Mean
#' Difference"). Works with any `meta` object type.
#'
#' @param metaRegModel A `metareg` object.
#' @return A character string label.
#' @noRd
getMetaRegScaleLabel <- function(metaRegModel) {
  meta:::xlab_meta(metaRegModel$.meta$x$sm, backtransf = FALSE)
}


#' Populate the Meta-Regression Text
#'
#' Called from `.run()` when the meta-regression model is available.
#'
#' @param textResult Html result element.
#' @param metaRegModel A `metareg` object.
#' @noRd
populateMetaRegText <- function(textResult, metaRegModel) {
  if (!textResult$visible) {
    return()
  }
  if (textResult$isFilled()) {
    return()
  }
  scaleLabel <- getMetaRegScaleLabel(metaRegModel)
  textResult$setContent(
    asHtml(
      summary(metaRegModel),
      cat(
        "\nNote: Estimates and confidence intervals are on the",
        scaleLabel,
        "scale."
      ),
      title = "Meta-Regression Summary"
    )
  )
}