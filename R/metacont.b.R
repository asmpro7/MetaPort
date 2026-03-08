
# This file is a generated template, your changes will not be overwritten

metaContClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
  "metaContClass",
  inherit = metaContBase,
  active = list(
    model = function() {
      if (is.null(private$.model)) {
        private$.model <- computeContModel(self$data, self$options)
      }
      private$.model
    }
  ),
  private = list(
    .model = NULL,

    .init = function() {
      if (self$results$plot$isFilled()) return()
      if (!self$options$forestPlot) return()

      # Compute model early for sizing
      private$.model <- computeContModel(
        self$readDataset(headerOnly = FALSE),
        self$options
      )
      if (is.null(private$.model)) return()

      model <- private$.model
      expr <- buildForestExpr(model, self$options)
      height <- calcForestHeight(expr)
      self$results$plot$setSize(width = 800, height = height)
    },

    .run = function() {
      if (is.null(self$model)) return(NULL)

      # Overall results
      if (self$options$showSummary)
        self$results$text$setContent(asHtml(summary(self$model)))
    },

    .forestPlot = function(image, ...) {
      if (is.null(self$model)) return(FALSE)

      grid::grid.newpage()
      grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))

      model <- self$model
      eval(buildForestExpr(model, self$options))

      TRUE
    }
  )
)
