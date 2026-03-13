metaContClass <- R6::R6Class(
  "metaContClass",
  inherit = metaContBase,
  active = list(
    model = function() {
      if (is.null(private$.model)) {
        cached <- self$results$modelCache$state
        if (!is.null(cached)) {
          private$.model <- cached
        } else {
          data <- self$data
          if (is.null(data) || nrow(data) == 0) {
            data <- self$readDataset()
          }
          private$.model <- computeContModel(data, self$options)
        }
      }
      private$.model
    }
  ),
  private = list(
    .model = NULL,

    .init = function() {
      initForestPlot(self$results$plot, self$model, self$options)
    },

    .run = function() {
      if (is.null(self$model)) {
        return(NULL)
      }

      # Cache model for next cycle
      self$results$modelCache$setState(self$model)

      # Overall results
      if (self$options$showSummary) {
        self$results$text$setContent(asHtml(summary(self$model)))
      }
    },

    .forestPlot = function(image, ...) {
      if (is.null(self$model)) {
        return(FALSE)
      }

      grid::grid.newpage()
      grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))

      model <- self$model
      eval(buildForestExpr(model, self$options))

      TRUE
    }
  )
)