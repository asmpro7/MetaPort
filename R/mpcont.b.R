
# This file is a generated template, your changes will not be overwritten

mpcontClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mpcontClass",
    inherit = mpcontBase,
    private = list(
        .run = function() {
          mean.e <- self$options$mean.e
          sd.e <- self$options$sd.e
          n.e <- self$options$n.e
          mean.c <- self$options$mean.c
          sd.c <- self$options$sd.c
          n.c <- self$options$n.c
          id <- self$options$id
          sm <- self$options$sm
          label.e <- self$options$label.e
          label.c <- self$options$label.c
          random <- self$options$random
          common <- self$options$common
          
          data <- self$data
          
          OverallMeta <- meta::metacont(data = data,
                                  mean.e = mean.e, sd.e = sd.e, n.e = as.numeric(n.e),
                                  mean.c = mean.c, sd.c = sd.c, n.c = as.numeric(n.c),
                                  studlab = id, sm = sm,
                                  label.e = label.e, label.c = label.c, random = random, common = common)

          metamodel <- self$results$plot
          metamodel$setState(OverallMeta)
          self$results$text$setContent(OverallMeta)
          

        },
        .plot=function(metamodel, ...) {
          
          
          meta::forest(metamodel$state, 
                 leftcols = c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c"),
                 leftlabs = c("Study", "Mean", "SD", "Total", "Mean", "SD", "Total"),
                 col.diamond = "black",col.subgroup ="gray30", digits.sd = 2)
          TRUE
        })
)
