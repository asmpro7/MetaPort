mpcontClass <- {
  R6::R6Class(
    "mpcontClass",
    inherit = mpcontBase,
    active = list(
      model = function() {
        if (is.null(private$.model)) {
          private$.model <- private$.computeModel(self$data)
        }
        private$.model
      },
      forestPlotExpr = function() {
        if (is.null(private$.forestPlotExpr)) {
          private$.forestPlotExpr <- quote(
            metafor::forest(
              self$model,
              new = FALSE,
              leftcols = c(
                "studlab",
                "mean.e",
                "sd.e",
                "n.e",
                "mean.c",
                "sd.c",
                "n.c"
              ),
              leftlabs = c(
                "Study",
                "Mean",
                "SD",
                "Total",
                "Mean",
                "SD",
                "Total"
              ),
              col.diamond = "black",
              col.subgroup = "gray30",
              digits.sd = 2
            )
          )
        }
        private$.forestPlotExpr
      }
    ),
    private = list(
      .model = NULL,
      .forestPlotExpr = NULL,
      .biasModel = NULL,
      .tfModel = NULL,
      .checkRequiredColumns = function() {
        if (
          is.null(self$options$meanE) ||
            is.null(self$options$sdE) ||
            is.null(self$options$nE) ||
            is.null(self$options$meanC) ||
            is.null(self$options$sdC) ||
            is.null(self$options$nC)
        ) {
          return(FALSE)
        }
        TRUE
      },
      .init = function() {
        if (self$results$plot$isFilled()) {
          return()
        }

        if (!self$options$forestPlot) {
          return()
        }

        if (!private$.checkRequiredColumns()) {
          return()
        }

        data <- self$readDataset(headerOnly = FALSE)

        private$.model <- private$.computeModel(data)

        expr <- self$forestPlotExpr

        height <- calcForestHeight(expr)
        self$results$plot$setSize(width = 800, height = height)
      },
      .computeModel = function(data) {
        if (!private$.checkRequiredColumns()) {
          return(NULL)
        }

        # 1. Extract and Validate Data
        # converting to numeric to strip attributes (jmvcore::toNumeric)
        mean.e <- jmvcore::toNumeric(data[[self$options$meanE]])
        sd.e <- jmvcore::toNumeric(data[[self$options$sdE]])
        n.e <- jmvcore::toNumeric(data[[self$options$nE]])
        mean.c <- jmvcore::toNumeric(data[[self$options$meanC]])
        sd.c <- jmvcore::toNumeric(data[[self$options$sdC]])
        n.c <- jmvcore::toNumeric(data[[self$options$nC]])

        # 2. Optional study labels
        studlab <- NULL
        if (!is.null(self$options$studyLabel)) {
          studlab <- data[[self$options$studyLabel]]
        }

        # 3. Options
        label.e <- self$options$groupLabelE
        label.c <- self$options$groupLabelC
        sm <- self$options$sm
        method.tau <- self$options$methodTau
        method.smd <- self$options$methodSmd
        random <- self$options$random
        common <- self$options$common
        prediction <- self$options$prediction
        level <- self$options$confidenceLevel

        # 4. Compute Model
        OverallMeta <- meta::metacont(
          n.e = n.e,
          mean.e = mean.e,
          sd.e = sd.e,
          n.c = n.c,
          mean.c = mean.c,
          sd.c = sd.c,
          studlab = studlab,
          sm = sm,
          method.tau = method.tau,
          method.smd = method.smd,
          common = common,
          random = random,
          prediction = prediction,
          level.ma = level,
          label.e = label.e,
          label.c = label.c
        )

        OverallMeta
      },
      .run = function() {
        if (is.null(self$model)) {
          return(NULL)
        }

        # Use simple utility to style it (it handles capture internally)
        html_content <- asHtml(summary(self$model))

        self$results$text$setContent(html_content)

        # Sizing is now handled in .init

        # End of my part, it would be good if we could clear .run() function
        # and use separate methods/functions rather than listing all of them
        # below

        private$.prepareLOOPlot()
        if (is.null(self$results$bias$state) # Clear triggered by user changes
        && isFALSE(self$options$biasTest == "disabled") # Don't run unless test is enabled
        ) {
          private$.runPublicationBias()
        } # Used clearWith, in analysis file to clear the table.
        # else {
        #   # clear BIAS table row (tables don't have clear())
        #   self$results$bias$setRow(
        #     rowNo = 1,
        #     values = list(
        #       method = "",
        #       statistic = NA_real_,
        #       p = NA_real_,
        #       interpretation = ""
        #     )
        #   )

        #   # clear bias images
        #   self$results$funnel$setState(NULL)
        #   self$results$doi$setState(NULL)

        #   # clear TRIMFILL table row
        #   self$results$tf$setRow(
        #     rowNo = 1,
        #     values = list(
        #       k0 = NA_integer_,
        #       model = "",
        #       TE = NA_real_,
        #       lci = NA_real_,
        #       uci = NA_real_,
        #       p = NA_real_
        #     )
        #   )

        #   # clear trimfill image
        #   self$results$funnel_tf$setState(NULL)

        #   # clear stored models
        #   private$.biasModel <- NULL
        #   private$.tfModel <- NULL
        # }

        if (is.null(self$results$subgroup_text$state) # Cleared by user changes
        && !is.null(self$options$subgroupCovariate)) # There is a subgroup covariate
          {
            subCovariate <- self$data[[self$options$subgroupCovariate]]
            subCovariateName <- self$options$subgroupName
            subgroup_results <- meta:::update.meta(
              self$model,
              subgroup = subCovariate,
              subgroup.name = subCovariateName
            )

            self$results$subgroup_text$setContent(
              subgroup_results
            )
            subgroup_plot <- self$results$subgroup_plot
            subgroup_plot$setState(subgroup_results)
          }


        if (is.null(self$results$meta_regression_text$state) &&
          !is.null(self$options$MetaRegressionCovariate)) {
          model <- self$model
          model$data$covariate <- jmvcore::toNumeric(self$data[[
            self$options$MetaRegressionCovariate
          ]])
          meta_regression_results <- meta::metareg(model, ~covariate)
          self$results$meta_regression_text$setContent(
            meta_regression_results
          )
          meta_regression_plot <- self$results$meta_regression_plot
          meta_regression_plot$setState(meta_regression_results)
        }


        #################################################
      },
      .forestPlot = function(image, ...) {
        # No state check needed due to requiresData: true in yaml
        if (is.null(self$model)) {
          return(FALSE)
        }

        grid::grid.newpage()
        grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))

        eval(self$forestPlotExpr)

        TRUE
      },
      .prepareLOOPlot = function() {
        if (!self$options$LOO || is.null(self$model)) {
          return(FALSE)
        }
        LOOResults <- meta::metainf(self$model)
        self$results$LOOText$setContent(LOOResults)

        LOOExpr <- quote(
          meta::forest(
            LOOResults,
            rightcols = c("effect", "ci", "tau2", "I2"),
            col.diamond = "black",
            col.subgroup = "gray30",
            new = FALSE
          )
        )

        height <- calcForestHeight(LOOExpr)
        self$results$LOOPlot$setSize(width = 800, height = height)
        self$results$LOOPlot$setState(LOOResults)
        # Store data, not expression
      },
      .LOOPlot = function(image, ...) {
        if (is.null(image$state) || !is.null(self$results$LOOText$state)) {
          return(FALSE)
        }
        grid::grid.newpage()
        grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))

        meta::forest(
          image$state,
          rightcols = c("effect", "ci", "tau2", "I2"),
          col.diamond = "black",
          col.subgroup = "gray30",
          new = FALSE
        )

        TRUE
      },
      .meta_reg_plot_func = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }

        par(bg = "white")
        meta::bubble(
          image$state,
          studlab = TRUE,
          xlab = self$options$MetaRegressionCovariate
        )
        TRUE
      },
      .subgroup_plot_func = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }

        par(bg = "white")
        meta::forest(
          image$state,
          leftcols = c(
            "studlab",
            "mean.e",
            "sd.e",
            "n.e",
            "mean.c",
            "sd.c",
            "n.c"
          ),
          leftlabs = c("Study", "Mean", "SD", "Total", "Mean", "SD", "Total"),
          col.diamond = "black",
          col.subgroup = "gray30",
          digits.sd = 2
        )
        TRUE
      },
      ################ pub
      .runPublicationBias = function() {
        private$.biasModel <- NULL
        private$.tfModel <- NULL

        if (is.null(self$model)) {
          return(NULL)
        }

        TE <- self$model$TE
        seTE <- self$model$seTE

        if (is.null(TE) || is.null(seTE)) {
          return(NULL)
        }

        keep <- is.finite(TE) & is.finite(seTE) & !is.na(TE) & !is.na(seTE)
        TE <- TE[keep]
        seTE <- seTE[keep]

        if (length(TE) < 2) {
          return(NULL)
        }

        private$.biasModel <- meta::metagen(
          TE = TE,
          seTE = seTE,
          sm = self$options$sm,
          common = isTRUE(self$options$common),
          random = isTRUE(self$options$random),
          method.tau = self$options$methodTau,
          level = self$options$confidenceLevel
        )

        if (self$options$biasTest == "egger") {
          private$.fillEggerRow(TE, seTE)

          if (length(TE) >= 10) {
            self$results$funnel$setState(list(
              model = private$.biasModel,
              tick = as.numeric(Sys.time())
            ))
          } else {
            self$results$funnel$setState(NULL) # clears the funnel image
          }
        }

        if (self$options$biasTest == "lfk") {
          private$.fillLFKRow(TE, seTE)

          if (length(TE) >= 5) {
            self$results$doi$setState(list(
              TE = TE,
              seTE = seTE,
              tick = as.numeric(Sys.time())
            ))
          } else {
            self$results$doi$setState(NULL) # or leave unchanged
          }
        }

        if (isTRUE(self$options$trimfill)) {
          private$.runTrimFill(TE, seTE)
          self$results$funnel_tf$setState(list(tick = as.numeric(Sys.time())))
        }

        NULL
      },
      .fillEggerRow = function(TE, seTE) {
        method <- "Egger regression"
        stat <- NA_real_
        pval <- NA_real_

        k <- length(TE)

        # Rule: Egger needs at least 10 studies
        if (k < 10) {
          self$results$bias$setRow(
            rowNo = 1,
            values = list(
              method = method,
              statistic = NA,
              p = NA,
              interpretation = "Egger test requires at least 10 studies."
            )
          )
          return(NULL)
        }

        # Need SE variability
        if (length(unique(seTE)) < 2) {
          self$results$bias$setRow(
            rowNo = 1,
            values = list(
              method = method,
              statistic = NA_real_,
              p = NA_real_,
              interpretation = "Egger test not applicable (no SE variability)."
            )
          )
          return(NULL)
        }

        # Run metabias safely
        eg <- tryCatch(
          meta::metabias(private$.biasModel, method.bias = "linreg"),
          error = function(e) NULL
        )

        if (is.null(eg)) {
          self$results$bias$setRow(
            rowNo = 1,
            values = list(
              method = method,
              statistic = NA_real_,
              p = NA_real_,
              interpretation = "Egger test failed to run."
            )
          )
          return(NULL)
        }

        stat <- suppressWarnings(as.numeric(eg$statistic))
        pval <- suppressWarnings(as.numeric(eg$p.value))

        # Safe finite checks (handle NA)
        if (
          is.na(stat) || is.na(pval) || !is.finite(stat) || !is.finite(pval)
        ) {
          self$results$bias$setRow(
            rowNo = 1,
            values = list(
              method = method,
              statistic = NA_real_,
              p = NA_real_,
              interpretation = "Egger test not computable."
            )
          )
          return(NULL)
        }

        interp <- if (pval < 0.05) {
          "Evidence of small-study effects (p < 0.05)."
        } else {
          "No evidence of funnel plot asymmetry (p ≥ 0.05)."
        }

        self$results$bias$setRow(
          rowNo = 1,
          values = list(
            method = method,
            statistic = stat,
            p = pval,
            interpretation = interp
          )
        )

        NULL
      },
      .lfkInterpret = function(lfk) {
        if (!is.finite(lfk)) {
          return("LFK not computable.")
        }
        a <- abs(lfk)
        if (a <= 1) {
          return(
            "No evidence of asymmetry (no indication of small-study effects) (|LFK| ≤ 1)."
          )
        }
        if (a <= 2) {
          return(
            "Minor asymmetry, possibly due to small-study effects (1 < |LFK| ≤ 2)."
          )
        }
        "Major asymmetry, suggestive of small-study effects and/or publication bias (|LFK| > 2)."
      },
      .fillLFKRow = function(TE, seTE) {
        keep <- is.finite(TE) &
          is.finite(seTE) &
          !is.na(TE) &
          !is.na(seTE) &
          seTE > 0
        TE <- TE[keep]
        seTE <- seTE[keep]

        k <- length(TE)

        # Rule: LFK needs at least 5 studies
        if (k < 5) {
          self$results$bias$setRow(
            rowNo = 1,
            values = list(
              method = "LFK index (Doi plot)",
              statistic = NA_real_,
              p = NA_real_,
              interpretation = "LFK index requires at least 5 studies."
            )
          )
          return(NULL)
        }

        lfk_obj <- tryCatch(
          metasens::lfkindex(TE = TE, seTE = seTE),
          error = function(e) NULL
        )

        if (is.null(lfk_obj) || is.null(lfk_obj$lfkindex)) {
          self$results$bias$setRow(
            rowNo = 1,
            values = list(
              method = "LFK index (Doi plot)",
              statistic = NA_real_,
              p = NA_real_,
              interpretation = "LFK not computable."
            )
          )
          return(NULL)
        }

        lfk_val <- as.numeric(lfk_obj$lfkindex)

        self$results$bias$setRow(
          rowNo = 1,
          values = list(
            method = "LFK index (Doi plot)",
            statistic = lfk_val,
            p = NA_real_,
            interpretation = private$.lfkInterpret(lfk_val)
          )
        )

        NULL
      },
      .runTrimFill = function(TE, seTE) {
        rma <- metafor::rma(yi = TE, sei = seTE, method = "REML")
        tf <- metafor::trimfill(rma)
        private$.tfModel <- tf

        # Build model label dynamically
        if (isTRUE(self$options$random)) {
          modelLabel <- paste0(
            "Random-effects (",
            toupper(self$options$methodTau),
            ")"
          )
        } else if (isTRUE(self$options$common)) {
          modelLabel <- "Common-effect"
        } else {
          modelLabel <- NA_character_
        }

        self$results$tf$setRow(
          rowNo = 1,
          values = list(
            k0 = tf$k0,
            model = modelLabel,
            TE = as.numeric(tf$b),
            lci = as.numeric(tf$ci.lb),
            uci = as.numeric(tf$ci.ub),
            p = as.numeric(tf$pval)
          )
        )

        NULL
      },
      .funnelPlot = function(image, ...) {
        # YAML renderFun: .funnelPlot
        if (is.null(image$state) || is.null(image$state$model)) {
          return(FALSE)
        }

        graphics::par(bg = "white")
        meta::funnel(image$state$model)
        TRUE
      },
      .doiPlot = function(image, ...) {
        if (
          is.null(image$state) ||
            is.null(image$state$TE) ||
            is.null(image$state$seTE)
        ) {
          return(FALSE)
        }

        TE <- image$state$TE
        seTE <- image$state$seTE

        keep <- is.finite(TE) &
          is.finite(seTE) &
          !is.na(TE) &
          !is.na(seTE) &
          seTE > 0
        TE <- TE[keep]
        seTE <- seTE[keep]

        if (length(TE) < 2) {
          return(FALSE)
        }

        # Jamovi-safe: create a fresh plotting page
        graphics::plot.new()
        graphics::par(bg = "white")

        # Standard DOI plot
        metasens::doiplot(
          TE = TE,
          seTE = seTE,
          lfkindex = TRUE,
          pos.lfkindex = "topleft",
          add = FALSE
        )

        TRUE
      },
      .funnelPlotTF = function(image, ...) {
        # YAML renderFun: .funnelPlotTF
        if (is.null(private$.tfModel)) {
          return(FALSE)
        }

        graphics::par(bg = "white")
        metafor::funnel(private$.tfModel)
        TRUE
      }
    )
  )
}
