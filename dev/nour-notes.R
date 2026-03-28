# Notes:
#
# `settings.meta()` for "BMJ" had purple color and other layout settings not
# implemented when we choose `layout = "BMJ"` in the forest plot. I need to open
# issue with this in meta package
#
# `settings.meta()` supposed to change layout and other things including
# rounding, some statistical preferences, etc and some settings not change
# layout, I need to think if we should implement them or not I think no for now
#
# Related to the first point the `colgap.forest` should be 5mm for BMJ but it is
# not applied when we set it at the layout argument level not settings.meta
# level so I deleted auto setting for colgap.forest for now, and hardcoded them
# with the default which is 2mm
#
# `xlim` argument intenally could be symmetric or NUll, right now we do not
# differentiate between them, we use auto to let the function decide


# isFilled() removal from initForestPlot ----------------------------------
#
# We removed isFilled() from initForestPlot for two reasons:
#
# 1. It is always FALSE during .init() — isFilled() checks filePath which
#    is set to NULL at create(). Only .load() (which runs AFTER .init())
#    can restore it. So this guard is dead code in .init().
#
# 2. Image dimensions set by setSize() are NOT saved/restored across
#    requests. fromProtoBuf() reads the CURRENT dimensions (set by .init())
#    and compares them against saved ones to detect sizeChanged. If we
#    skipped setSize() (via isFilled), the image would have default 400×300,
#    causing false sizeChanged detection and re-render at wrong dimensions.
#    So setSize() in .init() is mandatory on every request — we cannot skip
#    it, and therefore cannot skip the model computation that feeds it.


# Model caching via state -------------------------------------------------
#
# With the current guards (hasRequiredVars in .run(), !visible in
# initForestPlot, populateMainText, populateSubgroupText, and isFilled()
# on text results), the model is never computed when ALL outputs are hidden.
# R's lazy evaluation ensures self$model promises are never forced.
#
# Additionally, text results now use clearWith + isFilled() so changing
# non-model options (e.g., labelLeft, forestLayout) skips text population
# and never forces the model active binding.
#
# Caching the model in state (via setState in .run(), read in active binding)
# would NOT help when a forest plot is visible, because .init() accesses the
# model for calcForestHeight() BEFORE .load() restores state.
#
# The only cases where state caching could avoid model recomputation:
#   1. Forest plot unchecked but text visible — .init() never accesses the
#      model (!visible guard exits), .run() could read the cached model
#      from state instead of recomputing via the active binding.
#   2. Sub-analyses without forest plots (e.g., publication bias using base R
#      plots) — no .init() sizing needed, so changing their options could
#      reuse the cached model instead of recomputing.
#
# These are narrow conditions but a viable future optimization path.


# Computational cost assessment --------------------------------------------
#
# metacont(), metabin(), metagen() are all fast (<1 sec). The standard
# practice in jmv and gamlj is to recompute unconditionally in .run() —
# neither module uses state checks or isFilled() guards to skip model
# computation. State caching only pays off for computations >5 sec
# (e.g., gamlj's bootstrap model, which uses tempfile() + save()/load()).
#
# Current conclusion: no caching needed for any meta-analysis model.
# Revisit if we add bootstrap or permutation tests.


# Html clearWith + isFilled() optimization ---------------------------------
#
# clearWith on Html controls the .stale flag (not content restoration —
# content is ALWAYS restored from protobuf by fromProtoBuf lines 120-123
# in jmvcore/R/html.R). The .stale flag feeds isFilled():
#
#   isFilled() returns TRUE  when .stale = FALSE and content != ''
#   isFilled() returns FALSE when .stale = TRUE
#
# IMPORTANT: setContent() sets .stale = FALSE. If .init()
# calls setContent() (e.g., skeleton title), then fromProtoBuf() sees
# .stale = FALSE and returns immediately — the clearWith
# logic NEVER runs. So clearWith on Html only works when .init() does NOT
# touch the content.
#
# For this to work:
#   1. Skeleton setContent() lives in .run() (via initMainText /
#      initSubgroupText), NOT in .init()
#   2. Explicit clearWith on the Html result in .r.yaml lists only
#      model-affecting options
#   3. isFilled() guards in .run() skip populateMainText /
#      populateSubgroupText when content is still valid
#
# When a non-listed option changes (e.g., labelLeft):
#   fromProtoBuf → clearWith no match → .stale stays FALSE → isFilled()
#   TRUE → .run() skips populate → model never forced
#
# When a listed option changes (e.g., meanE):
#   fromProtoBuf → clearWith matches → .stale = TRUE → isFilled()
#   FALSE → .run() recomputes → setContent() updates


# hasRequiredVars() vs is.null(model) --------------------------------------
#
# model is NULL exactly when required variables are not assigned (because
# resolveModel() returns NULL when hasRequiredVars() fails). So both
# checks answer the same question: "are the data columns assigned yet?"
#
# Use hasRequiredVars(options, vars) when:
#   - You only need to know whether variables are assigned
#   - You do NOT need the actual model object
#   - Forcing the active binding would be wasted work
#   Examples: initMainText(), initSubgroupText(), the .run() early-exit guard
#
# Use is.null(model) / is.null(self$model) when:
#   - You genuinely need the model object for the next step
#   - Forcing the active binding is intentional and necessary
#   - It is effectively a low-code shorthand for hasRequiredVars —
#     both are identical in result, but is.null forces the binding
#   Examples: initForestPlot() (needs model for calcForestHeight),
#             render functions (need model to draw the plot)
#
# Every is.null(model) check should be immediately followed by code that
# uses the model. If the check is purely "are vars assigned?" with no
# model usage after, replace it with hasRequiredVars.
#
# Some checks are already covered by earlier guards. For example,
# subgroup visibility depends on options$subgroupVariable — when it's
# NULL, updateSubgroupVisibility hides the subgroup text, so !visible
# exits before any model check. Combined with hasRequiredVars in .run(),
# the subgroup model is guaranteed to exist by the time populate runs.
# In cases like this, is.null(subgroupModel) is redundant and should
# be removed.
