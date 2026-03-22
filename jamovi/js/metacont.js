var regression = require("./regression");

module.exports = {
  // Fires when the Supplier needs to refresh its available items.
  // Magic naming: {controlName}_updated for Supplier controls.
  metaRegModelSupplier_updated: function (ui) {
    regression.updateModelTerms(
      ui,
      this,
      "metaRegCovs",
      "metaRegFactors",
      "metaRegModelSupplier",
      "metaRegTerms",
    );
  },

  // Fires when the user adds/removes variables in Covariates
  metaRegCovs_changed: function (ui) {
    regression.updateModelTerms(
      ui,
      this,
      "metaRegCovs",
      "metaRegFactors",
      "metaRegModelSupplier",
      "metaRegTerms",
    );
  },

  // Fires when the user adds/removes variables in Factors
  metaRegFactors_changed: function (ui) {
    regression.updateModelTerms(
      ui,
      this,
      "metaRegCovs",
      "metaRegFactors",
      "metaRegModelSupplier",
      "metaRegTerms",
    );
  },
};
