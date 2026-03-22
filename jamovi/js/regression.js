/**
 * Shared model builder logic for meta-regression.
 *
 * Keeps a Supplier and Terms list in sync with two variable slots
 * (covariates + factors). When the user adds or removes a variable,
 * this function:
 *   1. Updates the Supplier so the user can drag variables into terms
 *   2. Auto-adds main effects for newly added variables
 *   3. Removes terms whose source variable was removed
 *
 * Uses the `utils` and `FormatDef` globals that jamovi injects into
 * every module's JS context (both jus 2.0 and 3.0).
 *
 * @param {Object} ui           - The UI controls object.
 * @param {Object} context      - The View instance (`this` in handlers).
 * @param {string} covsName     - Name of the continuous moderators control.
 * @param {string} factorsName  - Name of the categorical moderators control.
 * @param {string} supplierName - Name of the model Supplier control.
 * @param {string} termsName    - Name of the model Terms ListBox control.
 */
const updateModelTerms = function (
  ui,
  context,
  covsName,
  factorsName,
  supplierName,
  termsName,
) {
  // 1. Combine both variable slots into one list
  var covsList = utils.clone(ui[covsName].value(), []);
  var factorsList = utils.clone(ui[factorsName].value(), []);
  var allVars = covsList.concat(factorsList);

  // 2. Feed the combined list to the Supplier (creates proper
  //    FormattedValue items that the Supplier knows how to render)
  ui[supplierName].setValue(utils.valuesToItems(allVars, FormatDef.variable));

  // 3. Diff against previous state to detect added / removed variables
  var diff = context.findChanges(
    supplierName + "_vars",
    allVars,
    true,
    FormatDef.variable,
  );

  var terms = utils.clone(ui[termsName].value(), []);
  var changed = false;

  // 4. Remove terms that contain any removed variable
  for (var i = 0; i < diff.removed.length; i++) {
    for (var j = terms.length - 1; j >= 0; j--) {
      if (FormatDef.term.contains(terms[j], diff.removed[i])) {
        terms.splice(j, 1);
        changed = true;
      }
    }
  }

  // 5. Auto-add main effects for newly added variables
  for (var i = 0; i < diff.added.length; i++) {
    var newTerm = [diff.added[i]];
    if (!utils.listContains(terms, newTerm, FormatDef.term)) {
      terms.push(newTerm);
      changed = true;
    }
  }

  // 6. Sort terms by length (main effects before interactions)
  if (utils.sortArraysByLength(terms)) {
    changed = true;
  }

  if (changed) {
    ui[termsName].setValue(terms);
  }
};

module.exports = { updateModelTerms };
