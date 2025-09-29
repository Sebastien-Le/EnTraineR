# trainer 0.1.0

First CRAN release.

## New features
* Audience-aware prompt builders for common analyses:
  - `trainer_AovSum()` for ANOVA tables (FactoMineR::AovSum).
  - `trainer_LinearModel()` for multiple linear regression (FactoMineR::LinearModel)
    with global fit summary + per-term F/T sections and optional AIC/BIC selection.
  - `trainer_t_test()` for `stats::t.test`.
  - `trainer_var_test()` for `stats::var.test`.
  - `trainer_prop_test()` for `stats::prop.test`.
  - `trainer_cor_test()` for `stats::cor.test` (Pearson/Spearman/Kendall).
  - `trainer_chisq_test()` for `stats::chisq.test` (GOF and contingency tables).
* All trainers support three audiences: "beginner", "applied", "advanced".
  - Consistent language, alpha handling, and no invented numbers.
  - Optional `summary_only = TRUE` for 3-bullet executive summaries.

## Data
* Add three documented datasets for teaching and examples:
  - `deforestation`: water/air temperatures before vs after riparian clearing.
  - `ham`: sensory descriptors and overall liking (multiple regression).
  - `poussin`: chick weight by brooding temperature and sex (ANOVA).

## Quality and robustness
* Clear separation of orientation/setup, verbatim output, and output requirements.
* ANOVA `T-test` section:
  - Correctly re-attaches column header after filtering; clear "(filtered)" title and scope message.
* LinearModel:
  - Includes verbatim model-fit summary (RSE, R^2, F, p, AIC/BIC).
  - Explains partial (added last) F/T tests and interaction hierarchy.
  - Selection notes (AIC/BIC): kept/dropped RHS terms and deltas (AIC/BIC/RSE/df).
* Proportion and correlation tests:
  - Audience-specific guidance, CI wording, and small-sample cautions.
* Chi-squared test:
  - Branches for GOF vs contingency; notes on Yates correction and Monte Carlo p-values.

## Documentation
* Roxygen2 docs for all trainers and datasets.
* Examples guard optional dependencies with `requireNamespace("FactoMineR", quietly = TRUE)`.

## Internals
* Shared prompt skeleton via trainer core helpers (audience profile, headers, formatting).
* Consistent phrasing across trainers; improved error checks on inputs.
