# EnTraineR 0.9.0

Major pre-CRAN update with bug fixes, improved prompts, and Gemini API support.

## Highlights
- **New:** `gemini_generate()` — minimal, robust wrapper for Google Gemini
  (Generative Language API) to programmatically generate LLM responses from R.
  - Normalizes model ids (accepts `"gemini-2.5-flash"` or `"models/gemini-2.5-flash"`).
  - Correct endpoint (`v1beta/models/{model}:generateContent`) and query key.
  - Config params: `temperature`, `top_p`, `top_k`, `max_output_tokens`,
    `stop_sequences`, `system_instruction`, `seed`, `timeout`, `verbose`.
  - Adds polite **User-Agent** header by default:
    `EnTraineR/0.9.0 (https://github.com/Sebastien-Le/EnTraineR)`.
  - Safer parsing: handles empty/blocked candidates and reports `finishReason`
    or safety blocks with clear errors.

## Bug fixes & robustness
- Fixed occasional header-loss in filtered ANOVA T-test tables; always re-attaches
  the column header exactly once when filtering.
- Resolved errors in `trainer_LinearModel()` when global-fit lines were
  partially printed (missing df or p). Now defensive against missing fields.
- Stabilized audience sections across trainers (BEGINNER/APPLIED/ADVANCED) with
  consistent wording and limits (e.g., “Takeaway <=5 bullets”).
- Cleaned up duplicate attachment issues in verbatim blocks.
- Improved error messages when external APIs return 404 or safety-filtered results.

## Documentation & examples
- Expanded examples for:
  - `trainer_LinearModel()` (ham and deforestation case studies).
  - `trainer_AovSum()` (sensory chocolates and poussin datasets).
- Added dataset docs for **deforestation**, **ham**, **poussin** with guarded
  examples for optional dependencies.
- DESCRIPTION improved (clear package blurb) and added `Imports: httr2`
  to satisfy namespace checks for `gemini_generate()`.

## Internal / developer notes
- Consistent prompt skeleton (context → setup/how-to-read → verbatim → output requirements).
- Audience profiles aligned between ANOVA and LinearModel (hierarchy rules,
  deviation coding, partial vs global tests).
- Safer environment handling for keys: looks up `GEMINI_API_KEY` by default.

---

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
