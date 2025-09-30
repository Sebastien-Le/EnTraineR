# EnTraineR

An intelligent teaching assistant based on LLMs to help interpret statistical model outputs in R.  
EnTraineR builds audience-aware prompts (beginner, applied, advanced) that **never invent numbers**: it passes verbatim outputs from R and instructs how to explain them.

> Works out-of-the-box to produce high-quality prompts.  
> Optionally, you can connect your own LLM backend (via your functions built on top of `trainer_core_generate_or_return()`).

---

## Installation

From GitHub:

```r
# install.packages("remotes")
remotes::install_github("Sebastien-Le/EnTraineR")
```

Optional but recommended packages for examples:
- `FactoMineR`, `SensoMineR` (model objects used in examples)
- `stringr` (to squish multi-line intros)

---

## What it does

- **Generates clean prompts** to interpret:
  - ANOVA summaries (`AovSum`) with F-tests and T-tests
  - Linear models (`FactoMineR::LinearModel`) including model selection notes
  - Classical tests: t-test, variance F-test, proportion test, correlation test, chi-squared test
- **Audience-aware** guidance:
  - `beginner`: plain-language teaching focus
  - `applied`: decisions and practical implications
  - `advanced`: technical but concise, with appropriate cautions
- **No invented numbers**: only uses the verbatim output you provide.

---

## Included datasets

The package ships 3 small datasets for teaching:

- `deforestation`  
  Air and water temperatures before/after riparian deforestation.  
  Variables: `Temp_water`, `Temp_air`, `Deforestation` (BEFORE/AFTER).

- `ham`  
  Sensory descriptors for 21 hams and an `Overall liking` score.  
  Useful for multiple regression demonstrations.

- `poussin`  
  Chick weights by brooding `Temperature` (T1/T2/T3) and `Gender` (Female/Male).  
  Useful for two-factor ANOVA examples.

```r
data(deforestation); str(deforestation)
data(ham); summary(ham)
data(poussin); with(poussin, table(Temperature, Gender))
```

---

## Quick start

### 1) ANOVA (AovSum)

```r
# install.packages("SensoMineR")
library(SensoMineR)
data(chocolates)

# Build AovSum (example similar to chocolates::Granular ~ Product*Panelist)
res <- AovSum(Granular ~ Product*Panelist, data = chocolates)

intro <- "Six chocolates were evaluated by a sensory panel on a granular attribute."
intro <- gsub("\n", " ", intro)

p <- trainer_AovSum(
  aovsum_obj   = res,
  audience     = "applied",
  t_test       = c("Product", "Panelist"),  # filter T-test section
  introduction = intro
)

cat(p)   # a ready-to-use prompt for an LLM or for teaching
```

### 2) Linear model (FactoMineR::LinearModel)

```r
# install.packages("FactoMineR"); install.packages("stringr")
library(FactoMineR)

intro_ham <- "Can we predict ham overall liking from its sensory profile?"
intro_ham <- stringr::str_squish(gsub("\n", " ", intro_ham))

fit <- LinearModel(`Overall liking` ~ ., data = ham, selection = "bic")

pr <- trainer_LinearModel(
  lm_obj       = fit,
  introduction = intro_ham,
  audience     = "advanced"
)

cat(pr)
```

Another linear model with interaction and a categorical factor:

```r
fit2 <- LinearModel(Temp_water ~ Temp_air * Deforestation,
                    data = deforestation, selection = "none")

pr2 <- trainer_LinearModel(
  lm_obj       = fit2,
  introduction = "Effect of deforestation on the air-water temperature link.",
  audience     = "beginner"
)

cat(pr2)
```

### 3) Classical tests

t-test:
```r
tt <- t.test(rnorm(20, 0.1), mu = 0)
cat(trainer_t_test(tt, audience = "beginner"))
```

Variance F-test:
```r
vt <- var.test(rnorm(25, sd=1.0), rnorm(30, sd=1.3))
cat(trainer_var_test(vt, audience = "applied"))
```

Proportion test:
```r
pt <- prop.test(x = c(42, 35), n = c(100, 90))
cat(trainer_prop_test(pt, audience = "advanced", summary_only = TRUE))
```

Correlation test:
```r
set.seed(1)
x <- rnorm(30); y <- 0.5*x + rnorm(30, sd = 0.8)
ct <- cor.test(x, y, method = "pearson")
cat(trainer_cor_test(ct, audience = "applied"))
```

Chi-squared test:
```r
m <- matrix(c(10, 20, 30, 40), nrow = 2)
cx <- chisq.test(m, correct = TRUE)
cat(trainer_chisq_test(cx, audience = "beginner"))
```

---

## Audience profiles (summary)

- **beginner**: plain English, define what is tested, minimal jargon, short sentences.
- **applied**: decisions and implications, keep it practical, concise takeaways.
- **advanced**: technical but clear, mention df/F where relevant, caution about multiplicity/assumptions without inventing diagnostics.

All prompts emphasize: **do not invent numbers**; use only what appears in the printed output.

---

## Reproducibility and LLMs

By default, trainers return a **prompt string** (i.e., `generate = FALSE`).  
If you have a generator backend, you can pass `generate = TRUE` and a `llm_model` name; implement your own `trainer_core_generate_or_return()` to call your LLM API.

---

## Contributing

Issues and pull requests are welcome. Please:
- Keep code ASCII and roxygen2-ready.
- Add tests/examples where relevant.
- Follow the audience style guidelines.

---

## License and citation

See the `DESCRIPTION` file for license terms.  
If EnTraineR helps your teaching or analyses, starring the repo is appreciated.

---

## Acknowledgments

Thanks to the R community and the authors of `FactoMineR` and `SensoMineR` for inspiring teaching tools and example datasets used in demonstrations.
