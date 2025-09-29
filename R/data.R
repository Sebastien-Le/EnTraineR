#' River deforestation: air and water temperatures before/after
#'
#' @description
#' Monitoring data of water and air temperatures before and after riparian
#' deforestation. Useful to illustrate linear regression with an interaction
#' (Temp_air * Deforestation).
#'
#' @format
#' A data frame with 56 rows and 3 variables:
#' \describe{
#'   \item{Temp_water}{numeric; water temperature (deg C).}
#'   \item{Temp_air}{numeric; air temperature (deg C).}
#'   \item{Deforestation}{factor with 2 levels: "BEFORE", "AFTER". 28 periods each.}
#' }
#'
#' @details
#' Brief summary (indicative): Temp_water min ~ 0.55, median ~ 9.28, max ~ 18.89;
#' Temp_air min ~ -3.04, median ~ 6.53, max ~ 15.75.
#'
#' @usage data(deforestation)
#' @docType data
#' @name deforestation
#'
#' @examples
#' data(deforestation)
#' str(deforestation)
#' table(deforestation$Deforestation)
#'
#' @examplesIf requireNamespace("FactoMineR", quietly = TRUE)
#' # Linear model with interaction (FactoMineR):
#' fit <- FactoMineR::LinearModel(
#'   Temp_water ~ Temp_air * Deforestation,
#'   data = deforestation,
#'   selection = "none"
#' )
#' print(fit)
#'
#' @keywords datasets
"deforestation"


#' Ham: sensory descriptors and overall liking
#'
#' @description
#' Sensory profile of hams (quantitative attributes) and an overall liking score.
#' Useful to illustrate multiple regression and the joint reading of per-term F
#' tests and coefficient T tests.
#'
#' @format
#' A data frame with 21 rows (hams) and 15 variables:
#' \describe{
#'   \item{Juiciness}{numeric}
#'   \item{Crispy}{numeric}
#'   \item{Tenderness}{numeric}
#'   \item{Pasty}{numeric}
#'   \item{Fibrous}{numeric}
#'   \item{Salty}{numeric}
#'   \item{Sweet}{numeric}
#'   \item{Meaty}{numeric}
#'   \item{Seasoned}{numeric}
#'   \item{Metallic}{numeric}
#'   \item{Ammoniated}{numeric}
#'   \item{Fatty}{numeric}
#'   \item{Braised}{numeric}
#'   \item{Lactic}{numeric}
#'   \item{Overall liking}{numeric; overall acceptability score}
#' }
#'
#' @details
#' Brief summary (indicative): median Juiciness ~ 3.0; median Tenderness ~ 6.0;
#' mean Salty ~ 5.52; median Overall liking ~ 6.5.
#'
#' @usage data(ham)
#' @docType data
#' @name ham
#'
#' @examples
#' data(ham)
#' summary(ham)
#'
#' @examplesIf requireNamespace("FactoMineR", quietly = TRUE)
#' # Multiple regression without selection (FactoMineR):
#' fit <- FactoMineR::LinearModel(
#'   `Overall liking` ~ .,
#'   data = ham,
#'   selection = "none"
#' )
#' print(fit)
#'
#' @keywords datasets
"ham"


#' Poussin: weight by brooding temperature and sex
#'
#' @description
#' Chick weights measured under three brooding temperatures, with sex recorded.
#' Useful for ANOVA and linear models with categorical factors.
#'
#' @format
#' A data frame with 45 rows and 3 variables:
#' \describe{
#'   \item{Temperature}{factor with 3 levels: "T1", "T2", "T3" (15 each).}
#'   \item{Gender}{factor with 2 levels: "Female", "Male" (about 20 and 25).}
#'   \item{Weight}{numeric; weight (units as provided).}
#' }
#'
#' @details
#' Brief summary (indicative): Weight min ~ 15, median ~ 23, max ~ 33.
#'
#' @usage data(poussin)
#' @docType data
#' @name poussin
#'
#' @examples
#' data(poussin)
#' with(poussin, table(Temperature, Gender))
#' boxplot(Weight ~ Temperature, data = poussin,
#'         main = "Poussin weight by temperature")
#' # Two-factor ANOVA (base stats):
#' fit <- stats::aov(Weight ~ Temperature * Gender, data = poussin)
#' summary(fit)
#'
#' @keywords datasets
"poussin"
