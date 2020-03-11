#' Fits LSEM models with predifined bandwidths and measures fit
#'
#' @param data dataset for model
#' @param lavmodel Specified SEM in lavaan
#' @param bandwidthvector vector of tested bandwidths
#' @param moderator Variable name of the moderator
#' @param moderator.grid Focal points at which the LSEM should be evaluated.
#' @param statistic can be AIC or CV (cross-validated likelihood)
#' @param ... further arguments to be passed to lavaan::sem or lavaan::lavaan.
#'
#' @return dataframe with fit measures for every bandwidht
#' @export
#'
#' @examples
#'
#'data(simplefactordata)
#'
#'lavmodel <- "
#'         F=~ indicator1 + indicator2 + indicator3 + indicator4
#'         F ~~ 1*F"
#'
#' test_bandwidths(simplefactordata,moderator="moderator",moderator.grid=c(1:9)/10,
#' lavmodel=lavmodel, bandwidthvector = c(1,2), statistic = "AIC")
#'
test_bandwidths <- function(data,
                            lavmodel,
                            bandwidthvector,
                            moderator,
                            moderator.grid,
                            statistic = "AIC",
                            ...) {
  if (statistic == "AIC") {
    statistic_vector <-
      sapply(bandwidthvector,
             function(x) {
               train_lsemmodel(
                 data = data,
                 lavmodel = lavmodel,
                 bandwidth = x,
                 moderator = "moderator",
                 moderator.grid = moderator.grid,
                 fit_measures = "aic",
                 est_joint = TRUE,

               )$fitstats_joint$value
             },
             ...)
  }

  df <- data.frame(statistic = statistic,
                   bandwidth = bandwidthvector,
                   value = statistic_vector)

  return(df)
}


