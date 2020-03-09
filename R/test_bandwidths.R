test_bandwidths <- function(data,
                            lavmodel,
                            bandwidthvector,
                            moderator,
                            moderator.grid,
                            statistic = "AIC",
                            ...) {
  if (statistic == "AIC") {
    statistic_vector <-
      sapply(bandwidthvector, function(x) {
        train_lsemmodel(
          data = simplefactordata,
          lavmodel = lavmodel,
          bandwidth = x,
          moderator = "moderator",
          moderator.grid = moderator.grid,
          fit_measures = "aic",
          est_joint = TRUE
        )$fitstats_joint$value
      })
  }

  df <- data.frame(statistic = statistic,
                   bandwidth = bandwidthvector,
                   value = statistic_vector)

  return(df)
}
