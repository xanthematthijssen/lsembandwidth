#' Fits LSEM model on training set with predefined bandwidth
#'
#' @param data training dataset
#' @param lavmodel Specified SEM in lavaan
#' @param bandwidth Bandwidth factor
#' @param moderator_name Variable name of the moderator
#' @param moderator_grid Focal points at which the LSEM should be evaluated.
#' @param ... Further arguments to be passed to lavaan::sem or lavaan::lavaan.
#' @return fitted LSEM model
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
#' train_lsemmodel(simplefactordata,moderator_name="moderator",moderator_grid=c(1:9)/10,
#' lavmodel=lavmodel, bandwidth=2)
#'
#'
#'
train_lsemmodel <- function(data,
                            lavmodel,
                            bandwidth,
                            moderator_name,
                            moderator_grid,
                            ...) {
  model <- sirt::lsem.estimate(
    data = data,
    moderator = moderator_name,
    moderator.grid = moderator_grid,
    lavmodel = lavmodel,
    bw = bandwidth,
    verbose = FALSE,
    est_joint = TRUE,
    ...
  )
  return(model)

}
