#' A simple dataset for a moderated 4 indicator factor model.
#'
#' Contains 4 indicators and 1 moderator and true values of the latent
#'
#'
#' @format A data frame with 1000 rows and 7 variables:
#' \describe{
#'   \item{patnr}{price, in US dollars}
#'   \item{true_latent}{true value of latent used in simulation}
#'   \item{moderator}{value of the moderator between 0 and 1 (runif)}
#'   \item{indicator1}{rnorm with sd of 1 around true_latent}
#'   \item{indicator2}{rnorm with sd of 1 around true_latent}
#'   \item{indicator3}{rnorm with sd of 1 around true_latent*moderator}
#'   \item{indicator4}{rnorm with sd of 1 around true_latent*moderator}
#'   ...
#' }
#'
"simplefactordata"
