
#' @importFrom magrittr %>%
#' @importFrom rlang .data


calculate_RAM <- function(df_pars_onemod) {
  variable_vector <- unique(c(df_pars_onemod$lhs, df_pars_onemod$rhs))
  variable_vector <- variable_vector[variable_vector != ""]

  n_vars <- length(variable_vector)

  df_pars_onemod <- dplyr::ungroup(df_pars_onemod)

  fitted_means <- df_pars_onemod %>% dplyr::filter(.data$op == "~1")
  fitted_covs <- df_pars_onemod %>% dplyr::filter(.data$op == "~~")
  fitted_loadings <- df_pars_onemod %>% dplyr::filter(.data$op == "=~")

  mod_value <- df_pars_onemod$sample_mods[1]

  # mean vector
  mean_vector <- fitted_means$weighted_est
  names(mean_vector) <- fitted_means$lhs
  mean_vector <- mean_vector[variable_vector]
  mean_vector[is.na(mean_vector)] <- 0
  names(mean_vector) <- variable_vector

  #covariances
  fitted_covs <-
    rbind(fitted_covs,
          fitted_covs %>%
            dplyr::mutate(hs = .data$lhs,
                          lhs = .data$rhs,
                          rhs = .data$hs) %>%
            dplyr::select(-.data$hs))
  fitted_covs <- fitted_covs[!duplicated(fitted_covs),]

  cov_matrix <-
    make_full_matrix(fitted_covs, variable_vector = variable_vector)

  # loadings
  load_matrix <-
    make_full_matrix(fitted_loadings, variable_vector = variable_vector)

  # combine
  est_matrix <-
    solve(diag(length(variable_vector)) - load_matrix) %*%
    cov_matrix %*% t(solve(diag(length(variable_vector)) - load_matrix))

  return(list(
    mod_value = mod_value,
    mean_vector = mean_vector,
    variable_vector = variable_vector,
    cov_matrix = cov_matrix,
    est_matrix = est_matrix,
    load_matrix = load_matrix
  ))
}
