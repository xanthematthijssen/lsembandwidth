#' @importFrom magrittr %>%
#' @importFrom rlang .data

make_full_matrix <- function(parameter_df, variable_vector){

  all_df <- data.frame(sample_mods = -1,
                       par = "test",
                       lhs = rep(variable_vector, times = length(variable_vector)),
                       op = "test",
                       rhs = rep(variable_vector, each = length(variable_vector)),
                       weighted_est = 0
  )

  parameter_df <- rbind(parameter_df, all_df)
  parameter_df$duplicated <-
    duplicated(parameter_df[,c("rhs","lhs")]) |
    duplicated(parameter_df[,c("rhs","lhs")], fromLast = TRUE)

  parameter_df <- dplyr::filter(parameter_df,!(duplicated & .data$par == "test"))
  parameter_df <- dplyr::select(parameter_df,
                                -.data$op,
                                -.data$par,-.data$sample_mods,
                                -.data$duplicated)

  matrix <- tidyr::pivot_wider(
    parameter_df,
    names_from = .data$lhs,
    values_from = .data$weighted_est
  )
  matrix <- as.data.frame(matrix)

  rownames(matrix) <- matrix$rhs

  matrix <- dplyr::select(matrix, -.data$rhs)


  matrix <- matrix[variable_vector,]
  matrix <- matrix[,variable_vector]

  matrix <- as.matrix(matrix)

  return(matrix)

}
