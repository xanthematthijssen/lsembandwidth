calculate_likelihoods <- function(data, RAM_list, moderator){
  moderators <- sapply(RAM_list,function(list) list[["mod_value"]])
  variables <- intersect(names(RAM_list[[1]][["mean_vector"]]),
                         colnames(data))

  mean_list <- lapply(RAM_list,function(list) {list[["mean_vector"]][variables]})
  cov_list <- lapply(RAM_list,function(list) {list[["est_matrix"]][variables, variables]})

  apply(data, 1, function(x){
    index = which(moderators == x[moderator])
    list <- RAM_list[[index]]
    if(!list$mod_value == x[moderator]) cat("selected wrong list")
    mvtnorm::dmvnorm(x[variables], mean = mean_list[[index]], sigma = cov_list[[index]],
            log = TRUE)
  })

}
