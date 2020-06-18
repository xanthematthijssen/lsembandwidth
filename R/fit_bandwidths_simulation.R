fit_bandwidths_simulation <- function(data, bw_vector, moderator_name, moderator_grid,
                                      lavmodel){
  model_list <- list()
  for(i in 1:length(bw_vector)){
    bw <- bw_vector[i]
    model_list[[i]] <- tryCatch(
      train_lsemmodel(
        data = data,
        lavmodel = lavmodel,
        bandwidth = bw,
        moderator_name = moderator_name,
        moderator_grid = moderator_grid
      )$parameters,
      error = function(error){
        print(bw_vector[i])
        print(error)
        return(data.frame(
          grid_index = NA,
          moderator = NA,
          par = NA,
          parindex = NA,
          lhs = NA,
          op = NA,
          rhs = NA,
          est = NA,
          se = NA
        ))
      }
    )

  }
  names(model_list) <- 1:7
  model_df <- do.call(rbind, model_list)
  model_df$bw <-rownames(model_df)
  return(model_df)
}
