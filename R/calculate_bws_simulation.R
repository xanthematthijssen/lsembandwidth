calculate_bws_simulation <- function(data, bw_factor){
  n <- nrow(data)
  sd <- sd(data$moderator)
  bw <- calculate_bandwidth_hildebrandt(n = n, sd = sd, h = bw_factor)
  return(bw)
}
