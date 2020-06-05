calculate_bandwidth_hildebrandt <- function(n, sd, h){
  bw <- (h* sd )/ n^0.2
  return(bw)
}
