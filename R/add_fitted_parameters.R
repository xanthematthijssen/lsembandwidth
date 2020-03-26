data <- simplefactordata
model <- train_lsemmodel( simplefactordata, moderator="moderator",
                          moderator.grid=c(1:9)/10,
                          lavmodel=lavmodel, bandwidth =2)

parameters <- model$parameters

add_fitted_parameters <- function(data,
                                  parameters,
                                  moderator,
                                  moderator.grid){

  parameters <- par


}
