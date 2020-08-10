#simulate_data(type = "rnorm", effect = "equal", N = 300, seed = 121212)
B <- 500

simulation_frame_norm <- data.frame(i = 1:(B*6),
                                    type = rep("rnorm",B*6),
                                    effect = c(rep("equal",B*3),
                                               rep("ascending", B*3)),
                                    N = c(300,500,1000),
                                    seed = 121212 + 1:(B*6))


simulation_norm <- pmap_df(simulation_frame_norm, simulate_data2)

fwrite(simulation_norm, paste("I:/Pi helm/Promovendi/Xanthe/data scriptie/", Sys.Date(), "simulation_norm.csv", sep = ""))

simulation_frame_mix <- data.frame(i = 1:(B*6),
                                    type = rep("mixture",B*6),
                                    effect = c(rep("equal",B*3),
                                               rep("ascending", B*3)),
                                    N = c(300,500,1000),
                                    seed = 121212 + 1:(B*6))


simulation_mix <- pmap_df(simulation_frame_mix, simulate_data2)

fwrite(simulation_mix, paste("I:/Pi helm/Promovendi/Xanthe/data scriptie/", Sys.Date(), "simulation_mix.csv", sep = ""))

simulation_frame_unbalancedmix <- data.frame(i = 1:(B*6),
                                    type = rep("unbalanced mixture",B*6),
                                    effect = c(rep("equal",B*3),
                                               rep("ascending", B*3)),
                                    N = c(300,500,1000),
                                    seed = 121212 + 1:(B*6))


simulation_unbalancedmix<- pmap_df(simulation_frame_unbalancedmix, simulate_data)

fwrite(simulation_unbalancedmix, paste("I:/Promovendi_Annette/Xanthe/data scriptie/", Sys.Date(), "simulation_unbalancedmix.csv", sep = ""))



simulation_frame_increasing <- data.frame(i = 1:(B*6),
                                    type = rep("increasing",B*6),
                                    effect = c(rep("equal",B*3),
                                               rep("ascending", B*3)),
                                    N = c(300,500,1000),
                                    seed = 121212 + 1:(B*6))


simulation_increasing <- pmap_df(simulation_frame_increasing, simulate_data)

fwrite(simulation_increasing, paste("I:/Promovendi_Annette/Xanthe/data scriptie/", Sys.Date(), "simulation_increasing.csv", sep = ""))

simulation_frame_unif <- data.frame(i = 1:(B*6),
                                    type = rep("unif",B*6),
                                    effect = c(rep("equal",B*3),
                                               rep("ascending", B*3)),
                                    N = c(300,500,1000),
                                    seed = 121212 + 1:(B*6))

simulation_unif <- pmap_df(simulation_frame_unif, simulate_data2)

fwrite(simulation_unif, paste("I:/Pi helm/Promovendi/Xanthe/data scriptie/",
                                    Sys.Date(), "simulation_unif.csv", sep = ""))


fread(paste("data/", Sys.Date(), "simulation_norm.csv", sep = ""))

lavmodel <- "
        F=~ indicator1 + indicator2 + indicator3 + indicator4
        F ~~ 1*F"

train_lsemmodel(
  data = simulation_norm[[1]],
  lavmodel = lavmodel,
  bandwidth = 1,
  moderator_name  = "moderator",
  moderator_grid = c(-1,0,1))$parameters


library(extraDistr)
hist(rtriang(1000000, a = -1, b = 1, c = 1), breaks = 10000)
