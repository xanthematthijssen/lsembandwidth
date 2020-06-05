library(devtools)
library(testthat)
library(sirt)
library(data.table)
library(tidyverse)
sessionInfo()


load_all()

#data <- fread("I:/Promovendi_Annette/Xanthe/data scriptie/2020-05-25simulation_norm.csv")

data_list <- data %>%
  dplyr::group_split(i)

lavmodel <- "
        F=~ indicator1 + indicator2 + indicator3 + indicator4
        F ~~ 1*F"

moderator_grid <- c(-2, -1.5, -1.0, - 0.5, 0, 0.5, 1, 1.5, 2)

bw_factor <- c(1.1 , 1.5, 2.0, 2.5, 3.0)
moderator <-  "moderator"

B <- 1000

simulation_frame_norm <- data.frame(i = 1:(B*6),
                                    type = rep("rnorm",B*6),
                                    effect = c(rep("equal",B*3),
                                               rep("ascending", B*3)),
                                    N = c(300,500,1000),
                                    seed = 121212 + 1:(B*6))


simulation_fit <- function(data){
  n <- nrow(data)
  sd <- sd(data$moderator)
  bw <- calculate_bandwidth_hildebrandt(n = n, sd = sd, h = c(1.1 , 1.5, 2.0, 2.5, 3.0, 4.0, 5.0))
  return(bw)
}

bw_list <- data_list %>%
  lapply(., simulation_fit)

fit_bandwidths <- function(data, bw_vector){
  model_list <- list()
  for(i in 1:length(bw_vector)){
    bw <- bw_vector[i]
    model_list[[i]] <-train_lsemmodel( data = data, moderator_name = "moderator",
                                       moderator_grid=c(-2, -1.5, -1.0, - 0.5, 0, 0.5, 1, 1.5, 2),
                                       lavmodel="
        F=~ indicator1 + indicator2 + indicator3 + indicator4
        F ~~ 1*F", bandwidth =bw)$parameters

  }
  names(model_list) <- 1:7
  model_df <- do.call(rbind, model_list)
  model_df$bw <-rownames(model_df)
  return(model_df)
}
names(data_list) <- 1:6000

models <- map2_df(data_list, bw_list, fit_bandwidths, .id = "i")
fwrite(models, file = "I:/Promovendi_Annette/Xanthe/data scriptie/2020-05-28models_norm.csv")
results <- fread("I:/Promovendi_Annette/Xanthe/data scriptie/2020-05-28models_norm.csv")

results <- merge(results, simulation_frame_norm, by = "i")

results$bw <- gsub("\\.", "x", results$bw)

results$bw <- as.numeric(str_split(results$bw, "x", simplify = TRUE)[,1])


bws <- rep(rep(c(1,2,3,4,5,6,7), each = 126), 6000)
# results$bw <- bws

results_small <- results %>% filter(parindex %in% c(2,3,4))

library(tidylog)
results_small <- results_small %>%
  mutate(
    true_val = case_when(
      parindex == 2 ~ 1,
      parindex == 4 ~ 1 + moderator /2,
      parindex == 3 & effect == "equal" ~ 1 + moderator /2,
      parindex == 3 & effect == "ascending" ~ 1 + moderator /4
    )
  )

results_small <- results_small %>%
  mutate(
    diff = est - true_val,
    abs_diff = abs(diff),
    rmse = diff^2
  )

results_small %>%
  group_by(N, effect, parindex, bw) %>%
  summarise(
    mean_diff = mean(diff),
    mean_absdiff = mean(abs_diff),
    mean_rmse = mean(rmse)
  )


results_small %>%
  group_by( bw) %>%
  summarise(
    mean_diff = mean(diff),
    mean_absdiff = mean(abs_diff),
    mean_rmse = mean(rmse)
  )

results_small %>%
  group_by( bw, N) %>%
  summarise(
    mean_diff = mean(diff),
    mean_absdiff = mean(abs_diff),
    mean_rmse = mean(rmse)
  )

results_small %>%
  group_by( effect, bw) %>%
  summarise(
    mean_diff = mean(diff),
    mean_absdiff = mean(abs_diff),
    mean_rmse = mean(rmse)
  )

results_small %>%
  group_by( parindex, bw) %>%
  summarise(
    mean_diff = mean(diff),
    mean_absdiff = mean(abs_diff),
    mean_rmse = mean(rmse)
  )

results_small$condition = paste(results_small$effect, results_small$N, sep = "")

ggplot(subset(results_small, moderator == 0), aes(x = as.factor(bw), y = diff)) +
  geom_boxplot() +
  facet_grid(par ~ N + effect)

ggplot(subset(results_small, moderator == 2), aes(x = as.factor(bw), y = diff)) +
  geom_boxplot() +
  facet_grid(par ~ N + effect)

ggplot(subset(results_small, parindex == 2), aes(x = as.factor(bw), y = diff)) +
  geom_boxplot() +
  facet_grid( N + effect~ moderator)+ ylim(-3,3) +
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "red")


ggplot(subset(results_small, parindex == 3), aes(x = as.factor(bw), y = diff)) +
  geom_boxplot() +
  facet_grid( N + effect~ moderator)+ ylim(-2,2)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "red")


ggplot(subset(results_small, parindex == 4), aes(x = as.factor(bw), y = diff)) +
  geom_boxplot() +
  facet_grid( N + effect~ moderator)+ ylim(-2,2)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "red")

ggplot(subset(results_small, moderator == 2), aes(x = as.factor(bw), y = diff)) +
  geom_boxplot() +
  facet_grid(par ~ N + effect)+ ylim(-2,2)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "red")


ggplot(subset(results_small), aes(x = as.factor(bw), y = diff)) +
  geom_boxplot() +
  facet_grid(par ~ N + effect)+ ylim(-1.5,1.5)+
  theme_bw() +
  geom_hline(yintercept=0, linetype="dashed", color = "red")

ggplot(subset(results_small, moderator == 0), aes(x = as.factor(bw), y = rmse)) +
  geom_boxplot() +
  facet_grid(par ~ N + effect)


ggplot(subset(results_small, moderator == 2), aes(x = as.factor(bw), y = rmse)) +
  geom_boxplot() +
  facet_grid(par ~ N + effect)

ggplot(subset(results_small), aes(x = as.factor(bw), y = rmse)) +
  geom_boxplot() +
  facet_grid(par ~ N + effect) + ylim(0,5)

ggplot(subset(results_small, parindex == 2), aes(x = as.factor(bw), y = rmse)) +
  geom_boxplot() +
  facet_grid(N+effect ~ moderator) + ylim(0,5)

ggplot(subset(results_small, parindex == 3), aes(x = as.factor(bw), y = rmse)) +
  geom_boxplot() +
  facet_grid(N+effect ~ moderator) + ylim(0,1)

ggplot(subset(results_small, parindex == 3), aes(x = as.factor(bw), y = rmse)) +
  geom_boxplot() +
  facet_grid(N+effect ~ moderator) + ylim(0,5)



####################################
debugonce(fit_bandwidths)
fit_bandwidths(data_list[[1]], bw_list[[1]])

data <- data_list[[1000]]
bw <- bw_list[[1]][1]

a <- train_lsemmodel( data = data, moderator_name = "moderator",
                 moderator_grid=moderator_grid,
                 lavmodel=lavmodel, bandwidth =bw)$parameters


debugonce(train_lsemmodel)



