if(!require(devtools)){install.packages("devtools"); library(devtools)}
load_all()
if(!require(mice)){install.packages("mice"); library(mice)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}

rm(list=ls())
set.seed(121212)

calculate_bandwidth <- function(n, sd, h){
  bw <- (h* sd )/ n^0.2
  return(bw)
}

load("I:/Pi helm/Promovendi/Xanthe/4. Analyse hele EAC/EAC-nieuwe-start/0. Data/Produced/Beforeimp 2019-01-16.RData")

load("I:/Pi helm/Promovendi/Xanthe/4. Analyse hele EAC/EAC-nieuwe-start/0. Data/Produced/Naimp 2019-01-16.RData")

fulldata <- mice::complete(data_imp, include = T, action = 'long')


data <- data %>% mutate(inclusiejaar = format.Date(DAT1EBEZ,"%Y") %>% as.numeric)


data <- data %>% mutate(
  TJC_conv = 0.56*sqrt(TJC28),
  SJC_conv = 0.28*sqrt(SJC28),
  ESR_conv = 0.70*logbse,
  GH_conv = 0.014*VASZACT
)


data <- data %>% select(EACNUMM, BEZNR, TJC28, SJC28, BSE, VASZACT,TJC_conv:GH_conv, inclusiejaar,
                        Age_incl,geslacht, sympt_dur_dgn, CCP2np, rf_neg_pos, das28, HAQ)

data_base <- data %>% filter(BEZNR ==1)



fulldata <- fulldata %>% mutate(
  TJC_conv = 0.56*sqrt(TJC28),
  SJC_conv = 0.28*sqrt(SJC28),
  logbse = if_else(BSE > 0, log(BSE), 0),
  ESR_conv = 0.70*logbse,
  GH_conv = 0.014*VASZACT
)


fulldata <- fulldata %>% select(EACNUMM, BEZNR, TJC28, SJC28, BSE, VASZACT, TJC_conv:GH_conv, .imp,
                                Age_incl,geslacht, sympt_dur_dgn, CCP2np, rf_neg_pos, das28, HAQ)
fulldata <- full_join(fulldata, select(data_base, EACNUMM, inclusiejaar))

fulldata1 <- fulldata %>% filter(BEZNR ==1 & .imp == 1)


lavmodel_int <- "
        F=~ 1*ESR_conv+SJC_conv+TJC_conv+GH_conv
        ESR_conv ~ 0
        SJC_conv ~ 1
        TJC_conv ~ 1
        GH_conv  ~ 1
        F ~ 1"


moderator_grid <- 1994:2016

bandwidths <- c(1,2,3,4,5,6,7)

hildebrandt_bw <- calculate_bandwidth(n = nrow(data_base),
                                      sd = sd(data_base$inclusiejaar),
                                      h = c(1.1,2,3))

hildebrandt_bw4 <- calculate_bandwidth(n = nrow(fulldata %>% subset(BEZNR == 4 & .imp == 1)),
                    sd = sd(data_base$inclusiejaar),
                    h = c(1.1,2,3))



hildebrandt_bw9 <- calculate_bandwidth(n = nrow(fulldata %>% subset(BEZNR == 9 & .imp == 1)),
                    sd = sd(data_base$inclusiejaar),
                    h = c(1.1,2,3))




deviances_base <- test_bandwidths(
  data = data_base,
  lavmodel = lavmodel_int,
  bandwidthvector =  c(bandwidths, hildebrandt_bw),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  K = 5,
  statistic = "CV",
  silent = T
)

deviances_base
write.csv2(deviances_base, "deviance_base.csv")


deviances_base_full <- test_bandwidths(
  data = fulldata1,
  lavmodel = lavmodel_int,
  bandwidthvector = c(bandwidths, hildebrandt_bw),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  K = 5,
  statistic = "CV",
  silent = T
)

write.csv2(deviances_base_full, "deviance_base_full.csv")


deviances_base_full4 <- test_bandwidths(
  data = fulldata %>% subset(BEZNR == 4 & .imp == 1),
  lavmodel = lavmodel_int,
  bandwidthvector = c(bandwidths, hildebrandt_bw4),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  K = 5,
  statistic = "CV",
  silent = T
)

write.csv2(deviances_base_full4, "deviance_base_full4.csv")

deviances_base_full9 <- test_bandwidths(
  data = fulldata %>% subset(BEZNR == 9 & .imp == 1),
  lavmodel = lavmodel_int,
  bandwidthvector = c(bandwidths, hildebrandt_bw9),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  K = 5,
  statistic = "CV",
  silent = T
)

write.csv2(deviances_base_full9, "deviance_base_full9.csv")


####################  ACPA baseline


deviances_base_pos <- test_bandwidths(
  data = data_base %>% subset(CCP2np == 1),
  lavmodel = lavmodel_int,
  bandwidthvector =  c(bandwidths, hildebrandt_bw),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  K = 5,
  statistic = "CV",
  silent = T
)

deviances_base_pos
write.csv2(deviances_base_pos, "deviances_base_pos.csv")


deviances_base_neg <- test_bandwidths(
  data = data_base %>% subset(CCP2np == 0),
  lavmodel = lavmodel_int,
  bandwidthvector =  c(bandwidths, hildebrandt_bw),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  K = 5,
  statistic = "CV",
  silent = T
)

deviances_base_neg
write.csv2(deviances_base_neg, "deviances_base_neg.csv")
