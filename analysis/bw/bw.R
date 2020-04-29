load("H:/lsembandwidth/output/workspace/29-04-2020general.RData")

set.seed(121212)
print(Sys.time())

bw_aic <- test_bandwidths(
  data = data_base,
  lavmodel = lavmodel_int,
  bandwidthvector = bandwidths,
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  statistic = "AIC"
)

print(as.data.frame(bw_aic))

save(bw_aic,
     file = paste("../output/bw/", Sys.Date(), "bw_aic.RData", sep = ""))

set.seed(121212)
print(Sys.time())

bw_int <- test_bandwidths(
  data = data_base,
  lavmodel = lavmodel_int3,
  bandwidthvector = c(bandwidths, 2:12),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  statistic = "CV",
  silent = TRUE
)

print(as.data.frame(bw_int))

save(bw_int,
     file = paste("../output/bw/", Sys.Date(), "bw_int3_v2.RData", sep = ""))

set.seed(121212)
print(Sys.time())

bw_int_imp1 <- test_bandwidths(
  data = fulldata1,
  lavmodel = lavmodel_int,
  bandwidthvector = c(bandwidths,2:9),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  statistic = "CV",
  silent = TRUE
)

print(as.data.frame(bw_int_imp1))

save( bw_int_imp1,
      file = paste("../output/bw/", Sys.Date(), "bw_int_imp1.RData", sep = ""))

set.seed(121212)
print(Sys.time())

bw_int_imp1_pos <- 
  test_bandwidths(
    data = fulldata1 %>% subset(EACNUMM %in% EACNUMMs_ab_pos),
    lavmodel = lavmodel_int,
    bandwidthvector = c(bandwidths,2:14),
    moderator_name = "inclusiejaar",
    moderator_grid = moderator_grid,
    statistic = "CV",
    silent = TRUE
  )
print(as.data.frame(bw_int_imp1_pos))

save( bw_int_imp1_pos,
      file = paste("../output/bw/", Sys.Date(), "bw_int_imp1_pos.RData", sep = ""))


set.seed(121212)
print(Sys.time())

bw_int_imp1_neg <- test_bandwidths(
  data = fulldata1 %>% subset(EACNUMM %in% EACNUMMs_ab_neg),
  lavmodel = lavmodel_int,
  bandwidthvector = c(bandwidths,6,7,8,9,11,12,13,14), 
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid,
  statistic = "CV",
  silent = TRUE
)

print(as.data.frame(bw_int_imp1_neg))

save( bw_int_imp1_neg,
      file = paste("../output/bw/", Sys.Date(), "bw_int_imp1_neg.RData", sep = ""))

# ongeveer 8 min
print(Sys.time())
set.seed(121212)
# 2 is het beste 
bw_int_imp1_1j <- test_bandwidths(
  data = fulldata %>% subset(BEZNR == 4 & .imp == 1 & inclusiejaar < 2016),
  lavmodel = lavmodel_int,
  bandwidthvector = c(bandwidths,1.5,2.5,3,3.5,4,4.5,6),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid[moderator_grid < 2015],
  statistic = "CV",
  silent = FALSE
)

print(as.data.frame(bw_int_imp1_1j))

save(bw_int_imp1_1j,
     file = paste("../output/", 
                  Sys.Date(), 
                  "bw_int_imp1_1j.RData", sep = ""))

print(Sys.time())
set.seed(121212)

bw_int_imp1_5j <- test_bandwidths(
  data = fulldata %>% subset(BEZNR == 9 & .imp == 1 & inclusiejaar < 2012),
  lavmodel = lavmodel_int,
  bandwidthvector = c(bandwidths,1.5,2.5,3,3.5,4,4.5),
  moderator_name = "inclusiejaar",
  moderator_grid = moderator_grid[moderator_grid < 2011],
  statistic = "CV",
  silent = TRUE
)

print(as.data.frame(bw_int_imp1_5j))

save(bw_int_imp1_5j,
     file = paste("../output/", 
                  Sys.Date(), 
                  "bw_int_imp1_5j.RData", sep = ""))

print(Sys.time())
set.seed(121212)
