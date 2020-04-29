

## Permutation 1set


B <- 1000
print(Sys.time())
pmod_cv_int <- lsem.permutationTest(model_cv_int_pmod,
                                    B = B)



pmod_cv_int$teststat %>% print(digits = 3)


pmod_cv_int_pos <- lsem.permutationTest(model_cv_int_pmod_pos,
                                        B = B)



pmod_cv_int_pos$teststat %>% print(digits = 3)

pmod_cv_int_neg <- lsem.permutationTest(model_cv_int_pmod_neg,
                                        B = 1000)



pmod_cv_int_neg$teststat %>% print(digits = 3)

pmod_cv_int_pos <- lsem.permutationTest(model_cv_int_pmod,
                                        B = B)



pmod_cv_int$teststat %>% print(digits = 3)

pmod_cv_int2 <- lsem.permutationTest(model_cv_int_pmod2,
                                     B = B)



pmod_cv_int2$teststat %>% print(digits = 3)
plot(pmod_cv_int2)

pmod_cv_imp <- lsem.permutationTest(model_cv_imp,
                                    B = B)



pmod_cv_imp$teststat %>% print(digits = 3)

pmod_cv_imp_bw2 <- lsem.permutationTest(model_cv_imp_bw2,
                                        B = 1000)



pmod_cv_imp_bw2$teststat %>% print(digits = 3)

pmod_cv_imp_1j <- lsem.permutationTest(model_cv_imp_1j,
                                       B = 1000)



pmod_cv_imp_1j$teststat %>% print(digits = 3)


pmod_cv_imp_5j <- lsem.permutationTest(model_cv_imp_5j,
                                       B = 1000)



pmod_cv_imp_5j$teststat %>% print(digits = 3)


pmod_cv_imp_pos <- lsem.permutationTest(model_cv_imp_pos,
                                        B = B)



pmod_cv_imp_pos$teststat %>% print(digits = 3)

pmod_cv_imp_neg <- lsem.permutationTest(model_cv_imp_neg,
                                        B = B)



pmod_cv_imp_neg$teststat %>% print(digits = 3)


#
# pmod_hild_int <-
#   lsem.permutationTest(model_hild_int,
#                        B = B)
#
#
# pmod_hild_int$teststat %>% print(digits = 3)
#



  # save(pmod_hild_int,
  #      file = paste("../output/",
  #                   Sys.Date(),
  #                   B,
  #                   "pmod_hild_int.RData", sep = ""))

  save(pmod_cv_int,
       file = paste("../output/pmods/",
                    Sys.Date(),
                    "B",B,
                    "pmod_cv_int.RData", sep = ""))

  save(pmod_cv_int_pos,
       file = paste("../output/pmods/",
                    Sys.Date(),
                    "B",B,
                    "pmod_cv_int_pos.RData", sep = ""))

  save(pmod_cv_int_neg,
       file = paste("../output/pmods/",
                    Sys.Date(),
                    "B",B,
                    "pmod_cv_int_neg.RData", sep = ""))

  save(pmod_cv_int2,
       file = paste("../output/pmods/",
                    Sys.Date(),
                    "B",B,
                    "pmod_cv_int2.RData", sep = ""))

  save(pmod_cv_imp,
       file = paste("../output/pmods/",
                    Sys.Date(),
                    "B",B,
                    "pmod_cv_imp_set1.RData", sep = ""))
  save(pmod_cv_imp_bw2,
       file = paste("../output/pmods/",
                    Sys.Date(),
                    "B",1000,
                    "pmod_cv_imp_set1_bw2.RData", sep = ""))

  save(pmod_cv_imp_5j,
       file = paste("../output/pmods/",
                    Sys.Date(),
                    "B",1000,
                    "pmod_cv_imp_set1_5j.RData", sep = ""))

  save(pmod_cv_imp_1j,
       file = paste("../output/pmods/",
                    Sys.Date(),
                    "B",1000,
                    "pmod_cv_imp_set1_1j.RData", sep = ""))


  save(
    pmod_cv_imp_pos,
    file = paste(
      "../output/pmods/",
      Sys.Date(),
      "B",B,
      "pmod_cv_imp_pos_set1.RData",
      sep = ""
    )
  )

  save(
    pmod_cv_imp_neg,
    file = paste(
      "../output/pmods/",
      Sys.Date(),
      "B",B,
      "pmod_cv_imp_neg_set1.RData",
      sep = "") )


## 29-04-2020 nog niet goede bandwidth

  pmod_cv_imp_3j <- lsem.permutationTest(model_cv_imp_3j,
                                         B = 1000)



  pmod_cv_imp_3j$teststat %>% print(digits = 3)


  save(pmod_cv_imp_3j,
       file = paste("../output/pmods/",
                    Sys.Date(),
                    "B",1000,
                    "pmod_cv_imp_set1_3j.RData", sep = ""))

