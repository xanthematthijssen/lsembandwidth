
## perutation imp

#### fulldata 1
## Permutation imputation
```{r}
# 7 is nog niet gelukt
# 11n is nog niet gelukt
# 13n nog niet gelukt
# 14n niet gelukt

B_imp <- 1000
for (i in 15:30) {
  cat(i)
  cat("a")
  Sys.time() %>% print()
  pmod_cv_int_imp <-
    lsem.permutationTest(
      model_cv_int_imp_all <- lsem.estimate(
        fulldata %>% subset(.imp == i & BEZNR == 1),
        moderator = "inclusiejaar",
        moderator.grid = moderator_grid,
        lavmodel = lavmodel_int,
        bw = 6
      ),
      B = B_imp
    )

  save(
    pmod_cv_int_imp,
    file = paste(
      "../output/perimp/",
      Sys.Date(),
      "B",
      B_imp,
      "i",
      i,
      "pmod_cv_int_imp.RData",
      sep = ""
    )
  )


  cat(i)
  cat("p")
  Sys.time() %>% print()
  pmod_cv_int_imp_pos <-
    lsem.permutationTest(
      lsem.estimate(
        fulldata_pos %>% subset(.imp == i& BEZNR == 1),
        moderator = "inclusiejaar",
        moderator.grid = moderator_grid,
        lavmodel = lavmodel_int,
        bw = 12
      ),
      B = B_imp
    )

  save(
    pmod_cv_int_imp_pos,
    file = paste(
      "../output/perimp/",
      Sys.Date(),
      "B",
      B_imp,
      "i",
      i,
      "pmod_cv_int_imp_pos.RData",
      sep = ""
    )
  )

  cat(i)
  cat("n")
  Sys.time() %>% print()
  pmod_cv_int_imp_neg <-
    lsem.permutationTest(
      lsem.estimate(
        fulldata_neg %>% subset(.imp == i& BEZNR == 1),
        moderator = "inclusiejaar",
        moderator.grid = moderator_grid,
        lavmodel = lavmodel_int,
        bw = 9
      )
      ,
      B = B_imp
    )

  save(
    pmod_cv_int_imp_neg,
    file = paste(
      "../output/perimp/",
      Sys.Date(),
      "B",
      B_imp,
      "i",
      i,
      "pmod_cv_int_imp_neg.RData",
      sep = ""
    )
  )

}
}
