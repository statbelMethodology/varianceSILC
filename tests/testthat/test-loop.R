test_that("loop_my_svymean", {
  eusilc_design <- my_svyrepdesign(data=eusilc,
    data_jck = eusilc_jck,
    var_poids = "rb050",
    var_strate = "db040",
    var_facteur = "SCALE_JCK",
    var_id = "db030",
    var_JCK = "ID_JCK")

  # Uniquement des variables d'intérêt
  result <- loop_my_svymean(eusilc_design,c("eqIncome"))

  expect_equal(dim(result),c(1,6))
  expect_equal(colnames(result),c("step","indic","N","mean","se","pop"))
  expect_lt(abs((result$se/result$mean) - 0.09287157),0.0001)

  # Ajout de variables de sous-population
  result <- loop_my_svymean(eusilc_design,
                            c("eqIncome"),
                            c("pl030","rb090"))

  expect_equal(dim(result),c(27,8))
  expect_equal(colnames(result),c("step","indic","pl030","rb090","N","mean","se","pop"))
  expect_lt(abs(sum(result$se/result$mean) - 7.762128),0.0001)
  expect_equal(sum(is.na(result$mean)),0)
  expect_equal(sum(is.na(result$se)),0)

  # Ajout de variables de domaine
  result <- loop_my_svymean(eusilc_design,
                            c("eqIncome"),
                            c("pl030","rb090"),
                            "hsize")

  expect_equal(dim(result),c(161,9))
  expect_equal(colnames(result),c("step","indic","hsize","pl030","rb090","N","mean","se","pop"))
  expect_lt(abs(sum(result$se/result$mean) - 59.12176),0.0001)
  expect_equal(sum(is.na(result$mean)),0)
  expect_equal(sum(is.na(result$se)),0)
})


test_that("loop_svymean works", {
  repweights_matrix <- eusilc_jck %>%
    left_join(eusilc %>% select(db030,rb030),
              by="db030",relationship = "many-to-many") %>%
    select(db030, rb030, PSU_JCK, rb050) %>%
    arrange(PSU_JCK) %>%
    pivot_wider(
      id_cols = c(db030,rb030),
      names_from = PSU_JCK,
      values_from = rb050,
      names_prefix = "WEI_REP_",
      values_fill = 0
    ) %>%
    select(starts_with("WEI_REP_")) %>%
    as.matrix()

  scale_vector <- eusilc_jck %>% select(PSU_JCK,SCALE_JCK) %>%
    arrange(PSU_JCK) %>% distinct() %>% pull(SCALE_JCK)

  eusilc_design <- survey::svrepdesign(
    data = eusilc,
    weights = ~rb050,
    repweights = repweights_matrix,
    scale = 1,
    rscales = scale_vector,
    type="JKn"
  )

  # Uniquement des variables d'intérêt
  result <- loop_svymean(eusilc_design,c("eqIncome"))

  expect_equal(dim(result),c(1,5))
  expect_equal(colnames(result),c("step","indic","mean","se","pop"))
  expect_lt(abs((result$se/result$mean) - 0.005794579),0.0001)

  # Ajout de variables de sous-population
  result <- loop_svymean(eusilc_design,
                         c("eqIncome"),
                         c("pl030","rb090"))

  expect_equal(dim(result),c(27,7))
  expect_equal(colnames(result),c("step","indic","pl030","rb090","mean","se","pop"))
  expect_lt(abs(sum(result$se/result$mean) - 0.6333936),0.0001)
  expect_equal(sum(is.na(result$mean)),0)
  expect_equal(sum(is.na(result$se)),0)

  # Ajout de variables de domaine
  result <- loop_svymean(eusilc_design,
                         c("eqIncome"),
                         c("pl030","rb090"),
                         "hsize")

  expect_equal(dim(result),c(161,8))
  expect_equal(colnames(result),c("step","indic","hsize","pl030","rb090","mean","se","pop"))
  expect_lt(abs(sum(result$se/result$mean) - 12.27932),0.0001)
  expect_equal(sum(is.na(result$mean)),0)
  expect_equal(sum(is.na(result$se)),0)
})
