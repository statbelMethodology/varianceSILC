test_that("creation of eusilc jck sample work", {
  df_ <- eusilc
  jck <- example_eusilc()
  expect_equal(class(df_), "data.frame")
  expect_equal(class(jck), "data.frame")

  # On doit retrouver le même nombre de strate et de DB030
  nb_strates_df_ <- length(unique(df_$db040))
  nb_strates_jck <- length(unique(jck$db040))

  nb_ids_df_ <- length(unique(df_$db030))
  nb_ids_jck <- length(unique(jck$db030))

  expect_equal(nb_strates_df_, nb_strates_jck)
  expect_equal(nb_ids_df_, nb_ids_jck)
})
