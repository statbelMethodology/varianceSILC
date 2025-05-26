test_that("my_svyrepdesign et my_svymeans", {
  eusilc_design <- my_svyrepdesign(data=eusilc,
    data_jck = eusilc_jck,
    var_poids = "rb050",
    var_strate = "db040",
    var_facteur = "SCALE_JCK",
    var_id = "db030",
    var_JCK = "ID_JCK")

  result <- my_svymean(eusilc_design,~eqIncome)
  expect_equal(dim(result),c(1,4))
  expect_equal(colnames(result),c("indic","N","mean","se"))
  expect_lt(abs(result$se/result$mean - 0.09287157),0.0001)

  result <- my_svymean(eusilc_design,~eqIncome,~rb090)
  expect_equal(dim(result),c(2,5))
  expect_equal(colnames(result),c("indic","rb090","N","mean","se"))
  expect_lt(abs((result$se/result$mean)[1] - 0.08404323),0.0001)
  expect_lt(abs((result$se/result$mean)[2] - 0.10872770),0.0001)

})
