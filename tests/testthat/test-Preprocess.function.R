test_that("test the preproocess function", {
  load(system.file("testdata",'CAMAG_dye_mixture.Rdata',package="quanTLC"))
  x = f.preprocess(data$extracted, data$Preprocess.order, data$Preprocess.options)
  
  expect_equal(c(8L, 978L, 4L), dim(x))
})
