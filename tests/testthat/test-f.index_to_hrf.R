test_that("test f.index_to_hrf", {
  x = f.index_to_hrf(688,100,8,70,array(dim=c(5,978,4)))
  expect_equal(35, x)
})
