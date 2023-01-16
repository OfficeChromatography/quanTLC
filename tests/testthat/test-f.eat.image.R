test_that("test f.eat.image with classique rtlc picture", {
  img = f.read.image(system.file("testdata",'rTLC_demopicture.JPG',package="quanTLC"))
  chroms = f.eat.image(img)
  expect_equal(c(20L, 1193L, 4L), dim(chroms))
})
