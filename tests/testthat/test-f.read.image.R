test_that("Check f.read.image function with the classic rtlc file", {
  img = f.read.image(system.file("testdata",'rTLC_demopicture.JPG',package="quanTLC"))
  expect_equal(c(1193L, 2385L, 3L), dim(img))
})
