test_that("adjust_text() works", {
  expect_equal(adjust_text(text = "abc\ndef", flag = "\n"), "abc ...")
  expect_equal(adjust_text(text = "abc.", flag = "."), "abc.")
  expect_equal(adjust_text(text = "abc.", flag = "\n"), "abc.")
})
