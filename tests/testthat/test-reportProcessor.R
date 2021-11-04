test_that("report processor provides a file", {
  expect_true(file.exists(reportProcessor("veiledning", outputType = "html")))
})
