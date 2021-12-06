test_that("report processor provides files", {
  expect_true(
    file.exists(reportProcessor("veiledning", outputType = "html",
                                title = "Unit test"))
  )
  expect_true(
    file.exists(reportProcessor("eksSamlerapport", outputType = "html",
                                title = "Unit test"))
  )
})
