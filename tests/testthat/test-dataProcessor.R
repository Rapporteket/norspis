test_that("an organization name can be added to a data set", {
  df <- data.frame(AvdRESH = c(1, 2, 3))
  id <- c(1, 2, 3)
  name <- c("A", "B", "C")
  expect_true(class(addDeptName(df, id, name)) == "data.frame")
  expect_true(addDeptName(df, id, name)$deptName[2] == "B")
})
