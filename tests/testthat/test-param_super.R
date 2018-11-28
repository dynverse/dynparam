context("test-param_super")

test_that("as_parameter works", {
  expect_error(as_parameter(list(1)), "li does not have name class")
  expect_error(as_parameter(list(class = "hiowqhi")), "li\\$class is not an element of")
  expect_error(as_parameter(list(class = "logical_parameter")), "li does not have names")
  expect_error(as_parameter(list(class = "subset_parameter", id = "ks", description = "halp")), "li does not have names")
  expect_error(as_parameter(list(class = "dddqd", lower = 1, upper = 2, mean = 1, sd = 2)), "li\\$class is not an element of")
})

test_that("is_parameter works", {
  dis <- parameter(id = "param", default = 10)
  expect_true(is_parameter(dis))

  dis <- parameter(id = "param", default = 10, a = 5, description = "value")
  expect_true(is_parameter(dis))

  lis <- list(a = 1, b = 2)
  expect_false(is_parameter(lis))
})

test_that("print and cat works", {
  dis <- parameter(id = "param", default = 10)
  expect_match(as.character(dis), "parameter\\(id = \"param\", default = 10")
  expect_output(print(dis), "parameter\\(id = \"param\", default = 10")
})
