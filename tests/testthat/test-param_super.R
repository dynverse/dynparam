context("test-param_super")

test_that("as_parameter works", {
  expect_error(as_parameter(list(1)), "li does not have.*name.*type")
  expect_error(as_parameter(list(type = "hiowqhi")), "li\\$type is missing 1 element from names\\(parameters\\)")
  expect_error(as_parameter(list(type = "logical")), "li is missing 2 names from required_args")
  expect_error(as_parameter(list(type = "subset", id = "ks", description = "halp")), "li is missing 2 names from required_args")
  expect_error(as_parameter(list(type = "dddqd", lower = 1, upper = 2, mean = 1, sd = 2)), "li\\$type is missing 1 element from")
})

test_that("is_parameter works", {
  dis <- parameter(id = "param", default = 10)
  expect_true(is_parameter(dis))

  dis <- parameter(id = "param", default = 10, a = 5, description = "value")
  expect_true(is_parameter(dis))

  lis <- list(a = 1, b = 2)
  expect_false(is_parameter(lis))

  expect_match(get_description(dis), "Value")
  expect_match(get_description(dis), "abstract")
  expect_match(get_description(dis), "10")
})

test_that("print and cat works", {
  dis <- parameter(id = "param", default = 10)
  expect_match(as.character(dis), "param \\| type=abstract \\| domain=NA \\| default=10")
  expect_output(print(dis), "param \\| type=abstract \\| domain=NA \\| default=10")
})
