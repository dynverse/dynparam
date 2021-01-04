context("test-param_character")

test_that("correlation method test", {
  p <- character_parameter(
    id = "method",
    default = "pearson",
    values = c("pearson", "spearman", "kendall"),
    description = "Which correlation coefficient to compute."
  )

  expect_is(p, "character_parameter")
  expect_equal(p$id, "method", check.environment = FALSE)
  expect_equal(p$default, "pearson", check.environment = FALSE)
  expect_equal(p$values, c("pearson", "spearman", "kendall"), check.environment = FALSE)
  expect_equal(p$description, "Which correlation coefficient to compute.", check.environment = FALSE)

  expect_match(as.character(p), "character")
  expect_match(as.character(p), "method")
  expect_match(as.character(p), "\\{pearson, spearman, kendall\\}")
  expect_match(as.character(p), "default=pearson")

  li <- as.list(p)

  expect_equal(li$type, "character", check.environment = FALSE)
  expect_equal(li$id, "method", check.environment = FALSE)
  expect_equal(li$default, "pearson", check.environment = FALSE)
  expect_equal(li$values, c("pearson", "spearman", "kendall"), check.environment = FALSE)
  expect_equal(li$description, "Which correlation coefficient to compute.", check.environment = FALSE)

  p2 <- as_parameter(li)
  expect_equal(p2, p, check.environment = FALSE)

  ph <- as_paramhelper(p)

  expect_equal(ph$id, "method", check.environment = FALSE)
  expect_equal(ph$default, "pearson", check.environment = FALSE)
  expect_equal(unlist(ph$values) %>% unname(), c("pearson", "spearman", "kendall"), check.environment = FALSE)
  expect_equal(ph$len, 1, check.environment = FALSE)

  expect_match(get_description(p), "correlation coefficient to compute")
  expect_match(get_description(p), "character")
  expect_match(get_description(p), "kendall")
})

test_that("multiple value test", {
  p <- character_parameter(
    id = "winner",
    default = c("bob", "celine"),
    values = c("alice", "bob", "celine", "david", "eric", "filip"),
    description = "persons"
  )

  expect_equal(p$id, "winner", check.environment = FALSE)
  expect_equal(p$default, c("bob", "celine"), check.environment = FALSE)
  expect_equal(p$values, c("alice", "bob", "celine", "david", "eric", "filip"), check.environment = FALSE)
  expect_equal(p$description, "persons", check.environment = FALSE)

  expect_match(as.character(p), "character")
  expect_match(as.character(p), "winner")
  expect_match(as.character(p), "\\{alice, bob, celine, david, eric, filip\\}")
  expect_match(as.character(p), "default=\\{bob, celine\\}")

  li <- as.list(p)

  expect_equal(li$type, "character", check.environment = FALSE)
  expect_equal(li$id, "winner", check.environment = FALSE)
  expect_equal(li$default, c("bob", "celine"), check.environment = FALSE)
  expect_equal(li$values, c("alice", "bob", "celine", "david", "eric", "filip"), check.environment = FALSE)
  expect_equal(li$description, "persons", check.environment = FALSE)

  p2 <- as_parameter(li)
  expect_equal(p2, p, check.environment = FALSE)

  ph <- as_paramhelper(p)

  expect_equal(ph$id, "winner", check.environment = FALSE)
  expect_equal(unlist(unname(ph$default)), c("bob", "celine"), check.environment = FALSE)
  expect_equal(unlist(unname(ph$values)), c("alice", "bob", "celine", "david", "eric", "filip"), check.environment = FALSE)
  expect_equal(ph$len, 2, check.environment = FALSE)
})

test_that("wrong parse fails gracefully", {
  expect_error(character_parameter(id = "a", default = "b", values = "c", description = "d"), "default is missing 1 element from values")
  expect_error(character_parameter(id = "a", default = "b", values = c("c", "d"), description = "d"), "default is missing 1 element from values")
  expect_error(character_parameter(id = 1, default = "b", values = "b", description = "d"), "id is not a character vector")
  expect_error(character_parameter(id = "a", default = 1, values = "b", description = "d"), "default is not a character vector")
  expect_error(character_parameter(id = "a", default = "b", values = 1, description = "d"), "values is not a character vector")
  expect_error(character_parameter(id = "a", default = "b", values = "b", description = 1), "description is not NULL or description is not a character vector")
})

