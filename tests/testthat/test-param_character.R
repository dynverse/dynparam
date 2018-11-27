context("test-param_character")

test_that("correlation method test", {
  p <- character_parameter(
    id = "method",
    default = "pearson",
    values = c("pearson", "spearman", "kendall"),
    description = "Which correlation coefficient to compute."
  )

  expect_is(p, "character_parameter")
  expect_equal(p$id, "method")
  expect_equal(p$default, "pearson")
  expect_equal(p$values, c("pearson", "spearman", "kendall"))
  expect_equal(p$description, "Which correlation coefficient to compute.")

  expect_match(as.character(p), "character")
  expect_match(as.character(p), "method")
  expect_match(as.character(p), "\\{pearson, spearman, kendall\\}")
  expect_match(as.character(p), "default=pearson")

  li <- as_list(p)

  expect_equal(li$class, "character_parameter")
  expect_equal(li$id, "method")
  expect_equal(li$default, "pearson")
  expect_equal(li$values, c("pearson", "spearman", "kendall"))
  expect_equal(li$description, "Which correlation coefficient to compute.")

  p2 <- list_as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)
  ph1 <- ph$params[[1]]

  expect_equal(ph1$id, "method")
  expect_equal(ph1$default, "pearson")
  expect_equal(unlist(ph1$values) %>% unname(), c("pearson", "spearman", "kendall"))
  expect_equal(ph1$len, 1)
})

test_that("winner test", {
  p <- character_parameter(
    id = "winner",
    default = c("bob", "celine"),
    values = c("alice", "bob", "celine", "david", "eric", "filip"),
    description = "persons"
  )

  expect_equal(p$id, "winner")
  expect_equal(p$default, c("bob", "celine"))
  expect_equal(p$values, c("alice", "bob", "celine", "david", "eric", "filip"))
  expect_equal(p$description, "persons")

  expect_match(as.character(p), "character")
  expect_match(as.character(p), "winner")
  expect_match(as.character(p), "\\{alice, bob, celine, david, eric, filip\\}")
  expect_match(as.character(p), "default=\\{bob, celine\\}")

  li <- as_list(p)

  expect_equal(li$class, "character_parameter")
  expect_equal(li$id, "winner")
  expect_equal(li$default, c("bob", "celine"))
  expect_equal(li$values, c("alice", "bob", "celine", "david", "eric", "filip"))
  expect_equal(li$description, "persons")

  p2 <- list_as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)
  ph1 <- ph$params[[1]]

  expect_equal(ph1$id, "winner")
  expect_equal(unlist(unname(ph1$default)), c("bob", "celine"))
  expect_equal(unlist(unname(ph1$values)), c("alice", "bob", "celine", "david", "eric", "filip"))
  expect_equal(ph1$len, 2)
})


