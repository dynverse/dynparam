# dynparam 1.0.2 (04-01-2021)

* MINOR CHANGES: Set `check.environment` to `FALSE` in unit tests to fix breaking changes in R devel.

# dynparam 1.0.1 (24-06-2020)
* MINOR CHANGES: Move testthat to Suggests.
* MINOR CHANGES: Use `expect_equal(., ., tolerance = .)` instead of `expect_lte(abs(. - .), .)` in tests.

# dynparam 1.0.0 (02-04-2019)

* INITIAL RELEASE: dynparam helps describe method parameters.
