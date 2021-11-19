test_that("multiplication works", {
  expect_type(is_integer_like(1L), "logical")

  expect_true(is_integer_like(c(1L, 2L, 3L)))
  expect_true(is_integer_like(c(1, 2, 3)))
  expect_true(is_integer_like(c(1.0, 2.00, 3.000)))
  expect_true(is_integer_like(c(1, 2, 3, NA)))
  expect_true(is_integer_like(c("1", "2", "3")))
  expect_true(is_integer_like(c("1.0", "2.00", "3.000")))
  expect_true(is_integer_like(c("1", "2", "3", NA)))
  expect_true(is_integer_like(c(" 1", "2 ", " 3 ")))

  expect_false(is_integer_like(c("a", "b", "c")))
  expect_false(is_integer_like(c(1.1, 2, 3)))
  expect_false(is_integer_like(c("1.1", "2", "3")))
  expect_false(is_integer_like(c(TRUE, FALSE, NA)))
})
