context("apply_lag")

library(mnmacros)
#library(testthat)

data <- data.frame(
  a = c(1,2,3,4),
  b = c(2,3,4,5),
  c = c(4,5,6,7)
)

actual <- data %>% apply_lag(hold = "a", move = "b", lag = 0)
expected <- data.frame(
  a = c(1,2,3,4),
  b = c(2,3,4,5)
)
expect_equal(actual, expected)

actual <- data %>% apply_lag(hold = "a", move = "b", lag = 1)
expected <- data.frame(
  a = c(1,2,3),
  b = c(3,4,5)
)
expect_equal(actual, expected)

actual <- data %>% apply_lag(hold = "a", move = "c", lag = 1)
expected <- data.frame(
  a = c(1,2,3),
  c = c(5,6,7)
)
expect_equal(actual, expected)

actual <- data %>% apply_lag(hold = c("a", "b"), move = "c", lag = 1)
expected <- data.frame(
  a = c(1,2,3),
  b = c(2,3,4),
  c = c(5,6,7)
)
expect_equal(actual, expected)

actual <- data %>% apply_lag(hold = "a", move = c("b", "c"), lag = 1)
expected <- data.frame(
  a = c(1,2,3),
  b = c(3,4,5),
  c = c(5,6,7)
)
expect_equal(actual, expected)
