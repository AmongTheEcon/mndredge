context("data_dredge_correlation")

library(magrittr)
library(mnmacros)
#library(testthat)

rm(list = ls())
set.seed(0)

y1 <- seq(-10, 10, .5) %>% as.integer()
y2 <- y1 + y1 %>% length() %>% rnorm()
y3 <- y1^3 + y1 %>% length() %>% rnorm()
y4 <- y1 %>% length() %>% rep('a', .)
y5 <- y4 %>% factor()
y6 <- y1 %>% length() %>% rep(T, .)
y7 <- y1 %>% length() %>% rep(5, .) %>% as.integer()

# not enough columns
expect_error(
  data.frame(y1) %>% data_dredge_correlation(),
  "good_column_count >= 2")

# not enough _good_ columns
expect_error(
  data.frame(y1, y4) %>% data_dredge_correlation(),
  "good_column_count >= 2")

# simple 2 column test
actual <- data.frame(y1, y2) %>% data_dredge_correlation()
expected <- data.frame(
  column_a = c("y1"),
  column_b = c("y2"),
  estimate = c(0.9882),
  p.value = c(0),
  lag = c(0),
  stringsAsFactors = F)
expect_equal(actual, expected, tolerance = 0.001)

# simple 3 column test
actual <- data.frame(y1, y2, y3) %>% data_dredge_correlation()
expected <- data.frame(
  column_a = c("y1", "y1", "y2"),
  column_b = c("y2", "y3", "y3"),
  estimate = c(0.9882, 0.9136, 0.9049),
  p.value = c(0, 0, 0),
  lag = c(0, 0, 0),
  stringsAsFactors = F)
expect_equal(actual, expected, tolerance = 0.001)

# mixed case test
actual <-
  data.frame(y1, y2, y4, stringsAsFactors = F) %>%
  data_dredge_correlation()
expected <- data.frame(
  column_a = c("y1"),
  column_b = c("y2"),
  estimate = c(0.9882),
  p.value = c(0),
  lag = c(0),
  stringsAsFactors = F)
expect_equal(actual, expected, tolerance = 0.001)

# no variation test
actual <- data.frame(y1, y7) %>% data_dredge_correlation()
expect_equal(actual %>% nrow(), 0)

# big mixed case test
actual <-
  data.frame(y1, y2, y3, y4, y5, y6, y7, stringsAsFactors = F) %>%
  data_dredge_correlation()
expected <- data.frame(
  column_a = c("y1", "y1", "y2"),
  column_b = c("y2", "y3", "y3"),
  estimate = c(0.9882, 0.9136, 0.9049),
  p.value = c(0, 0, 0),
  lag = c(0, 0, 0),
  stringsAsFactors = F
)
expect_equal(actual, expected, tolerance = 0.001)
