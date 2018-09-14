context("select_regression_response_columns")

library(magrittr)
library(mnmacros)
#library(testthat)

rm(list = ls())
set.seed(0)

# single column char
y1 <- rep(c("a", "b"), c(5, 5))
expect_error(
  data.frame(y1, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% ncol() >= 2",
  fixed = T)

# single column factor
y2 <- rep(c("a", "b"), c(5, 5)) %>% factor()
expect_error(
  data.frame(y2, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% ncol() >= 2",
  fixed = T)

# single column numeric
y3 <- rnorm(10)
expect_error(
  data.frame(y3, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% ncol() >= 2",
  fixed = T)

# single column integer
y4 <- rnorm(10) %>% as.integer()
expect_error(
  data.frame(y4, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% ncol() >= 2",
  fixed = T)

# single column char low varance
y5 <- rep("a", 10)
expect_error(
  data.frame(y5, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% ncol() >= 2",
  fixed = T)

# single column factor low varance
y6 <- rep("a", 10) %>% factor()
expect_error(
  data.frame(y6, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% ncol() >= 2",
  fixed = T)

# single column numeric low varance
y7 <- rep(1, 10)
expect_error(
  data.frame(y7, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% ncol() >= 2",
  fixed = T)

# single column integer low varance
y8 <- rep(1, 10) %>% as.integer()
expect_error(
  data.frame(y8, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% ncol() >= 2",
  fixed = T)

# double column char
actual <-
  data.frame(y1, y1, stringsAsFactors = F) %>%
  select_regression_response_columns()
expected <-
  c()
expect_equal(actual, expected)

# double column factor
actual <-
  data.frame(y2, y2, stringsAsFactors = F) %>%
  select_regression_response_columns()
expected <-
  c()
expect_equal(actual, expected)

# double column numeric
actual <-
  data.frame(y3, y3, stringsAsFactors = F) %>%
  select_regression_response_columns()
expected <-
  c("y3", "y3.1")
expect_equal(actual, expected)

# double column integer
actual <-
  data.frame(y4, y4, stringsAsFactors = F) %>%
  select_regression_response_columns()
expected <-
  c("y4", "y4.1")
expect_equal(actual, expected)

# double column char low varance
actual <-
  data.frame(y5, y5, stringsAsFactors = F) %>%
  select_regression_response_columns()
expected <-
  c()
expect_equal(actual, expected)

# double column factor low varance
actual <-
  data.frame(y6, y6, stringsAsFactors = F) %>%
  select_regression_response_columns()
expected <-
  c()
expect_equal(actual, expected)

# double column numeric low varance
actual <-
  data.frame(y7, y7, stringsAsFactors = F) %>%
  select_regression_response_columns()
expected <-
  c()
expect_equal(actual, expected)

# double column integer low varance
actual <-
  data.frame(y8, y8, stringsAsFactors = F) %>%
  select_regression_response_columns()
expected <-
  c()
expect_equal(actual, expected)

# single column too small
y9 = rnorm(2)
expect_error(
  data.frame(y9, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% nrow() >= 3",
  fixed = T)

# double column too small
expect_error(
  data.frame(y9, y9, stringsAsFactors = F) %>%
    select_regression_response_columns(),
  "data %>% nrow() >= 3",
  fixed = T)

# mixed column
actual <-
  data.frame(y1, y2, y3, y4, y5, y6, y7, y8, stringsAsFactors = F) %>%
  select_regression_response_columns()
expected <-
  c("y3", "y4")
expect_equal(actual, expected)
