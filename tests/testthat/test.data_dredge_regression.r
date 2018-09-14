context("data_dredge_regression")

library(magrittr)
library(mnmacros)
#library(testthat)

rm(list = ls())
set.seed(0)

# not enough columns
y1 <- rep(c("a", "b"), c(10,10))
expect_error(
  data.frame(y1, stringsAsFactors = F) %>% data_dredge_regression(),
  "data %>% ncol() >= 2 is not TRUE",
  fixed = T)

# not enough response columns
expect_error(
  data.frame(y1, y1, stringsAsFactors = F) %>% data_dredge_regression(),
  "response_column_count >= 1 is not TRUE",
  fixed = T)

# categorical predicts continuous 
y2 <- c(rnorm(10), rnorm(10, mean = 2))
actual <- data.frame(y1, y2, stringsAsFactors = F) %>% data_dredge_regression()
expected <- data.frame(
  formula = c("y2 ~ y1"),
  best_pvalue = c(0.01135),
  adjusted_r_squared = c(0.26782),
  stringsAsFactors = F)
expect_equal(actual, expected, tolerance = 0.001)

# continuous predicts continuous 
y3 <- c(rnorm(10, mean = 2), rnorm(10, mean = 4))
actual <- data.frame(y2, y3, stringsAsFactors = F) %>% data_dredge_regression()
expected <- data.frame(
  formula = c("y3 ~ y2", "y2 ~ y3"),
  best_pvalue = c(0.01400, 0.01400),
  adjusted_r_squared = c(0.25210, 0.25210),
  stringsAsFactors = F)
expect_equal(actual, expected, tolerance = 0.001)

# mixed case test
actual <-
  data.frame(y1, y2, y3, stringsAsFactors = F) %>%
  data_dredge_regression()
expected <- data.frame(
  formula = c("y2 ~ y1", "y3 ~ y1", "y3 ~ y2", "y2 ~ y3", "y3 ~ y1*y2"),
  best_pvalue = c(0.01135, 0, 0.01400, 0.01400, 0.00029),
  adjusted_r_squared = c(0.26782, 0.67155, 0.25210, 0.25210, 0.67321),
  stringsAsFactors = F)
expect_equal(actual, expected, tolerance = 0.001)

# perfect numeric continuous regression
y4 <- y3 + 1
actual <-
  data.frame(y3, y4, stringsAsFactors = F) %>%
  data_dredge_regression()
expect_equal(actual %>% nrow(), 0)

# perfect integer continuous regression
y5 <- 1:10
y6 <- 1:10 + 1 %>% as.integer()
actual <-
  data.frame(y5, y6, stringsAsFactors = F) %>%
  data_dredge_regression()
expect_equal(actual %>% nrow(), 0)

# perfect numeric ~ categorical regression
y7 <- c("q", "w", "e", "r", "t", "y", "u", "i", "o", "p")
actual <-
  data.frame(y5, y7, stringsAsFactors = F) %>%
  data_dredge_regression()
expect_equal(actual %>% nrow(), 0)

# perfect integer ~ categorical regression
actual <-
  data.frame(y6, y7, stringsAsFactors = F) %>%
  data_dredge_regression()
expect_equal(actual %>% nrow(), 0)

