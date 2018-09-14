context("prepare_for_regression")

library(magrittr)
library(mnmacros)
#library(testthat)

rm(list = ls())
set.seed(0)

y1 <- 50 %>% rnorm(mean = 1, sd = 1)
y2 <- y1 %>% length() %>% rep('a', .)
y3 <- y1 %>% as.character()
y4 <- y1 %>% as.integer()
y5 <- y1 %>% length() %>% rep(1, .)

# single column factoring
actual <-
  data.frame(y2, stringsAsFactors = F) %>%
  prepare_for_regression()
expected <- data.frame(y2)
expect_equal(actual, expected)

# multi column factoring
actual <-
  data.frame(y2, y3, stringsAsFactors = F) %>%
  prepare_for_regression()
expected <- data.frame(y2, y3)
expect_equal(actual, expected)

# single column normalization
actual <-
  data.frame(y1) %>%
  prepare_for_regression()
expected <- data.frame(y1)
expected$y1 <- expected$y1 %>% apply_bcskew0() %>% scale() %>% .[,1]
expect_equal(actual, expected, tolerance = 0.001)

# multi column normalization
actual <- data.frame(y1, y4) %>% prepare_for_regression()
expected <- data.frame(y1, y4)
expected$y1 <- expected$y1 %>% apply_bcskew0() %>% scale() %>% .[,1]
expected$y4 <- expected$y4 %>% apply_bcskew0() %>% scale() %>% .[,1]
expect_equal(actual, expected, tolerance = 0.001)

# mixed column preparation
actual <-
  data.frame(y1, y2, y3, y4, stringsAsFactors = F) %>%
  prepare_for_regression()
expected <- data.frame(y1, y2, y3, y4)
expected$y1 <- expected$y1 %>% apply_bcskew0() %>% scale() %>% .[,1]
expected$y4 <- expected$y4 %>% apply_bcskew0() %>% scale() %>% .[,1]
expect_equal(actual, expected, tolerance = 0.001)

# non varying columns get skiped
actual <-
  data.frame(y5) %>%
  prepare_for_regression()
expected <- data.frame(y5)
expect_equal(actual, expected, tolerance = 0.001)

# short frames dont get bcskew0
actual <-
  data.frame(y1, y2) %>%
  .[1:7,]  %>%
  prepare_for_regression()
expected <-
  data.frame(y1, y2) %>%
  .[1:7,]
expected$y1 <- expected$y1 %>% scale() %>% .[,1]
expect_equal(actual, expected, tolerance = 0.001)
