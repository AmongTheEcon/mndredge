select_regression_prediction_columns <- function(data) {
  
  stopifnot(data %>% is.data.frame())
  stopifnot(data %>% nrow() >= 3)
  stopifnot(data %>% ncol() >= 2)
  
  column_names <- data %>% colnames()
  
  class_filter <- function(x) { x == "numeric" || x == "integer" || x == "factor" }
  class_filter_result <- 
    data %>%
    sapply(class) %>%
    sapply(class_filter) %>%
    column_names[.]
  
  if((class_filter_result %>% length()) == 0) return(c())
  
  variability_filter <- function(x) { x > 1 }
  variability_filter_result <-
    data[class_filter_result] %>%
    lapply(unique) %>%
    lapply(length) %>%
    sapply(variability_filter) %>%
    class_filter_result[.]
  
  if((variability_filter_result %>% length()) == 0) return(c())
  
  variability_filter_result
}