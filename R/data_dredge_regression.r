#' @title data_dredge_regression
#'
#' @description data dredge a \code{\link[base]{data.frame}} using \code{\link[stats]{lm}}
#'
#' @importFrom magrittr "%>%"
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @importFrom utils combn
#' @export
#'
#' @param data a \code{\link[base]{data.frame}} to operate over
#' @param max_columns the maximum columns that can go into \code{\link[stats]{lm}} formula
#' @param min_pvalue the maximum the pvalue from \code{\link[stats]{lm}} can be before the result is filtered out. this generaly is an issue only in overfitted models
#' @param max_pvalue the maximum the pvalue from \code{\link[stats]{lm}} can be before the result is filtered out
#' @param ... addtional parameters to pass to \code{\link[stats]{lm}}
#' @return 
#'
#' @author Mark Newman, \email{mark@trinetteandmark.com}
#' @keywords macros
#' @family data_dredging
#'
#' @examples
#'   \dontshow{
#'     library(mndredge)}
#'   set.seed(0)
#'
data_dredge_regression <- function(data, max_columns = 4, min_pvalue = 1e-10, max_pvalue = 0.05, ...) {
  
  stopifnot(data %>% is.data.frame())
  stopifnot(max_columns %>% is.numeric())
  stopifnot(max_columns > 0)
  stopifnot(max_pvalue %>% is.numeric())
  stopifnot(max_pvalue < 1)
  stopifnot(max_pvalue > 0)
  stopifnot((data %>% nrow()) >= 3)

  adjusted_data <- data %>% prepare_for_regression()
  
  prediction_columns <- adjusted_data %>% select_regression_prediction_columns()
  prediction_column_count <- prediction_columns %>% length()
  response_columns <- adjusted_data %>% select_regression_response_columns()
  response_column_count <- response_columns %>% length()
  
  stopifnot(prediction_column_count >= 1)
  stopifnot(response_column_count >= 1)
  
  max_columns <- min(max_columns, prediction_column_count)
  # this formula needs tuning. this seems to be the ceiling
  result_count <-
    choose(prediction_column_count, 1:max_columns) %>% sum() *
    response_column_count

  formula <- character(length = result_count)
  best_pvalue <- numeric(length = result_count)
  adjusted_r_squared <- numeric(length = result_count)
  
  safe_lm_summary <- function(data, formula, ...) {
    tryCatch(
      expr = { lm(formula = formula, data = data, ...) %>% summary() },
      warning = function(x) {} ) }
  
  indx = 1
  for(i in 1:max_columns) {

    all_prediction_options <- combn(prediction_columns, i) %>% t()
    all_prediction_options_count <- all_prediction_options %>% nrow()
    
    for(j in 1:all_prediction_options_count) {

      prediction <-
        all_prediction_options[j,] %>%
        paste(collapse = "*")
      
      for(k in 1:response_column_count) {

        response <- response_columns[k]
        
        if(!(response %in% all_prediction_options[j,])) {
          f1 <- sprintf("%s ~ %s", response, prediction)
          model_summary <-
            adjusted_data %>%
            safe_lm_summary(f1 %>% as.formula(), ...)
          
          if(!(model_summary %>% is.null())) {
          
            arv <- model_summary$adj.r.squared
            if(!(arv %>% is.nan())) {
              
              model_coefficients <-
                model_summary$coefficients %>%
                as.data.frame()
              model_coefficients <-
                model_coefficients[2:(model_summary$coefficients %>% nrow()),]
              mp <- min(model_coefficients$`Pr(>|t|)`)
              
              if(min_pvalue < mp & mp < max_pvalue) {
                formula[indx] <- f1
                best_pvalue[indx] <- mp
                adjusted_r_squared[indx] <- model_summary$adj.r.squared
                indx <- indx + 1
              }}}}}}}
  
  result_count_filterout <- formula != ""
  formula <- formula[result_count_filterout]
  best_pvalue <- best_pvalue[result_count_filterout]
  adjusted_r_squared <- adjusted_r_squared[result_count_filterout]
  
  data.frame(
    formula,
    best_pvalue,
    adjusted_r_squared,
    stringsAsFactors = F)
}
