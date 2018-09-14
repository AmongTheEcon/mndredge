#' @title data_dredge_correlation
#'
#' @description data dredge a \code{\link[base]{data.frame}} using \code{\link[stats]{cor.test}}
#'
#' @importFrom magrittr "%>%"
#' @importFrom stats cor.test
#' @export
#'
#' @param data a \code{\link[base]{data.frame}} to operate over
#' @param max_lag the maximum lag to search
#' @param max_pvalue the maximum the pvalue from \code{\link[stats]{cor.test}} can be before the result is filtered out
#' @param ... addtional parameters to pass to \code{\link[stats]{cor.test}}
#' @return 
#'
#' @author Mark Newman, \email{mark@trinetteandmark.com}
#' @keywords macros
#' @family data_dredging
#'
#' @examples
#'   \dontshow{
#'     library(mnmacros)}
#'   set.seed(0)
#'   y1 <- seq(-10, 10, .5) %>% as.integer()
#'   y2 <- y1 + y1 %>% length() %>% rnorm()
#'   data.frame(y1, y2) %>% data_dredge_correlation()
#'
data_dredge_correlation <- function(data, max_lag = 0, max_pvalue = 0.05, ...) {
  
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(max_lag))
  stopifnot(is.numeric(max_pvalue))
  stopifnot(nrow(data) >= 3)
 
  column_names <- data %>% colnames()
  acceptable_column <- function(x) { x == "numeric" || x == "integer" }
  good_columns <- column_names[data %>% sapply(class) %>% sapply(acceptable_column)]
  good_column_count <- good_columns %>% length()
  
  stopifnot(good_column_count >= 2)
  result_count = choose(good_column_count, 2) * (1 + max_lag)

  column_a <- character(length = result_count)
  column_b <- character(length = result_count)
  estimate <- numeric(length = result_count)
  p.value <- numeric(length = result_count)
  lag <- numeric(length = result_count)
  
  indx = 1
  for(i in 1:(good_column_count - 1)) {
    for(j in (i + 1):good_column_count) {
      
      a <- good_columns[i]
      b <- good_columns[j]
      t1 <- data[, c(a, b)]

      for(k in 0:max_lag) {
        
        t2 <- t1 %>% apply_lag(hold = a, move = b, lag = k)
        ct <- suppressWarnings(cor.test(t2[,a], t2[,b], ...))
        
        column_a[indx] <- a
        column_b[indx] <- b
        lag[indx] <- k
        estimate[indx] <- ct$estimate
        p.value[indx] <- ct$p.value
        
        indx <- indx + 1
      }}}
  
  res <- data.frame(
    column_a,
    column_b,
    estimate,
    p.value,
    lag,
    stringsAsFactors = F)
  
  filterout <- is.na(res$p.value) | res$p.value >= max_pvalue
  res <- res[!filterout,]
  rownames(res) <- NULL
  
  res
}
