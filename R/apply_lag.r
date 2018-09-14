#' @title apply_lag
#'
#' @description lag some of the columns in a \code{\link[base]{data.frame}}
#'
#' @importFrom magrittr "%>%"
#' @export
#'
#' @param data a \code{\link[base]{data.frame}} to operate over
#' @param hold a vector of column names to hold steady
#' @param move a vector of column names to lag
#' @param lag the number of rows to lag by
#' @return 
#'
#' @author Mark Newman, \email{mark@trinetteandmark.com}
#' @keywords macros
#' @family data_dredging
#'
#' @examples
#'   \dontshow{
#'     library(mndredge)}
#'   data.frame(
#'     a = c(1,2,3,4),
#'     b = c(2,3,4,5),
#'     c = c(4,5,6,7)) %>%
#'     apply_lag(hold = "a", move = "b", lag = 1)
#'
apply_lag <- function(data, hold, move, lag = 0) {
  
  stopifnot(is.data.frame(data))
  stopifnot(is.character(hold))
  stopifnot(is.character(move))
  
  cn <- colnames(data)
  stopifnot(hold %in% cn %>% all())
  stopifnot(move %in% cn %>% all())

  stopifnot(is.numeric(lag))
  stopifnot(lag >= 0)
  
  n <- nrow(data)
  stopifnot(n > lag)

  df1 <- data[, hold] %>% as.data.frame()
  df2 <- data[, move] %>% as.data.frame()

  res <- cbind(  
    df1[1:(n-lag),],
    df2[(1+lag):n,]) %>%
    as.data.frame()
  colnames(res) <- c(hold, move)
  rownames(res) <- 1:(n-lag)

  res
}
