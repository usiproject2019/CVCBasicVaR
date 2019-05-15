#' Compute the Daily Volatility using a Rolling Window Approach
#'
#' Using a rolling window approach, the function computes the daily volatility of
#'  a pre-specified vector of assets for given time series of returns.
#'
#' @param return a class "xts" element: time series of returns. In order to get it, it is recommended to use \code{\link[CVCBasicVaR:Return_Download]{Return_Download}} function
#' @param window a class "numeric" element that specifies what is the time interval in days that will be used for the volatility computation

#' @return The function returns a class "xts" object containing the volatility of the pre-specified assets for the pre-specified time window
#' It also returns some inputs of the function itself (window).
#' @author Massimo Caprari, Anastasiya Varvus, Michele Cotugno (CVC)
#'
#' @references \href{https://finance.yahoo.com/}{Yahoo Finance}, \href{https://www.investopedia.com/terms/v/volatility.asp}{Investopedia}
#'
#' @seealso \code{\link[quantmod:quantmod]{quantmod}}, \code{\link[quantmod:getSymbols.yahoo]{getSymbols.yahoo}}, \code{\link[CVCBasicVaR:Return_Download]{Return_Download}},
#'  \code{\link[quantmod:dailyReturn]{dailyReturn}}
#'
#' @examples
#' ## Not Run:
#' Ticker <- c("GE","BA")
#' Returns <- Return_Download(myticker, 4, "2008-01-01", "2018-01-01")
#' Window <- c(10000)
#' Window_Volatility <- Rolling_Window_Volatility(myreturns, mywindow)
#'
#' ## End (Not run)
#'
#' @export
Rolling_Window_Volatility <- function (return, window) {
  Single_Volatility <- rollapply(return, window, sd)
  Single_Volatility <- na.omit(Single_Volatility)
  return_column_name <- data.frame(vector_column_name = c(colnames(return)))
  new_column_name <- sub(" Daily Return", "", return_column_name$vector_column_name)
  colnames(Single_Volatility) <- paste0(new_column_name, " Daily Variance over last ", window, " obs")
  return(Single_Volatility)
}
