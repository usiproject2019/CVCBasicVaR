#' Compute Daily Returns of Selected Stocks/Indices from Yahoo Finance
#'
#' The function computes daily returns from Yahoo Finance for a single company or for a vector that contains
#' the tickers of several companies. Note that this function is based on the functions \code{\link[quantmod:getSymbols]{getSymbols}} and \code{\link[quantmod:dailyReturn]{dailyReturn}}  of the \code{\link[quantmod]{quantmod}}
#' package
#'
#' @param ticker a character vector of the Yahoo Finance ticker of the companies/indices for which the daily return will be computed.
#' @param price_type a number that specifies what is the base of the data on which the returns will be computed (default = 4).
#'  1 = Daily price,
#'  2 = Highest price,
#'  3 = Lowest price,
#'  4 = Closing price,
#'  6 = Adjusted price
#' @param from a class "date" element. It specifies the starting date on which the returns will be computed (date of the first observation). Note that, as the returns are calculated using the arithmetic approach, the first observation will be lost.
#' @param to a class "date" element. It specifies until when the returns will be computed (date of the last observation)
#'
#' @return a class "xts" element that contains the daily returns of each specified company in the specified time-interval
#'
#' @author Massimo Caprari, Anastasiya Varvus, Michele Cotugno (CVC)
#'
#' @references \href{https://finance.yahoo.com/}{Yahoo Finance}
#'
#' @seealso \code{\link[quantmod:getSymbols.yahoo]{getSymbols.yahoo}}, \code{\link[TEST:Price_Download]{Price_Download}}, \code{\link[quantmod:dailyReturn]{dailyReturn}}
#'
#'
#' @examples
#' Return_Download("^DJI", price_type = 4, from="2019-05-01", to="2019-05-10")
#'
#'
#' Stocks <- c("GE", "FORD", "BA", "^DJI", "DB")
#' Returns <- Return_Download(Stocks, 4, from = "2019-05-01", to = "2019-05-10")
#'
#' @export
Return_Download <- function (ticker, price_type=4, from, to) {
  if (price_type ==5) {print("ERROR! Returns must be computed on prices, not on volume! Please select a different value for the variable: price_type")}
  else {
    Return <- lapply(ticker, function(i) {(getSymbols(i, from = from, to = to, auto.assign=FALSE)[, price_type])})
    Return <- do.call(merge, Return)
    Return <- na.omit(Return)
    Return <- lapply(Return, function(i) {dailyReturn(i)})
    Return <- do.call(merge, Return)
    Return <- Return[-1]
    colnames(Return)<- paste(ticker, "Daily Return")
    return(Return)
  }
}

