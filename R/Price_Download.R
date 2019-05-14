#' Download Daily Price or Volume from Yahoo Finance
#'
#' Download daily price or volume from yahoo finance for a single company or for a vector that contains
#' the tickers of several companies. Note that this function is based on the \code{\link[quantmod:getSymbols]{getSymbols}} function of the \code{\link[quantmod]{quantmod}}
#' package. Note that in order to have an omogeneous output all rows containing at least one missing observation will be deleted.
#'
#' @param ticker a character vector of the yahoo finance ticker of the companies/indices for which prices/volume should be downloaded
#' @param price_type a number between 1 to 6 that specifies what is the base of the data that should be downloaded (default = 4).
#'  1 = Daily price,
#'  2 = Highest price,
#'  3 = Lowest price,
#'  4 = Closing price,
#'  5 = Volume
#'  6 = Adjusted price
#' @param from a class "date" element. It specifies the starting date from which the data will be downloaded (date of the first observation)
#' @param to a class "date" element. It specifies until when the data will be downloaded (date of the last observation)
#'
#' @return The function returns a class "xts" object that contains the downloaded prices/volume of each specified company in the specified time-interval
#'
#' @author Massimo Caprari, Anastasiya Varvus, Michele Cotugno (CVC)
#'
#' @references \href{https://finance.yahoo.com/}{Yahoo Finance}
#'
#' @seealso \code{\link[quantmod:getSymbols.yahoo]{getSymbols.yahoo}}
#'
#' @examples
#' ## Not run:
#' Price_Download("^DJI", , "2019-05-01", "2019-05-10")
#'
#'
#' Stocks <- c("GE", "FORD", "BA", "^DJI", "DB")
#' Prices <- Price_Download(Stocks, 4, from = "2019-05-01", to = "2019-05-10")
#'## End (Not run)
#' @export
Price_Download <- function(ticker, price_type = 4, from, to) {
  Price <- lapply(ticker, function(i) {(getSymbols(i, from = from, to = to, auto.assign=FALSE, src=)[, price_type])})
  Price<- do.call(merge, Price)
  Price<-na.omit(Price)
  return(Price)
}
