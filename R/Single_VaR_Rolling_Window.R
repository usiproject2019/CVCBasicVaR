#' Compute the Value at Risk for Single Positions using a Rolling Window Approach
#'
#' Using the Asset Normal Approach, the function computes the Value at Risk each day for different assets
#' using a pre-specified time window for the volatility computation and a certain vector of monetary exposure in each stock/index
#' in a given day.
#'
#' @param price a class "xts" element: time serie of prices In order to get it, it is recommended to use \code{\link[CVCBasicVaR:Price_Download]{Price_Download}} function
#' @param wealth a numeric vector that specifies the monetary exposure assumed in every stock/index (note that the order of elements of the vector must reflect the order of the elements of the prices)
#' @param reference_date  a class "date" element, it is the date on which the wealth is referred. It is used to compute the number of stocks for each assets.
#' @param window a class "numeric" element that specifies what is the time interval in days that will be used for the volatility computation
#' @param confidence_level a number between 0 and 1 that specifies what is the confidence level to use in the computation
#' @param T a number that specifies the time interval (in days) based on which the Value at Risk is computed
#'
#' @return The function returns a class "xts" object that contains the daily VaRs of every assets for the its relative monetary exposure in a certain date
#' and for a chosen window of observation.
#' It also returns some inputs of the function itself (Confidence level, time interval).
#'
#' @author Massimo Caprari, Anastasiya Varvus, Michele Cotugno (CVC)
#'
#' @references \href{https://finance.yahoo.com/}{Yahoo Finance}, \href{ https://www.investopedia.com/terms/r/rollingreturns.asp}{Investopedia}
#'
#' @seealso \code{\link[quantmod:quantmod]{quantmod}}, \code{\link[quantmod:getSymbols.yahoo]{getSymbols.yahoo}}, \code{\link[CVCBasicVaR:Price_Download]{Price_Download}},
#'  \code{\link[CVCBasicVaR:Rolling_Window_Volatility]{Rolling_Window_Volatility}}
#'
#' @examples
#' ## Not Run:
#' Stocks <- c("GE", "AAPL")
#' Price <- Price_Download(Stocks, 6, from = "2010-01-04", to = "2018-01-01")
#' Wealth <- c(100000, 50000)
#' RollingVaR <- Single_VaR_Rolling_Window(Price, Wealth, "2010-01-02", 250, 0.99, 1)
#'## End (Not run)
#' @export
Single_VaR_Rolling_Window <- function (price, wealth, reference_date, window, confidence_level, T) {
  Stock_Number <- wealth/price[reference_date]
  Wealth <- as.data.frame(price)
  Wealth <- mapply('*', Wealth, Stock_Number)

  Daily_Return <- lapply(price, function(i) {dailyReturn(i)})
  Daily_Return <- do.call(merge, Daily_Return)
  Daily_Return <- Daily_Return[-1]

  Single_Volatility <- rollapply(Daily_Return, window, sd)
  Single_Volatility <- na.omit(Single_Volatility)

  Wealth <- Wealth [-(1:window), ]

  alpha <- qnorm(confidence_level)
  SigmaW <- Wealth * Single_Volatility
  Single_VaR_Rolling <- alpha * SigmaW*sqrt(T)

  return_column_name <- data.frame(vector_column_name = c(colnames(price)))
  new_column_name <- gsub("[:.:].*\\w", "", return_column_name$vector_column_name)
  colnames(Single_VaR_Rolling) <- paste0(new_column_name, " VaR (",  confidence_level, ", ", T, ")")

  return(Single_VaR_Rolling)
  assign("output", Single_VaR_Rolling, envir=globalenv())
}


