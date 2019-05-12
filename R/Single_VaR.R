#' Compute the Value at Risk for Single Positions
#'
#' Using the Asset Normal Approach, the function computes the Value at Risk for a given time series of return and vector of positions.
#'
#' @param return a class "xts" element: time serie of returns. In order to get it, it is recommended to use \code{\link[TEST:Return_Download]{Return_Download}} function
#' @param confidence_level a number between 0 and 1 that specifies what is the confidence level to use in the computation
#' @param wealth a numeric vector that specifies the monetary exposure assumed in every stock/index (note that the order of elements of the vector must reflect the order of the elements of the returns)
#' @param T a number that specifies the time interval (in days) based on which the Value at Risk is computed
#'
#' @author Massimo Caprari, Anastasiya Varvus, Michele Cotugno (CVC)
#'
#' @references \href{https://finance.yahoo.com/}{Yahoo Finance}
#'
#' @seealso \code{\link[quantmod:quantmod]{quantmod}}, \code{\link[quantmod:getSymbols.yahoo]{getSymbols.yahoo}}, \code{\link[TEST:Return_Download]{Return_Download}},
#'  \code{\link[quantmod:dailyReturn]{dailyReturn}},
#'
#' @examples
#' Stocks <- c("GE", "AAPL", "BA", "DB", "FORD", "^DJI")
#' Returns <- Return_Download(Stocks, 4, from = Sys.Date()-365.25*3, to = Sys.Date())
#' Wealth <- c(100000, 50000, 130000, 75000, 500000, 930000)
#' IndividualVaR <- SingleVaR(Returns, 0.99, Wealth, 10)
#'
#' @export
Single_VaR <- function (return, confidence_level, wealth, T) {

  Standard_Deviation <- lapply(return, function(i){sd(i)})
  Standard_Deviation <- as.data.frame(Standard_Deviation)
  alpha <- qnorm(confidence_level)
  SigmaW <- wealth * Standard_Deviation
  SingleVaR <- alpha * SigmaW*sqrt(T)
  return_column_name <- data.frame(vector_column_name = c(colnames(return)))
  new_column_name <- sub(" Daily Return", "", return_column_name$vector_column_name)
  colnames(SingleVaR) <- paste0(new_column_name)
  rownames(SingleVaR) <- paste0("VaR (", confidence_level, ", ", T, ")")
  return(SingleVaR)
  assign("output", SingleVaR, envir=globalenv())
}
