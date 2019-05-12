#' Compute the Marginal Value at Risk of a Portfolio
#'
#' Using the Asset Normal Approach, the function computes the Marginal Value at Risk of a portfolio for a given time series of returns and a vector of monetary positions.
#'
#' @param return a class "xts" element: time serie of returns. In order to get it, it is recommended to use \code{\link[TEST:Return_Download]{Return_Download}} function
#' @param confidence_level a number between 0 and 1 that specifies what is the confidence level to use in the computation
#' @param wealth a numeric vector that specifies the monetary exposure assumed in every stock/index (note that the order of elements of the vector must reflect the order of the elements of the returns)
#' @param T a number that specifies the time interval (in days) based on which the Value at Risk is computed
#'
#' @author Massimo Caprari, Anastasiya Varvus, Michele Cotugno (CVC)
#'
#' @references \href{https://finance.yahoo.com/}{Yahoo Finance}, \href{https://www.investopedia.com/ask/answers/041715/what-variancecovariance-matrix-or-parametric-method-value-risk-var.asp}{Investopedia}
#'
#' @seealso \code{\link[quantmod:quantmod]{quantmod}}, \code{\link[quantmod:getSymbols.yahoo]{getSymbols.yahoo}}, \code{\link[TEST:Return_Download]{Return_Download}},
#'  \code{\link[quantmod:dailyReturn]{dailyReturn}},
#'
#' @examples
#' Stocks <- c("GE", "AAPL", "BA", "DB", "FORD", "^DJI")
#' Returns <- Return_Download(Stocks, 4, from = Sys.Date()-365.25*3, to = Sys.Date())
#' Wealth <- c(100000, 50000, 130000, 75000, 500000, 930000)
#' MarginalVaR <- Portfolio_Marginal_VaR(Returns, 0.99, Wealth, 10)
#'
#' @export
Portfolio_Marginal_VaR <- function (return, confidence_level, wealth, T) {
  VCV_Matrix <- cov(return)
  Portfolio_Volatility <- sqrt (wealth %*% VCV_Matrix %*% wealth)
  alpha <- qnorm(confidence_level)
  Portfolio_VaR <- alpha * Portfolio_Volatility * (sqrt(T))
  Beta_Numerator <- (VCV_Matrix %*% wealth)
  Beta_Denominator <- as.numeric(wealth %*% VCV_Matrix %*% wealth)
  Beta <- Beta_Numerator / Beta_Denominator
  Marginal_VaR <- Beta %*% Portfolio_VaR
  return_column_name <- data.frame(vector_column_name = c(colnames(return)))
  new_column_name <- sub(" Daily Return", "", return_column_name$vector_column_name)
  rownames(Marginal_VaR) <- paste0(new_column_name)
  colnames(Marginal_VaR) <- paste0("Marginal VaR (", confidence_level, ", ", T, ")")
  return(Marginal_VaR)
  assign("output", Marginal_VaR, envir=globalenv())
}
