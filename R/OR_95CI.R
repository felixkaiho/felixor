#' Calculate odds ratios and confidence intervals
#'
#' @param coef A vector of coefficient estimates
#' @param se A vector of standard errors
#' @param siglevel The significance level
#' @param roundto The number of decimal places
#'
#' @return Odds ratios and confidence intervals
#' @export
#' @import stats
#' @examples
#' OR_95CI(1.00551, 0.25341, 0.05, 3)
OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
