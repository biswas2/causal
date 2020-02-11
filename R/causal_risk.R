#' @title Calculate measures of causal effects for conditionally randomized experiments
#'
#' @description This function computes the causal risk measures in a conditionally randomized experiment via standardization.
#'
#' @param L Vector of the values of L, on which the experiment is conditionally randomized.
#' @param A Vector of treatment assignment. A = 1, if the individual received the treatment; and A = 0, if the individual did not.
#' @param Y Vector of outcome values.  Y = 1, if the individual died; and A = 0, otherwise.
#' @param data (Optional) The data frame containing the A, L, and Y variables by their exact names. If this argument is provided, then the A, L, and Y arguments become optional.
#'
#' @return Measures of causal effects (e.g., risk difference, risk ratio, odds ratio).
#'
#' @examples
#' L <- rep(x = c(0,1), times = c(7, 13))
#' A <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#' Y <- c(0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)
#' romans <- data.frame(L = L, A = A, Y = Y)
#' require(dplyr)
#' causal_risk(A = A, L = L, Y = Y)
#' causal_risk(data = romans) # both are correct
#'
#' @import 'dplyr'
#' @export

causal_risk <- function(A, L, Y, data = NULL) {
  ifelse(
    is.null(data),
    dat <- data.frame(A = as.double(A), L = as.double(L), Y = as.double(Y)),
    dat <- data.frame(A = as.double(data$A),
                      L = as.double(data$L),
                      Y = as.double(data$Y))
  )
  prob <- dplyr::summarise(dplyr::group_by(dat, A, L), m = mean(Y))
  risk_in_treated <-
    prop.table(table(dat$L)) %*% as.matrix(prob$m[prob$A == 1])
  risk_in_non_treated <-
    prop.table(table(dat$L)) %*% as.matrix(prob$m[prob$A == 0])

  results <- data.frame(
    row.names = c(
      "Causal risk difference",
      "Causal risk ratio",
      "Causal odds ratio"
    ),
    values = c(
      as.numeric(risk_in_treated - risk_in_non_treated),
      as.numeric(risk_in_treated / risk_in_non_treated),
      as.numeric((risk_in_treated / (1 - risk_in_treated)) /
                   (risk_in_non_treated / (1 - risk_in_non_treated)))
    )
  )
  return(results)
}
