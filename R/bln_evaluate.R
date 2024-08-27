#' Evaluate a soil quality function using the general logistic function
#'
#' This function evaluates the calculated values from an indicator using a general logistic function
#'
#' @param x (numeric) The values of a calc function to be converted to an evaluation
#' @param b (numeric) The growth rate
#' @param x0 (numeric) The offset of the x-axis
#' @param v (numeric) Affects the growth rate near the maximum
#' @param increasing (boolean) Should the evaluation increase (\code{TRUE}) with x or decrease (\code{FALSE})?
#'
#' @references \url{https://en.wikipedia.org/wiki/Generalised_logistic_function}
#'
#' @examples
#' bln_evaluate_logistic(x = 5, b = 2, x0 = 3, v = 2.6)
#' bln_evaluate_logistic(x = c(0.1,0.5,1.5,3.5), b = 2, x0 = 3, v = 2.6)
#'
#' @return
#' A transformed variable after applying a logistic evaluation function. A numeric value.
#'
#' @export
bln_evaluate_logistic <- function(x, b, x0, v, increasing = TRUE) {

  # Settings
  if (increasing) {
    A <- 0 # Lower asympote
    K <- 1 # Upper asympote
  } else {
    A <- 1 # Lower asympote
    K <- 0 # Upper asympote
  }
  C <- 1

  # General logistic function
  y <- A + ((K - A) / (C + exp(-b * (x - x0)))^(1 / v))

  return(y)

}

#' Evaluate a soil quality function using a parabolic function
#'
#' This function evaluates the calculated values from an indicator using a parabolic function. After the optimum is reached the it stays at its plateau.
#'
#' @param x (numeric) The values of a calc function to be converted to an evaluation
#' @param x.top (numeric) The value at which x reaches the plateau
#' @param must.plateau (boolean) is there a plateau present in the parabolic function
#'
#' @examples
#' bln_evaluate_parabolic(x = 5, x.top = 8)
#' bln_evaluate_parabolic(x = c(0.1,0.5,1.5,3.5), x.top = 6.5)
#'
#' @return
#' A transformed variable after applying a parabolic evaluation function. A numeric value.
#'
#' @export
bln_evaluate_parabolic <- function(x, x.top, must.plateau = TRUE) {

  # Setting
  a <- 1 / x.top^2
  b <- x.top

  # Calcute the values
  y <- 1 - a * (x - b) ^2

  # Set plateaus
  if(must.plateau){
    y <- ifelse(x >= x.top, 1, y)
  }
  y <- ifelse(y < 0, 0, y)

  return(y)

}

#' Evaluate logistically initially and gausian eventually
#'
#' This function evaluates the calculated values from an indicator using a general logistic function
#'
#' @param x (numeric) The values of a calc function to be converted to an evaluation
#' @param b (numeric) The growth rate
#' @param x0 (numeric) The offset of the x-axis
#' @param v (numeric) Affects the growth rate near the maximum
#' @param optimum (numeric) Point where x has approximaltly reached the optimal value
#' @param optimum_ofset (numeric) Multiplication factor to determine at what point gaussian evaluation should be applied
#'
#' @references \url{https://en.wikipedia.org/wiki/Generalised_logistic_function}
#' \url{https://en.wikipedia.org/wiki/Gaussian_function}
#'
#' @examples
#' bln_evaluate_logistic_gaus_down(x = 5, b = 2, x0 = 3, v = 2.6, optimum = 1)
#' bln_evaluate_logistic_gaus_down(x = c(0.1,0.5,1.5,3.5), b = 2, x0 = 3, v = 2.6, optimum = 0.5)
#'
#' @return
#' A transformed variable after applying a logistic evaluation function. A numeric value.
#'
#' @export
bln_evaluate_logistic_gaus_down <- function(x, b, x0, v, optimum, optimum_ofset = 1.5) {

  # add visual binding
  C = y = NULL

  # collect in dt
  fun.dt <- data.table(x = x)
  fun.dt[, b := b]
  fun.dt[, x0 := x0]
  fun.dt[, optimum := optimum]
  fun.dt[, optimum_ofset := optimum_ofset]
  fun.dt[, C := 1]

  # Settings, if FALSE, evaluation starts at 1 and goes to 0, not testsed with gaussian yet
  if (TRUE) {
    A <- 0 # Lower asympote
    K <- 1 # Upper asympote
  } else {
    A <- 1 # Lower asympote
    K <- 0 # Upper asympote
  }

  # General logistic function
  fun.dt[x <= optimum*optimum_ofset,
         y := A + ((K - A) / (C + exp(-b * (x - x0)))^(1 / v))]

  # when x > allowable range, evaluate with the tail end of a gaussian curve
  # https://en.wikipedia.org/wiki/Gaussian_function
  # fun.dt[x > optimum*optimum_ofset, y := 1 * exp(-((x-optimum*optimum_ofset)^2/(2*3^2)))]

  # YF: this gaussian curve is too steep when the optimum value is large (because SD is set to be 3).
  # To scale it better, SD should be variable as a function of the optimum * optimum_ofset value, e.g. 30% (arbitrarily chosen)
  sd <- optimum*optimum_ofset * 0.3
  fun.dt[x > optimum*optimum_ofset, y := 1 * exp(-((x - optimum*optimum_ofset)^2/(2*sd^2)))]

  return(fun.dt$y)

}

#' Helper function to weight and correct the risk and scores
#'
#' @param x The risk or score value to be weighted
#'
#' @examples
#' cf_ind_importance(x = 0.5)
#' cf_ind_importance(x = c(0.1,0.5,1.5))
#'
#' @return
#' A transformed variable after applying a inverse weighing function so that lower values will gain more impact when applied in a weighed.mean function. A numeric value.
#'
#' @export
cf_ind_importance <- function(x) {
    y <- 1 / (x  + 0.2)

  return(y)
}
