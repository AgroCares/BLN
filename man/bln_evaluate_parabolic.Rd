% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bln_evaluate.R
\name{bln_evaluate_parabolic}
\alias{bln_evaluate_parabolic}
\title{Evaluate a soil quality function using a parabolic function}
\usage{
bln_evaluate_parabolic(x, x.top, must.plateau = TRUE)
}
\arguments{
\item{x}{(numeric) The values of a calc function to be converted to an evaluation}

\item{x.top}{(numeric) The value at which x reaches the plateau}

\item{must.plateau}{(boolean) is there a plateau present in the parabolic function}
}
\value{
A transformed variable after applying a parabolic evaluation function. A numeric value.
}
\description{
This function evaluates the calculated values from an indicator using a parabolic function. After the optimum is reached the it stays at its plateau.
}
\examples{
bln_evaluate_parabolic(x = 5, x.top = 8)
bln_evaluate_parabolic(x = c(0.1,0.5,1.5,3.5), x.top = 6.5)

}
