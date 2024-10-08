% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bln_prod_workability.R
\name{ind_workability}
\alias{ind_workability}
\title{Calculate indicator for workability}
\usage{
ind_workability(D_WO, B_LU_BRP)
}
\arguments{
\item{D_WO}{(numeric) The value of the relative (workable) season length calculated by \code{\link{calc_workability}}}

\item{B_LU_BRP}{(numeric) The crop code from the BRP}
}
\value{
The evaluated score for the soil function to allow the soil to be managed by agricultural activities. A numeric value between 0 and 1.
}
\description{
This function calculates the indicator for the workability of the soil expressed as the period in which the soil can be worked without
inflicting structural damage that cannot be restored by the regular management on the farm.
}
\examples{
ind_workability(D_WO = 0.85,B_LU_BRP = 256)
ind_workability(D_WO = c(0.15,0.6,0.9),B_LU_BRP = c(256,1019,1019))

}
