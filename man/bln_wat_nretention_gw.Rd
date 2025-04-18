% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bln_gw_nretention.R
\name{bln_wat_nretention_gw}
\alias{bln_wat_nretention_gw}
\title{Function to calculate and evaluate the N retention in soils in view of water purification for groundwater quality}
\usage{
bln_wat_nretention_gw(
  ID,
  B_LU_BRP,
  B_SOILTYPE_AGR,
  B_AER_CBS,
  B_GWL_CLASS,
  A_SOM_LOI,
  A_N_RT,
  A_CN_FR = NA_real_
)
}
\arguments{
\item{ID}{(character) A field id}

\item{B_LU_BRP}{(numeric) The crop code}

\item{B_SOILTYPE_AGR}{(character) The agricultural type of soil}

\item{B_AER_CBS}{(character) The agricultural economic region in the Netherlands (CBS, 2016)}

\item{B_GWL_CLASS}{(character) The groundwater table class}

\item{A_SOM_LOI}{(numeric) The organic matter content of the soil (\%)}

\item{A_N_RT}{(numeric) The organic nitrogen content of the soil in mg N / kg}

\item{A_CN_FR}{(numeric) The carbon to nitrogen ratio (-)}
}
\description{
This function equals the N retention function of the OSI
}
\examples{
bln_wat_nretention_gw(
ID = 15,
B_LU_BRP = c(233,259,2014,308),
B_SOILTYPE_AGR = rep('rivierklei',4),
B_GWL_CLASS = rep('GtIII',4),
B_AER_CBS = rep('LG06',4),
A_SOM_LOI = rep(3,4),
A_N_RT = c(1000,2500,3500,9800),
A_CN_FR = c(25,15,10,5.5)
)


}
