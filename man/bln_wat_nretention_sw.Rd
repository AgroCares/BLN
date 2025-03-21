% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bln_sw_nretention.R
\name{bln_wat_nretention_sw}
\alias{bln_wat_nretention_sw}
\title{Function to calculate and evaluate the N retention in soils in view of water purification for surface quality}
\usage{
bln_wat_nretention_sw(
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
