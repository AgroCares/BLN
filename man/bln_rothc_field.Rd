% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bln_clim_rothc.R
\name{bln_rothc_field}
\alias{bln_rothc_field}
\title{Function for simple RothC simulation for grassland and cropland field}
\usage{
bln_rothc_field(
  B_LU_BRP,
  A_SOM_LOI,
  A_CLAY_MI,
  simyears = 50,
  init = FALSE,
  scen = c("BAU", "ALL"),
  spinup = 10
)
}
\arguments{
\item{B_LU_BRP}{(numeric) value of the BRP crop code}

\item{A_SOM_LOI}{(numeric) value for the soil organic matter content of the soil}

\item{A_CLAY_MI}{(numeric) value for the clay content of the soil}

\item{simyears}{(integer) value for the amount of years to simulate, default is 50 years}

\item{init}{(boolean) use internal analytical solution for initialisation RothC}

\item{scen}{(character) scenarios to be simulated. Options include BAU, BAUIMPR,CLT and ALL.}

\item{spinup}{(numeric) the spinup period that is used for initialisation model}
}
\description{
This function calculates the optimal sequestration scenario for cropland and grassland with the RothC model over the course of 50 years
For cropland this function converts the crop to winter wheat where crop residue is left behind, grassland field are converted to permanent grassland
The function assumes that the P-status of the soil is neutral according to the Dutch fertilization regulation and applies the maximal permitted P-dose in the form of cattle slurry
}
