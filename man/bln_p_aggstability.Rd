% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bln_prod_aggregatestability.R
\name{bln_p_aggstability}
\alias{bln_p_aggstability}
\title{Calculate and evaluates the aggregate stability index based on occupation CEC}
\usage{
bln_p_aggstability(
  B_SOILTYPE_AGR,
  A_SOM_LOI,
  A_K_CO_PO,
  A_CA_CO_PO,
  A_MG_CO_PO
)
}
\arguments{
\item{B_SOILTYPE_AGR}{(character) The type of soil}

\item{A_SOM_LOI}{(numeric) The organic matter content of soil in percentage}

\item{A_K_CO_PO}{(numeric) The occupation of the CEC with K (\%)}

\item{A_CA_CO_PO}{(numeric) The occupation of the CEC with Ca (\%)}

\item{A_MG_CO_PO}{(numeric) The occupation of the CEC with Mg (\%)}
}
\value{
The aggregate stability index of a soil given the Cation Exchange Capacity and its composition with major cations. A numeric value.
}
\description{
This function calculates and evaluates an aggregate stability index given the CEC and its occupation with major cations.
}
