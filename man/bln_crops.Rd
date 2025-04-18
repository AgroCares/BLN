% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bln_tables.R
\docType{data}
\name{bln_crops}
\alias{bln_crops}
\title{Linking table between crops and different functions in OBIC}
\format{
A data.frame with 521 rows and 8 columns:
\describe{
  \item{crop_code}{The BRP gewascode of the crop}
  \item{crop_name}{The name of the crop, in lower case}
  \item{crop_cat1}{Classification of crop per land use type (arable, maize, grass, nature)}
  \item{bln_country}{The country name where the crop codes are applicable}
  \item{B_LU}{The international crop code used in pandex}
  \item{B_LU_EOM}{The effective organic matter supply via roots and root exudates (kg EOS/ha)}
  \item{B_LU_EOM_RESIDUE}{The effective organic matter supply via crop residues (kg EOS/ha)}
  \item{B_LU_HC}{The humification coefficient for the crop remainings left in soil after harvest}
  \item{B_LU_WATERSTRESS_OBIC}{A crop category used in OBIC to express sensitivity to water stress}
  \item{B_LU_MAKKINK}{A crop category used to link crop to Makkink correction factor to estimate actual evaporation}
}
}
\usage{
bln_crops
}
\description{
This table helps to link the different crops in the OBIC functions with the crops selected by the user
}
\keyword{datasets}
