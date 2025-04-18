% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bln_rothc_sim.R
\name{bln_rothc_sim}
\alias{bln_rothc_sim}
\title{Simulate SOC evolution using Roth-C for the Netherlands}
\usage{
bln_rothc_sim(
  A_SOM_LOI,
  A_CLAY_MI,
  A_DEPTH = 0.3,
  B_DEPTH = 0.3,
  cf_yield = 1,
  M_TILLAGE_SYSTEM = "CT",
  rothc_rotation = NULL,
  rothc_amendment = NULL,
  rothc_parms = list(simyears = 50, init = FALSE, spinup = 10, method = "adams")
)
}
\arguments{
\item{A_SOM_LOI}{(numeric) The percentage organic matter in the soil (\%).}

\item{A_CLAY_MI}{(numeric) The clay content of the soil (\%).}

\item{A_DEPTH}{(numeric) Depth for which soil sample is taken (m). Default set to 0.3.}

\item{B_DEPTH}{(numeric) Depth of the cultivated soil layer (m), simulation depth. Default set to 0.3.}

\item{cf_yield}{(numeric) A relative yield correction factor (fraction) if yield is higher than regional average}

\item{M_TILLAGE_SYSTEM}{(character) gives the tillage system applied. Options include NT (no-till), ST (shallow-till), CT (conventional-till) and DT (deep-till).}

\item{rothc_rotation}{(data.table) Table with crop rotation details and crop management actions that have been taken. Includes also crop inputs for carbon. See details for desired format.}

\item{rothc_amendment}{(data.table) A table with the following column names: year, month, P_NAME, P_DOSE, P_HC, P_OM, and p_p2o5, where month is optional.}

\item{rothc_parms}{(list) A list with simulation parameters controlling the dynamics of RothC Model. Default is NULL. For more information, see details.}
}
\description{
This function calculates the change in carbon stock or C pools (in kg C per ha) based on organic matter amendments, crop rotation, and long-term averaged weather conditions.
}
\details{
This function simulates the fate of SOC given the impact of soil properties, weather and management.
The following inputs are mandatory: rothc_rotation, A_SOM_LOI (\%), and A_CLAY_MI (\%). All other data is optional.
When no weather inputs are given, these are estimated from long-term average weather conditions in the Netherlands.

The organic amendment table includes the columns year, month, P_NAME, P_DOSE, P_HC, P_OM, and p_p2o5, where month is optional.
P_NAME is the fertilizer name, P_DOSE has units (kg / ha), P_HC is the humification coefficient (fraction), P_OM is the organic matter content (%) and p_p2o5 is the phosphate content (%)

The crop table includes the columns: year, B_LU (a crop id), B_LU_NAME (a crop name), B_LU_EOM_CROP (the effective organic matter content, kg/ha), B_LU_EOM_CROPRESIDUE (the effective organic matter content for crop residues, kg/ha), and the B_LU_HC (the humification coeffient,-).
The crop table might include the measures M_GREEN, M_GREEN_PROPERLY, M_CROPRESIDUE, M_IRRIGATION and M_RENEWAL, all in upper case.
* M_GREEN_TIMING (character) the month in which the catch crop is sown, options: (august,september,october,november,never)
* M_CROPRESIDUE (boolean) gives whether crop residues are amended to the soil after harvest.
* M_IRRIGATION (boolean) gives whether the crop is irrigated.
* M_RENEWAL (boolean) gives whether the grassland is renewed (only applicable for grassland)

The simulation of C via the RothC model can be adapted by the following parameters: initialize, c_fractions, dec_rates, simyears and unit.
These have to be in a list called 'rothc_parms'.
}
