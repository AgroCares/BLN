# tables with supportive information

#' Table with BLN parameters being used in the package
#'
#' This table contains all parameters being used in the BLN package to calculate the soil quality index.
#'
#' @format A data.table with x rows and x columns:
#' \describe{
#'   \item{bln_parm_id}{the parameter id}
#'   \item{bln_parm_name}{the name of the parameter}
#'   \item{bln_parm_type}{the type of the parameter. Options: measurement, field property}
#'   \item{bln_parm_description}{a short description of the parameters}
#'   \item{bln_parm_unit}{the unit of the parameter}
#'   \item{bln_parm_min}{the maximum allowed value for the parameter}
#'   \item{bln_parm_max}{the minimum allowed value for the parameter}
#'   \item{bln_parm_data_type}{the data type of the parameter: numeric, character or boolean}
#'   \item{bln_parm_enum}{does the parameter have predefined options}
#'   \item{bln_parm_options}{allowed options for the parameteer}
#'}
"bln_parms"

#' Table with country specific soil types being used to evaluate soil quality
#'
#' This table contains categories of soil types being used to evaluate soil quality. Categories vary per country.
#'
#' @format A data.table with 9 rows and 4 columns:
#' \describe{
#'   \item{bln_soil_id}{the soil category id}
#'   \item{bln_country}{the name of the country where the soil category is used}
#'   \item{bln_soil_cat1}{soil category 1, might be in the language of the country}
#'   \item{bln_soil_cat2}{soil category 2, in english}
#'}
"bln_soiltype"

#' Table with country specific threshold values to evaluate soilquality'
#'
#' This table contains the evaluation coefficients to convert a soil function value into a soil quality score ranging from 0 to 1.
#' The scoring function migh vary per country, indicator and subcatogries defined by variable categories of soil and crop types.
#'
#' @format A data.table with x rows and 12 columns:
#' \describe{
#'   \item{bln_trh_id}{the threshold category id}
#'   \item{bln_country}{the country name where the threshold coefficients apply}
#'   \item{bln_esd}{the soil ecosystem service for which the threshold applies}
#'   \item{bln_category}{the soil quality category: chemical, biological, physical, other}
#'   \item{bln_indicator}{the bln indicator for which the threshold applies}
#'   \item{bln_indicator_name}{the bln indicator name}
#'   \item{bln_threshold_cropcat}{the bln crop category for which specific thresholds apply. if empy, then the threshold is generally applicable}
#'   \item{bln_threshold_soilcat}{the bln soil category for which specific thresholds apply. if empy, then the threshold is generally applicable}
#'   \item{bln_scoringtype}{the soil evaluation function to score the soil function. Options: parabolic, logistic, linear}
#'   \item{bln_st_c1}{the first coefficient of the scoring function selected}
#'   \item{bln_st_c2}{the second coefficient of the scoring function selected}
#'   \item{bln_st_c3}{the third coefficient of the scoring function selected}
#'}
"bln_thresholds"

#' Linking table between crops and different functions in OBIC
#'
#' This table helps to link the different crops in the OBIC functions with the crops selected by the user
#'
#' @format A data.frame with 465 rows and 3 columns:
#' \describe{
#'   \item{crop_code}{The BRP gewascode of the crop}
#'   \item{crop_name}{The name of the crop, in lower case}
#'   \item{crop_cat1}{Classification of crop per land use type (arable, maize, grass, nature)}
#'   \item{bln_country}{the country name where the crop codes are applicable}
#' }
"bln_crops"

#'Local Surface Water properties needed for BBWP functions
#'
#'This table gives the mean soil properties per LSW needed for the BBWP risk indicators in view of the soils contribution to water quality
#'
#'
#' @format A data.frame with x rows and x columns:
#' \describe{
#'   \item{B_LSW_ID}{The LSW ID}
#'   \item{B_SOM_LOI}{}
#'   \item{B_CLAY_MI}{}
#'   \item{B_SAND_MI}{}
#'   \item{B_SILT_MI}{}
#'   \item{B_N_RT}{}
#'   \item{B_P_AL}{}
#'   \item{B_P_CC}{}
#'   \item{B_P_WA}{}
#'   \item{B_P_SG}{}
#'   \item{B_FE_OX}{}
#'   \item{B_AL_OX}{}
#'   \item{B_RO_R}{}
#'   \item{B_SA_W}{}
#'   \item{B_SOM_LOI_SD}{}
#'   \item{B_CLAY_MI_SD}{}
#'   \item{B_SAND_MI_SD}{}
#'   \item{B_SILT_MI_SD}{}
#'   \item{B_N_RT_SD}{}
#'   \item{B_P_AL_SD}{}
#'   \item{B_P_CC_SD}{}
#'   \item{B_P_WA_SD}{}
#'   \item{B_P_SG_SD}{}
#'   \item{B_FE_OX_SD}{}
#'   \item{B_AL_OX_SD}{}
#'   \item{B_RO_R_SD}{}
#'   \item{B_SA_W_SD}{}
#' }
"bln_lsw"
