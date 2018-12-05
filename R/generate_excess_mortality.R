#' a function that takes as arguments age and time and returns a numeric rate of mortality for specified age and time
#' The \code{generate_excess_mortality} function is required as an argument for the packages main \code{do_sim} function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param t numeric, indicates time at which mortality rate is desired
#' @param constant ?
#' @param age_min numeric, indicates desired minimum age
#' @param age_max numeric, indicates desired maximum age
#' @param exmin numeric, indicates minimum rate of mortality, which is reached at \code{age_min}
#' @param exfin numeric, indicates peak mortality, which is reached at \code{age_max}
#' @return a numeric vector that represents the mortalit rate at \code{t}
#' @examples x <- generate_excess_mortality(t=7, constant = 0.05, age_min = 0, age_max = 50, exmin =0.01, exfin =0.05)
#'

generate_excess_mortality <- function(t, constant = 0.05, age_min = 0, age_max = 50, exmin =0.01, exfin =0.05)
  {

  excess_mortality = ifelse(t <= age_min, 0,
                   ifelse(t <= age_max, exmin + ((exfin - exmin)/(age_max - age_min)) * (t - age_min),
                          0))

  return(excess_mortality)
}

