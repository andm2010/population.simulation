#' generate_excess_mortality
#'
#' a function that takes as arguments age and time and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of excess mortality -i.e. among infected population relative to non-infected population - at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param t numeric, indicates time or times at which the excess mortality rate is desired
#' @param constant numeric, indicates a constant rate of excess mortality
#' @param age_min numeric, indicates minimum age to be included in the simulation
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param exmin numeric, indicates minimum excess mortality, which is at age_min
#' @param exfin numeric, indicates maximum/final excess mortality at age_max, unless otherwise specified by user defined function
#' @return a numeric vector that represents the excess mortality rate at t.

generate_excess_mortality <- function(t, constant = 0.05, age_min = 0, age_max = 50, exmin =0.01, exfin =0.05)
  {

  excess_mortality = ifelse(t <= age_min, 0,
                   ifelse(t <= age_max, exmin + ((exfin - exmin)/(age_max - age_min)) * (t - age_min),
                          0))

  return(excess_mortality)
}

