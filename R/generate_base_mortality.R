#' generate_base_mortality
#'
#' a function that takes as arguments age and time and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of mortality at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param t numeric, indicates time or times at which the mortality rate is desired
#' @param constant numeric, indicates a constant rate of mortality
#' @param age_min numeric, indicates minimum age to be included in the simulation
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param exmin numeric, indicates minimum mortality, which is at age_min
#' @param exfin numeric, indicates maximum/final mortality at age_max, unless otherwise specified by user defined function
#' @return a numeric vector that represents the mortality rate at t.

#Option 1

generate_base_mortality <- function(t, constant = 0.01, age_min = 1,
                                   age_max = 50,
                                   exmin =0,
                                   exfin =0.01){

  base_mortality = ifelse(t <= age_min, 0,
                     ifelse(t <= age_max, exmin + ((exfin - exmin)/(age_max - age_min)) * (t - age_min),
                           0))

  return(base_mortality)
}



# Option # 2
# generate_base_mortality <- function(t=seq(50,0.55), age = 0:49) {
#
#   base_mortality = exp(-(1/age)*t)*0.1
#
#   return(base_mortality)
# }
