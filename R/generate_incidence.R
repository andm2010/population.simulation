#' generate_incidence is a function that takes as arguments age and time and returns a numeric vector of length \code{Ipeak}
#' representing a rate of incidence at the indicated age and time
#' The generate_incidence function is required as an argument for the do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param t numeric, indicates time at which the incidence rate is desired
#' @param constant numeric, indicates a constant rate of incidence
#' @param age_min numeric, indicates desired minimum age
#' @param age_max numeric, indicates desired maximum age
#' @param age_peak numberic, indicates age at which indicence reaches maximum, which is at \code{Ipeak}
#' @param Imin numeric, indicates minimum incidence, which is at \code{age_min}
#' @param Ipeak numeric, indicates maximum incidence at \code{Ipeak}
#' @param Ifin numeric, indicates incidence at \code{Ipeak}
#' @return a numeric vector that represents the incidence rate at \code{t}.
#' @examples To be entered
#'
#' generate_incidence (t=7, constant = 0.05, age_min = 0, age_max = 50,  age_peak= 25, Imin =0.01,  Ipeak =0.05,  Ifin =0.02)

generate_incidence1 <- function(t, age ) {

  incidence = exp(-(1/age)*t)*0.1

  return(incidence)
}


generate_incidence2 <- function(t, conc = 0.05, agemin = 0,
                            agemax = 50,  agepeak= 25,
                            Imin =0.01,  Ipeak =0.05,
                            Ifin =0.02){

  # consider providing some guidance about how a user may define incidence function
  #varying Inicdence
  incidence = ifelse(t <= agemin, 0,
                     ifelse(t <= agepeak, Imin + ((Ipeak - Imin)/(agepeak - agemin)) * (t - agemin),
                            ifelse(t <= agemax, Ipeak + ((Ifin - Ipeak )/(agemax - agepeak)) * (t - agepeak), 0)))

  return(incidence)
}




