# generate_mortality ?????????



#'function for base mortality
#' @param t time .
#' @param conc constant mortality
#' @param age_min minimum age
#' @param exmin minimum excess mortality
#' @param exmax maximum excess mortality
#' @return returns a number or or a vector of mortality rates for a a given age and time given  \code{t},\code{conc}, \code{exmax},\code{exmin}, and \code{exfin}
#' @examples
#' generate_birth_counts(1000, 1984 : 1990, 1)
#' generate_birth_counts(10000, 2005 : 2018, 1)


generate_base_mortality <- function(t, conc = 0.01, age_min = 1,
                                   age_max = 50,
                                   exmin =0,
                                   exfin =0.01){
  #varying mortality
  Ex_mort = ifelse(t <= age_min, 0,
                   ifelse(t <= age_max, exmin + ((exfin - exmin)/(age_max - age_min)) * (t - age_min),
                          0))

  return(base_mortality)
}

