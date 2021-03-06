#' generate_infected_mortality_array
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param age_steps a number. Indicates the number of steps forward each age group will be aged in the simulation by the do_sim function
#' @param birth_dates a numeric vector of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
#' @param generate_base_mortality_fun a function which takes as arguments age and time and returns a numberic rate of mortality for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function.
#' @param generate_excess_mortality_tau_fun a function which takes as arguments age, time and tau - i.e. the time since infection among the infected population - and returns a numberic rate of mortality for each age and time included in the simulation
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function.
#' @return returns an array of dimensions time, age and time since infection
#' @examples
#' x <- generate_infected_mortality_array (age_steps = 2, birth_dates = 1992:1995,
#' generate_excess_mortality_tau_fun = generate_excess_mortality_tau,
#' generate_base_mortality_fun = generate_base_mortality)

generate_infected_mortality_array <- function(age_steps, birth_dates,
                                              generate_excess_mortality_tau_fun,
                                              generate_base_mortality_fun)
  {

  times  <-  0 : length(birth_dates)
  ages <- 0:age_steps
  infected_mortality_array <-  array(NA, dim = c(length(times)+ age_steps, length(ages), length(ages)))

  for (aa in ages){
    for (ta in ages){

      infected_mortality_array[times + aa +1, aa + 1, ta + 1] = 1 - generate_base_mortality_fun(times + aa, aa) * generate_excess_mortality_tau_fun(times + aa, aa, ta)

    }
  }

  return(infected_mortality_array)

}






