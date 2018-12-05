#'calculates the probability of survival in the infected populations
#' @param age_steps the aging step of the initial birth counts
#' @param birth_dates calender dates of birth
#' @param generate_excess_mortality_fun function of excess mortality for a given age, time, and time since infection.
#' @param generate_base_mortality_fun baseline mortality as a function of age and time
#' @return returns an array of dimension time, age and time since infection see example
#' @examples
#' generate_infected_mortality_array(age_steps = 2, birth_dates = 1992:1995, generate_excess_mortality_tau_fun = generate_excess_mortality_tau, generate_base_mortality_fun = generate_base_mortality)

generate_infected_mortality_array <- function(age_steps,
                                        birth_dates,
                                        generate_excess_mortality_tau_fun,
                                        generate_base_mortality_fun){



  times  <-  0 : length(birth_dates)
  ages <- 0:age_steps
  infected_mortality_array <-  array(NA, dim = c(length(times)+ age_steps, length(ages), length(ages)))

  for (aa in ages){
    for (ta in ages){

      infected_mortality_array[times + aa +1, aa + 1, ta + 1] = generate_base_mortality_fun(times + aa, aa) * generate_excess_mortality_tau_fun(times + aa, aa, ta)

    }
  }

  return(infected_mortality_array)

}






