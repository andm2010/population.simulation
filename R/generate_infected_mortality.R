#generate_infected_mortality

#'calculates the probability of survival in the infected populations
#' @param  infected_pop_counts the
#' matrix of the total  number of people infected at a given age and time
#' @param  infected_mortality_matrix a survival probability matrix for the infected a 3 dimensional structure of age, time and tau(time since infection)
#' @return returns an array  \code{susceptible_survival_rate_matri}
#' @examples
#'  ...


generate_infected_mortality <- function(Ages,Time, generate_excess_mortality, generate_mortality)
  {

  #NB not running

  #function calculates the dicretised survival probabilities i.e. prob of not getting
  #infected and the probability of not dying for a specific age and time.
  infected_mortality_array = array(NA, dim = c(length(Time), length(Ages), length(Ages)))

  for (aa in Ages){
    for(tt in Time){
      for (ta in Ages){

        infected_mortality_array[times + aa, aa, ta] = generate_mortality(tt, aa) + generate_excess_mortality(tt, aa, ta)

        infection_matrix[times + aa, aa] =  generate_incidence(times + aa, aa) # should this also be "infected_mortality_array", i.e. not matrix?

       }
    }
  }

  return(infected_mortality_array)

}
