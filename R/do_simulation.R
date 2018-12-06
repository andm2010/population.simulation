#' a function that simulates a population over a specified range of ages and times and returns a matrix - or matrices - of
#' the total number of individual members of the simulated population who are infected and susceptible at each age and time specified by the user.
#' The function also returns a matrix - or matrices -  of the prevalence of infection at each age and time.
#'
#' @param total_births
#' @param birth_dates
#' @param delta
#' @param age_steps
#' @param generate_birth_counts_fun The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#' @param generate_incidence_fun The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#' @param generate_base_mortality_fun The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#' @param generate_base_mortality_tau_fun The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#' @return a list, including a matrix of counts of the infected population, a matrix of counts of the susceptible population, and a matrix of the prevalence of infection among the total population at each age and time indicated by the simulation
#' @examples do_simiulation(total_births = 1000,
#'                         birth_dates = 1945:1950,
#'                         delta = 1,
#'                         age_steps = 3,
#'                         generate_birth_counts_fun = generate_birth_counts,
#'                         generate_incidence_fun = generate_incidence,
#'                         generate_base_mortality_fun = generate_base_mortality,
#'                         generate_base_mortality_tau_fun = generate_excess_mortality_tau)


do_simiulation <- function (total_births,
                            birth_dates,
                            delta,
                            age_steps,
                            generate_birth_counts_fun,
                            generate_incidence_fun,
                            generate_base_mortality_fun,
                            generate_base_mortality_tau_fun)
{

  birth_counts <- generate_birth_counts_fun (total_births, birth_dates, delta)

  incidence_matrix <- generate_incidence_matrix (age_steps, birth_dates, generate_incidence_fun)

  base_mortality_matrix <- generate_base_mortality_matrix (age_steps,birth_dates, generate_base_mortality_fun)

  susceptible_survival_rate_matrix <- generate_susceptible_surv_rate(incidence_matrix, base_mortality_matrix)

  susceptible_cumulative_survival_matrix <- generate_susceptible_cumulative_survival_matrix(susceptible_survival_rate_matrix)

  susceptible_pop_counts <- generate_susceptibles(susceptible_cumulative_survival_matrix, birth_counts)

  # infected_mortality_matrix <- generate_infected_mortality_matrix (generate_base_mortality_fun,
  #                                                                  generate_excess_mortality_fun,
  #                                                                  birth_dates, age_steps) # excess + baseline mortality

  infected_tau_0_counts <- generate_infected_tau_0(incidence_matrix, susceptible_pop_counts)

  # infected_mortality_array <- generate_infected_mortality_array (generate_base_mortality_fun,
  #                                                                generate_excess_mortality_tau_funy,
  #                                                                birth_dates,
  #                                                                age_steps) # excess + baseline mortality
  #
  # infected_pop_counts <- generate_infected_population (infected_tau_0_counts, infected_mortality_array) # do we need a matrix version of this

  prevalence <- infected_tau_0_counts/ (susceptible_pop_counts + infected_tau_0_counts)
  output <- list (infected_tau_0_counts, susceptible_pop_counts, prevalence)

  return(output)

  }


# Example
do_simiulation(total_births = 1000,
               birth_dates = 1945:1950,
               delta = 1,
               age_steps = 3,
               generate_birth_counts_fun = generate_birth_counts,
               generate_incidence_fun = generate_incidence,
               generate_base_mortality_fun = generate_base_mortality,
               generate_base_mortality_tau_fun = generate_excess_mortality_tau)


