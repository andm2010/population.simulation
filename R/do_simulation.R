# Do simulation "core function
# Created 6 Dec 2018

# Step 1; User defines incidence funcitions; skip this step an go to do sim if you want to choose a default from the pacage
generate_incidence <- function(age, time) {
  returns (incidence)
}
generate_base_mortality <- function(age,time) {
  returns (base_mortality)
}
generate_excess_mortality <- function(age,time) {
  returns (excess_mortality)
}
generate_excess_mortality_tau <- function(age,time,tau) {
    returns (excess_mortality_tau)
}
generate_birth_counts <- function(total_births,
                                  birth_dates, delta)

do_simiulation <- function (generate_incidence,
                            generate_base_mortality,
                            generate_excess_mortality,
                            generate_birth_counts,
                            age_steps,
                            age_max,
                            time_steps,
                            birth_dates)
  {

incidence_matrix <- generate_incidence_matrix (age_steps, birth_dates)

base_mortality_matrix <- generate_base_mortality_matrix (birth_dates,age_max,
                                                          generate_base_mortality_fun,
                                                          time_step)

susceptible_survival_rate_matrix <- generate_susceptible_surv_rate(incidence_matrix,
                                                                   base_mortality_matrix)

susceptible_cumulative_survival_matrix <- generate_susceptible_cumulative_survival_matrix(susceptible_survival_rate_matrix)


susceptible_pop_counts <- generate_susceptibles(susceptible_cumulative_survival_matrix,
                                                birth_counts)


infected_mortality_matrix <- generate_infected_mortality_matrix (generate_base_mortality_fun,
                                                                 generate_excess_mortality_fun,
                                                                 birth_dates, age_steps) # excess + baseline mortality

infected_tau_0_counts <- generate_infected_tau_0(incidence_matrix,
                                                 susceptible_pop_counts,
                                                 susceptible_survival_rate_matrix)

infected_mortality_array <- generate_infected_mortality_array (generate_base_mortality_fun,
                                                               generate_excess_mortality_tau_fun,
                                                               birth_dates, age_steps) # excess + baseline mortality

infected_pop_counts <- generate_infected_population (infected_tau_0_counts,
                                                     infected_mortality_array) # do we need a matrix version of this

}





