# Package Funcations Workflow
# Created 3 Dec 2018
generate_incidence <- function(age, time) # set default, as database object, existing "Incidence_var_a"
generate_mortality <- function(age,time) # set default, as database object, existing "Background_Mortality_var_a"
generate_excess_mortality <- function(age,time, tau) # set default, as database object, existing "Excess_Mortality_var_a", tau (time since infection) is 3d of array
generate_birth_counts <- function(birth_scheme, time_step, birth_dates) # set default, as database object, existing "Background_Mortality_var_a"

do_simiulation <- function (births = constant_births, # as a function
                    incidence = constant_incidence, # as a function
                    base_mortality = baseline_mortality, # as a function
                    excess_mortality = excess_mortality, # as a function
                    age_max = 49, # as a value, age min is set to 0
                    time_step = 1, # as a value
                    birth_dates = c(date1, date2),
                    perinatal_positivity = 0) # as vector of values, defaults to zero
  {

incidence_matrix <- generate_incidence_matrix(birth_dates, age_max, incidence, time_step)

base_mortality_matrix <-  generate_base_mortality (birth_dates, age_max, base_mortality, time_step)

susceptible_survival_rate_matrix <- generate_susceptible_surv_rate(incidence_matrix, base_mortality_matrix)

susceptible_cumulative_survival_matrix <-  generate_susceptible_cumulative_survival(susceptible_survival_rate_matrix)

birth_counts <- generate_birth_counts(birth_scheme = births, time_step = time_step, birth_date_range = birth_dates)

negative_births <- birth_counts * (1- perinatal_positivity) # find home

positive_births <- birth_counts * perinatal_positivity # find home

susceptible_pop_counts <- generate_susceptibles(survival_matrix=susceptible_cumulative_survival_matrix,births=negative_births)

infected_tau_0_counts <- generate_infected_tau_0(incidence_matrix,susceptible_pop_counts,susceptible_survival_rate_matrix)

infected_mortality_array <- generate_infected_mortality (base_mortality, excess_mortality, birth_dates, age_max) # excess + baseline mortality

infected_pop_counts <- generate_infected_population (infected_tau_0_counts,infected_mortality_array)

}





