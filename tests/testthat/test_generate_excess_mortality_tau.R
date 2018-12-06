context("generate_excess_mortality_tau")

library(testthat)
library(population.simulation)

test_check("population.simulation")

# define function
generate_excess_mortality_tau <- function(t, ta, constant = 0.05, age_min = 0,
                                          age_max = 50,
                                          exmin =0.01,
                                          exfin =0.05)
{

  Ex_mort_tau = ifelse(t <= age_min, 0,
                       ifelse(t <= age_max, exmin + ((exfin - exmin)/(age_max - age_min)) * ta *(t - age_min),
                              0))

  return(Ex_mort_tau)
}

# Call function
x <- generate_excess_mortality_tau(t=25, ta=50)

# Run expectations
expect_is(x, "numeric")
expect_is(x, "factor")
expect_length(x, 1)
expect_length(x, 2)
expect_equivalent(length(x), length(t))
expect_equivalent(length(x), length(age))
expect_false(is.array(x))
expect_true(is.vector(x))
