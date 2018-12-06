context("do.simulation")

library(testthat)
library(population.simulation)

test_check("population.simulation")

#passes
test_that("checks that the return is a list", {
  test_pop <- do_simiulation(total_births = 1000, birth_dates = 1945:1950, delta = 1,
                             age_steps = 3,
                             generate_birth_counts_fun = generate_birth_counts,
                             generate_incidence_fun = generate_incidence,
                             generate_base_mortality_fun = generate_base_mortality,
                             generate_base_mortality_tau_fun = generate_excess_mortality_tau)
  expect_true (is.list(test_pop))
})


