library(testthat)
library(population.simulation)

test_check("population.simulation")

# test: generate_infected_tau_0


incidence_matrix =  matrix(rep(0.04, 4), 2, 2)
susceptible_pop_counts = matrix(rep(100, 4), 2, 2)

x <- generate_infected_tau_0(incidence_matrix,
                             susceptible_pop_counts)


#passes
test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), dim(incidence_matrix))
})

#fails

test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), c(3,2))
})
