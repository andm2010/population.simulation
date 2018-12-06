library(testthat)
library(population.simulation)

test_check("population.simulation")

# test: generate_infected_population

w = function(time = 4, age = 0:2, tau = 0:2){return(0.2)}
y = function(time = 4, age = 0:2) {return(0.02)}
infected_mortality_matrix = generate_infected_mortality_array(age_step = 2,
                                      birth_dates = 1992:1995,
                                      generate_excess_mortality_tau_fun = w,
                                      generate_base_mortality_fun = y)

infected_pop_counts = matrix(1:21, 7, 3)

x <- generate_infected_population(infected_mortality_matrix , infected_pop_counts)



#passes
test_that("checks the dimensions of the matrix", {
 expect_equal(dim(x[, , 1]), dim(infected_pop_counts))
 })

#fails

test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), dim(infected_mortality_matrix))
  expect_equal(dim(infected_mortality_matrix), dim(infected_pop_counts))
})
