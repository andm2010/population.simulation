library(testthat)
library(population.simulation)

test_check("population.simulation")

# test: generate_infected_mortality
age_steps = 2
birth_dates = 1992:1995

generate_excess_mortality_tau_fun = function(time, age, tau) {return(0.02)}
generate_base_mortality_fun = function(time, age) {return(0.02)}

x <- generate_infected_mortality_array(age_steps , birth_dates ,
                                       generate_excess_mortality_tau_fun ,
                                       generate_base_mortality_fun )


#passes
test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), c(7,3,3))
})

#fails

test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), c(3,2))
})
