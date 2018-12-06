library(testthat)
library(population.simulation)

test_check("population.simulation")

# test: generate_susceptible_cumulative_survival


susceptible_survival_rate_matrix = matrix(seq(0.01, 0.16, 0.01), ncol = 2 , nrow = 8)

x <- generate_susceptible_cumulative_survival(susceptible_survival_rate_matrix )


#passes
test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), dim(susceptible_survival_rate_matrix))
})

#fails

test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), c(3,2))
})
