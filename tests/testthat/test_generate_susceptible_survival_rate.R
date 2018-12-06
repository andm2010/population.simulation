library(testthat)
library(population.simulation)

test_check("population.simulation")

# test: generate_susceptible_survival_rate

incidence_matrix = matrix(seq(0.01, 0.16, 0.01), ncol = 2 , nrow = 8)
base_mortality_matrix = matrix(seq(0.01, 0.16, 0.01), ncol = 2 , nrow = 8)

x <- generate_susceptible_surv_rate(incidence_matrix ,
                                    base_mortality_matrix)


#passes
test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), dim(incidence_matrix))
  expect_equal(dim(x), dim(base_mortality_matrix))
  expect_equal(dim(incidence_matrix), dim(base_mortality_matrix))
})

#fails

test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), c(3,2))
  expect_equal(dim(x), c(3,2))
  expect_equal(dim(incidence_matrix), c(3,2))
})
