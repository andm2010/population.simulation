library(testthat)
library(population.simulation)

test_check("population.simulation")

# test: generate_incidence_matrix


age_steps = 2

birth_dates = 10:15

generate_incidence_fun = generate_incidence2


x <- generate_incidence_matrix(age_steps, birth_dates, generate_incidence_fun)


#passes
test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), c((length(birth_dates) + age_steps), age_steps))

})

#fails

test_that("checks the dimensions of the matrix", {
  expect_equal(dim(x), c((length(birth_dates)), age_steps))

})



