context("generate_incidence")

library(testthat)
library(population.simulation)

test_check("population.simulation")

# define function
generate_incidence <- function(t, age ) {
  incidence = exp(-(1/age)*t)*0.1
  return(incidence)
}

# Call function
x <- generate_incidence(t=25, age=25 )

# Run expectations

expect_is(x, "numeric")
expect_is(x, "factor")
expect_length(x, 1)
expect_length(x, 2)
expect_equivalent(length(x), length(t))
expect_equivalent(length(x), length(age))
expect_false(is.array(x))
expect_true(is.vector(x))

# passes
test_that("checks that returned value is numeric", {
  expect_is(x, "numeric")
  })
#fails
test_that("checks that returned value is numeric", {
  expect_is(x, "factor")
})


#Or......

generate_incidence <- function(t=25, constant = 0.05, agemin = 0,
                               agemax = 50,  agepeak= 25,
                               Imin =0.01,  Ipeak =0.05,
                               Ifin =0.02){

  incidence = ifelse(t <= agemin, 0,
                     ifelse(t <= agepeak, Imin + ((Ipeak - Imin)/(agepeak - agemin)) * (t - agemin),
                            ifelse(t <= agemax, Ipeak + ((Ifin - Ipeak )/(agemax - agepeak)) * (t - agepeak), 0)))

  return(incidence)
}

y <- generate_incidence()

# Run expectations
expect_is(x, "numeric")
expect_is(x, "factor")
expect_length(x, 1)
expect_length(x, 2)
expect_equivalent(length(x), length(t))
expect_equivalent(length(x), length(age))
expect_false(is.array(x))
expect_true(is.vector(x))
expect_gt(y, Ipeak)
expect_gt(y, Imin)
