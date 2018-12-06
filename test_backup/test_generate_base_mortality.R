context("generate_base_mortality")

library(testthat)
library(population.simulation)

test_check("population.simulation")

# define function
generate_base_mortality <- function(t, age ) {
  base_mortality = exp(-(1/age)*t)*0.1
  return(base_mortality)
}

# Call function
x <- generate_base_mortality(t=25, age=25 )

# Run expectations
expect_is(x, "numeric")
expect_is(x, "factor")
expect_length(x, 1)
expect_length(x, 2)
expect_equivalent(length(x), length(t))
expect_equivalent(length(x), length(age))
expect_false(is.array(x))
expect_true(is.vector(x))




#OR......
generate_base_mortality <- function(t, constant = 0.01, age_min = 1,
                                    age_max = 50,
                                    exmin =0,
                                    exfin =0.01){

  base_mortality = ifelse(t <= age_min, 0,
                          ifelse(t <= age_max, exmin + ((exfin - exmin)/(age_max - age_min)) * (t - age_min),
                                 0))

  return(base_mortality)
}
y <- generate_base_mortality()

# Run expectations
expect_is(x, "numeric")
expect_is(x, "factor")
expect_length(x, 1)
expect_length(x, 2)
expect_equivalent(length(x), length(t))
expect_equivalent(length(x), length(age))
expect_false(is.array(x))
expect_true(is.vector(x))
expect_gt(y, exfin)
expect_gt(y, exmin)
