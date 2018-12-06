context("generate_birth_counts")

library(testthat)
library(population.simulation)

test_check("population.simulation")


# define function
generate_birth_counts <- function(total_births,
                                  birth_dates, delta){

  birth_rate <- function(birth_dates, delta){
    times = seq(0, max(birth_dates) - min(birth_dates), delta)
    birth_rates = seq(times)/sum(times)

    return(birth_rates)
  }

  birth_counts =  birth_rate(birth_dates, delta) * total_births

  return(birth_counts)
}

# Call function
generate_birth_counts(1000, 1984:1990, 1)

# Run expectations
expect_is(x, "numeric")
expect_is(x, "factor")
expect_length(x, 1)
expect_length(x, 2)
expect_equivalent(length(x), length(max(birth_dates):min(birth_dates)))
expect_false(is.array(x))
expect_true(is.vector(x))





