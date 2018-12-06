library(testthat)
library(population.simulation)

test_check("population.simulation")

# test: generate_base_mortality



Backgrnd_Mortality_var <- function(time, age) {return(0.05)}
age_step = 2
birth_dates = 0:5
generate_base_mortality = Backgrnd_Mortality_var


x <-  generate_base_mortality_matrix(age_step, birth_dates , generate_base_mortality)


#passes
 test_that("checks the dimensions of the matrix", {
   expect_equal(dim(x), c((length(birth_dates)+age_steps), age_steps))

 })

 #fails

 test_that("checks the dimensions of the matrix", {
   expect_equal(dim(x), c(3,2))
 })
