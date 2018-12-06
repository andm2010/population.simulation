#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#' @usage \code{generate_base_mortality_matrix} (\code{age_steps},\code{birth_dates},\code{generate_base_mortality_fun})
#' @param age_steps a number. Indicates the number of steps forward each age group will be aged in the simulation by \code{do_sim}
#' @param birth_dates a numeric vector of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
#' @param generate_base_mortality_fun a function which takes as arguments age and time and returns a numberic rate of mortality for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the \code{generate_base_mortality_matrix} function.
#' @return a matrix of column length \code{age_steps} and row length \code{birth_dates}.
#' Values stored in the matrix are numeric-double, from 0-1, which represent the probability of dying at \code{t} and \code{age_steps}
#' @examples mortality_matrix <- generate_base_mortality_matrix(2, 0:5, generate_base_mortality_fun = generate_base_mortality)


generate_base_mortality_matrix <- function(age_step,
                                    birth_dates,
                                    generate_base_mortality_fun
                                    ){

  mortality_matrix  = matrix(NA, nrow = length(birth_dates) + age_step, ncol =  length(1:age_step))
  times  = 0:length(birth_dates)

  for (aa in 1:age_step){

    mortality_matrix[times + aa, aa] =  generate_base_mortality_fun(times + aa, aa)
  }
  return(mortality_matrix)
}



