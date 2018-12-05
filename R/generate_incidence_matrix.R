#' generate_incidence_matrix is a function that returns a matrix of probabilities of infection accross indicated ranges of age and time
#'
#' @param age_steps numeric, indicates the number of steps forward that you would like to age your population in \code{do_sim}
#' @param birth_dates ?
#' @param generate_incidence a user defined or package default function which takes as arguments age and time and returns a numberic rate of incidence
#' @return a matrix of column length \code{age_steps} and row length \code{birth_dates} .
#' Values in matrix are numeric from 0-1, which represent the probability of becoming infected at \code{t} and \code{age_steps}
#' @examples y <-  generate_incidence_matrix (age_steps = 2, birth_dates = 10:15, generate_incidence)


generate_incidence_matrix <- function(age_steps,birth_dates,generate_incidence_fun)
  {

  incidence_matrix  = matrix(NA, nrow = length(birth_dates) + age_steps, ncol =  length(1:age_steps))
  times  = 0:length(birth_dates)

  for (aa in 1:age_steps){

    incidence_matrix[times + aa, aa] =  generate_incidence(times + aa, aa)

  }

  return(incidence_matrix)
}

y <-  generate_incidence_matrix (age_steps = 2, birth_dates = 10:15,
                                 generate_incidence_fun = generate_incidence)

