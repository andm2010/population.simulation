#' generate_base_mortality_matrix is a function that returns a matrix of probabilities of mortality for each age and time
#'
#' @param age_step numeric, indicating how many age steps
#' @param birth_dates a vector of two numeric values, indicating the minimum and maximum birthdates
#' @param generate_base_mortality a string, which refers to the name of the user-defined or package default function for mortality as a function of age and time
#' @return a matrix of column length \code{age_step} and row length \code{birth_dates}, variables which are supplied as arguments to \code{generate_base_mortality}
#' @examples
#' mortality_matrix <- generate_base_mortality_matrix(2, 0:5, generate_base_mortality = Backgrnd_Mortality_var)
#'

generate_base_mortality_matrix <- function(age_step,
                                    birth_dates,
                                    generate_base_mortality
                                    ){

  mortality_matrix  = matrix(NA, nrow = length(birth_dates) + age_step, ncol =  length(1:age_step))
  times  = 0:length(birth_dates)

  for (aa in 1:age_step){

    mortality_matrix[times + aa, aa] =  generate_base_mortality(times + aa, aa)
  }
  return(mortality_matrix)
}

mortality_matrix <- generate_base_mortality_matrix(2, 0:5, generate_base_mortality = Backgrnd_Mortality_var)





