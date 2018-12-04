#' generate_susceptibles is a function that returns a matrix of numeric values for age and time, representing the number of individuals within the population who are alive and not HIV infected
#' 
#' @param survival_matrix matrix of survival probabilities for \code{age} and \code{t}, defined by package's \code{generate_susceptible_cumulative_survival function} - Development note -  should paramter name be changed to "susceptible_cumulative_survival_matrix"
#' @param births a vector of numberic values representing the number of HIV negative births at age and time  - Development note - should parameter be defined as "negative_births"
#' @return a matrix of row length \code{age_steps} and column length \code{t}. Numeric values in each cell of the matrix represent the number of individuals in the population who are alive and not HIV infected
#' @examples T
#' 
#' w <-  generate_susceptible_surv_rate(matrix(seq(0.01, 0.16, 0.01), ncol = 2 , nrow = 8), matrix(seq(0.01, 0.16, 0.01), ncol = 2 , nrow = 8))
#' generate_susceptibles(survival_matrix = w,births = rep(100, 4))
#' 

generate_susceptibles <- function(survival_matrix = susceptible_cumulative_survival_matrix,
                                  births = negative_births
                                  ){
  
  delta_d <- row(survival_matrix) - col(survival_matrix)
  susceptible_pop_counts  = matrix(NA, nrow = nrow(survival_matrix), ncol =  ncol(survival_matrix))
  
  susceptible_pop_counts[(1:length(births)), ] =  births
  
  seQ = min(delta_d):max(delta_d)
  
  for (aa in seQ){
    
    if (aa >= 0){
    
    susceptible_pop_counts[delta_d == aa]  = survival_matrix[delta_d == aa] * births[aa + 1]
 
    }else{
    
    susceptible_pop_counts[delta_d == aa] = NA 
    
      }
  }
  return(susceptible_pop_counts)
  
}  

