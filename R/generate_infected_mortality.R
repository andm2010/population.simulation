#'calculates the probability of survival in the infected populations
#' @param age_step the aging step of the initial birth counts   
#' @param birth_dates calender dates of birth 
#' @param ex_mortality_fun function of excess mortality for a given age, time, and time since infection.
#' @param base_mortality_fun baseline mortality as a function of age and time 
#' @return returns an array of dimension time, age and time since infection see example
#' @examples
#' x = function(time = 4, age = 0:2, tau = 0:2){return(0.2)}
#' y = function(time = 4, age = 0:2) {return(0.02)}
#'  
#' generate_infected_mortality(age_step = 2, birth_dates = 1992:1995, ex_mortality_fun = x, base_mortality_fun = y)  



generate_infected_mortality <- function(age_step,
                                        birth_dates, 
                                        ex_mortality_fun,
                                        base_mortality_fun
){
  
  
  
  times  <-  0 : length(birth_dates)
  ages <- 0:age_step
  infected_mortality_array <-  array(NA, dim = c(length(times)+ age_step , length(ages), length(ages)))
  
  
  for (aa in ages){
    for (ta in ages){
      
      infected_mortality_array[times + aa +1, aa + 1, ta + 1] = base_mortality_fun(times + aa, aa) * ex_mortality_fun(times + aa, aa, ta)
      
      
      
    }
  }
  
  return(infected_mortality_array)
  
}



