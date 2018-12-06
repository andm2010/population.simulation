# a function that returns a numeric vector of birth counts from the arguments of total births, a range of birth dates and an indicted birth rate
#' @usage generate_birth_counts \code{total_births} (\code{birth_dates}, \code{delta})
#' @param total_births the total number of births between maximum and minimun birth_dates given.
#' @param delta the time step between consecurtive birth_dates
#' @param birth_dates the calender dates of births.
#' @return Returns a vector of bith counts  from the inputs \code{total_births},\code{delta}, and \code{birth_dates} of length \code(max(birth_dates):min(birth_dates))
#' @examples
#' generate_birth_counts(1000, 1984:1990, 1)
#' generate_birth_counts(10000, 2005 : 2018, 1)


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



