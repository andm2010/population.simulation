# generate_birth_counts


#'Generate birth counts
#' @param total_births the total number of births between maximum and minimun birth_dates given.
#' @param delta the time step between consecurtive birth_dates
#' @param birth_dates the calender dates of births.
#' @return Returns a vector of bithcounts  froms the inputs \code{total_births},\code{delta}, and \code{birth_dates} of length \code(max(birth_dates):min(birth_dates))
#' @examples
#' generate_birth_counts(1000, 1984:1990, 1)
#' generate_birth_counts(10000, 2005 : 2018, 1)


#can we make one function as per suggestions but this can save as a toy example

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



