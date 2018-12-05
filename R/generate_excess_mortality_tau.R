#generate_excess_mortality
# example -generate_excess_mortality_tau(t = 14, ta = 50)

generate_excess_mortality_tau <- function(t, ta, constant = 0.05, age_min = 0,
                                                                  age_max = 50,
                                                                  exmin =0.01,
                                                                  exfin =0.05)
  {

  Ex_mort_tau = ifelse(t <= age_min, 0,
                   ifelse(t <= age_max, exmin + ((exfin - exmin)/(age_max - age_min)) * ta *(t - age_min),
                          0))

  return(Ex_mort_tau)
}







