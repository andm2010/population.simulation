
######################################################################################
#Age time structured population simulations 

#Written by Laurette Mhlanga 2018/10/29

# simulation of the susceptibles from given initial population distribution 
#S(0, t_1:t_2) a subject to some attrition rates

#####################################################################################
rm(list = ls())
library(lubridate)



Birth_rates <- function(t_1, t_2, Delta){
  Time  = seq(t_1, t_2, Delta)
  birth_rates = seq(Time)/sum(Time)
  
  return(birth_rates)
}

sum(Birth_rates(t_1 = 1,  
                t_2 = 5, 
                Delta =1))


Make_birth_counts <- function(Total_births, 
                              Birth_rate = Birth_rates){
  
  
  
  birthcounts =  Birth_rate * Total_births
  
  return(birthcounts)
}


Make_birth_counts(Total_births = 10000,
                  Birth_rate = Birth_rates(t_1 = 1,  
                                           t_2 = 5, 
                                           Delta = 1))

Age_distribution <- function(t_1 = 1,
                             t_2 = 5,
                             Delta = 1,
                             Total_births = 10000){
  
  age_distribution = (rev(seq(t_1, t_2, Delta))/sum(seq(t_1, t_2, Delta)))* Total_births
  
  return(age_distribution)
}





Incidence_var_a <- function(t, conc = 0.05, agemin = 0,  
                            agemax = 50,  agepeak= 25, 
                            Imin =0.01,  Ipeak =0.05, 
                            Ifin =0.02){
 
  # consider providing some guidance about how a user may define incidence function 
  #varying Inicdence 
  incidence = ifelse(t <= agemin, 0, 
                     ifelse(t <= agepeak, Imin + ((Ipeak - Imin)/(agepeak - agemin)) * (t - agemin),
                            ifelse(t <= agemax, Ipeak + ((Ifin - Ipeak )/(agemax - agepeak)) * (t - agepeak), 0)))
  
  return(incidence)
}



Excess_Mortality_var_a <- function(t, conc = 0.05, agemin = 0,  
                                   agemax = 50,   
                                   exmin =0.01,  
                                   exfin =0.05){
  #varying mortality
  Ex_mort = ifelse(t <= agemin, 0, 
                   ifelse(t <= agemax, exmin + ((exfin - exmin)/(agemax - agemin)) * (t - agemin),
                          0))
  
  return(Ex_mort)
}



Backgrnd_Mortality_var <- function(t, conc = 0.01, agemin = 1,  
                                   agemax = 50,   
                                   exmin =0,  
                                   exfin =0.01){
  #varying mortality
  Ex_mort = ifelse(t <= agemin, 0, 
                   ifelse(t <= agemax, exmin + ((exfin - exmin)/(agemax - agemin)) * (t - agemin),
                          0))
  
  return(Ex_mort)
}


##############################################################################################################################
##############################################################################################################################
##############################################################################################################################

Discretised_Susceptible_Survival_Prob <- function(Ages,
                                                  Time, 
                                                  Incidence, 
                                                  Mortality, 
                                                  Del
){
  
  
  #function calculates the dicretised survival probabilities i.e. prob of not getting 
  #infected and the probability of not dying for a specific age and time.  
  
  Survival_prob = matrix(0, nrow = length(Time), ncol =  length(Ages))
  
  for (aa in Ages){
    for(tt in Time){ 
      
      Survival_prob[tt, aa] =  1 - (Incidence(tt, aa) + Mortality(tt, aa))
    }
  }
  
  return(Survival_prob)
  
}



##################################################################################################################
##################################################################################################################


Susceptibles_pop <- function(Age,
                             Time,
                             prob_survival,
                             Birth_counts,
                             Age_distribution
                             ){
  
  #calculates the total number of the susceptible population (disritised manner)at a given Age and Time 
  #for a range of initial values of S(t_1:t_2, 0) and ages them through Delta_t. 
  #Returns a matrix
  
  Susceptible  = matrix(0, nrow = length(Time), ncol =  length(Age))
  
  Susceptible[(1:length(Time)), 1] = Birth_counts
  Susceptible[1, (1:length(Age))]  = Age_distribution
  
  Times = 2:length(Time)
  
  for (aa in Age[-length(Age)]){

    Susceptible[2:length(Time) , aa +1] =  (prob_survival[ 1:length(Time),aa] * Susceptible[ 1:length(Time),aa])[-length(Age)]
    
    
  }
  return(Susceptible)
  
}  


###################################################Susceptible Population###########################################################
####################################################################################################################################


Discretised_Susceptible_Cumulative_Survival_Prob <- function(Age,
                                                             Time,
                                                             Survival_Prob
                                                             ){
  #calculates the cumulative probability of survival in the susceptible population for a given age and time
  #the current survival of a specific age and time is multiplied by the cumulative probabilities from previous t - 1 and a-1.
  
  Cumulative_incidence = matrix(0, nrow = length(Time) + 1, ncol = length(Age) + 1)
  
  Cumulative_incidence[ ,1] = rep(1, length(Age)+1)
  Cumulative_incidence[1 , ] = rep(1, length(Time)+1)
  
  for (tt in Time){
    for (aa in Age){
      
      Cumulative_incidence[tt + 1, aa + 1] = Survival_Prob[tt + 1, aa + 1] * Cumulative_incidence[tt, aa]
    }
  }
  return(Cumulative_incidence)
}



#######################################################################################################################################################
#######################################################################################################################################################

Susceptibles_Cum_Pop <- function(Age,
                                 Time,
                                 Cum_prob_survival,
                                 Birth_counts,
                                 Age_distribution
                                 ){
  
  #calculates the total number of the susceptible population (disritised manner)at a given Age and Time 
  #for a range of initial values of S(t_1:t_2, 0) and ages them through Delta_t. 
  #Returns a matrix
  
  Susceptible  = matrix(0, nrow = length(Time)+1, ncol =  length(Age)+1)
  
  Susceptible[(1:nrow(Susceptible)), 1] = Birth_counts
  Susceptible[1, (1:ncol(Susceptible))]  = Age_distribution
  
  #Times = 2:length(Time)
  
  for (tt in Time){
    for (aa in Age){
      

      if (tt > aa){
        
        Susceptible[tt +1, aa + 1] = Cum_prob_survival[tt + 1, aa + 1] *  (Susceptible[ , 1])[(tt - aa)+1]
      
      }else{
        
        Susceptible[tt +1, aa + 1] = Cum_prob_survival[tt + 1, aa + 1] *  (Susceptible[1, ])[(aa - tt)+1]
        
        }
    
    }
  }
  return(Susceptible)
  
}  



############################################################################################################################################
######################################### OPTIONS C ########################################################################################

Vectorised_Susceptibles <- function(Age,
                                    Time,
                                    Cum_prob_survival,
                                    Birth_counts,
                                    Age_distribution 
                                    ){

#approach 2 to Calculating the number of susceptibles 
# one for loop amd a switch 
delta_d <- row(Cum_prob_survival) - col(Cum_prob_survival)
sus = matrix(0, ncol = length(Age) + 1 , nrow = length(Time) + 1)

seQ = min(delta_d):max(delta_d)

for (dd in seQ){
  
  if (dd >= 0){
    
  sus[delta_d == dd]  = Cum_prob_survival[delta_d == dd] * Birth_counts[dd + 1]
  
  }else{
    
    sus[delta_d == dd] = Cum_prob_survival[delta_d == dd] *  Age_distribution[abs(dd) + 1]
  }
}
return(sus)
}



#######################################################################################################################################################
#######################################################################################################################################################




# Discretised_Infected_Survival_Prob <- function(Ages,
#                                                Time,
#                                                Natural_Mortality,
#                                                Excess_Mortality
# ){
#   
#   
#   #function calculates the attrition rates i.e. prob of not getting
#   #infected and the probability of not dying for a specific age and time.
#   
#   Infected_Survival_Prob  = matrix(0, nrow = length(Time), ncol =  length(Ages))
#   
#   for (aa in Ages){
#     for (tt in Time){
#       
#       
#       Infected_Survival_Prob[tt, aa] =  1 - (Natural_Mortality(tt, aa) + Excess_Mortality(tt, aa))
#     }
#   }
#   
#   return(Infected_Survival_Prob)
#   
# }
#############################################################################################################################
##################################################### Discretised_Infected_Survival_Prob ####################################


Discretised_probability_of_infection <- function(Ages,
                                                 Time, 
                                                 Incidence
){
  
  
  #function calculates the attrition rates i.e. prob of not getting 
  #infected and the probability of not dying for a specific age and time.  
  
  infection_prob =  matrix(0, nrow = length(Time) + 1, ncol =  length(Ages) + 1)
  
  infection_prob[1, ]  = infection_prob[, 1] = rep(0, ncol(infection_prob)) 
  
  for (aa in Ages){
    for(tt in Time){ 
      
      infection_prob[tt + 1, aa + 1] =  Incidence(tt, aa)
    }
  }
  
  return(infection_prob)
}



#############################################################################################################################
##########################################Initially_Infected_individuals#####################################################

Initially_Infected  <- function(Ages,
                                Time,
                                Susceptible,
                                Birth_counts,
                                Age_distribution,
                                Prob_infection
                               ){
  
  
  #Infected_0 = Susceptible %*% Prob_infection 
  
  # #the function simulates the infected population at time t, aged a, who have been infected for 
  # #time tau i.e all individuals infected at a = aa_0 (where aa_0 id the age of infection).
  # 
  Infected_0 = matrix(0, nrow = length(Time) + 1, ncol =  length(Ages) + 1)
  
   
  for (aa in 1:ncol(Infected_0)){

    Infected_0[ ,aa] =  Prob_infection[ ,aa] * Susceptible[ ,aa]
  }
  
  
  
  return(Infected_0)
}
####################################################################################################################################
#######################################Survival probabilities of the Infected#######################################################


Discretised_Infected_Survival_Prob <- function(Ages,
                                               Time, 
                                               Back_Mort, 
                                               Excess_Mort
                                               ){
  
  
  #function calculates the dicretised survival probabilities i.e. prob of not getting 
  #infected and the probability of not dying for a specific age and time.  
  
  Survival_prob = matrix(0, nrow = length(Time)+1, ncol =  length(Ages)+1)
  
  for (aa in Ages){
    for(tt in Time){ 
      
      Survival_prob[tt+1, aa+1] =  1 - (Excess_Mort(tt+1, aa+1) + Back_Mort(tt+1, aa+1))
    }
  }
  
  return(Survival_prob)
  
}

####################################################################################################################################
#######################################Survival probabilities of the Infected#######################################################



Infected_Overall <- function(Initially_infected,
                             Survival_prob){
  
  infected = matrix(0, nrow = ncol(Initially_infected), ncol =  nrow(Initially_infected))
  
  for (aa in 1:ncol(Initially_infected)){
  
    infected[, aa] = Initially_infected[,aa] * Survival_prob[, aa]
  
  }
  
  return(infected)
  
}




####################################################################################################################################
######################################Discretised_Susceptible_Cumulative_incidence_Prob#############################################

  Discretised_Susceptible_Cumulative_incidence_Prob <- function(Age,
                                                              Time,
                                                              Survival_Prob
# Age; vector  
# Time; vector
# Surv; - previous function - Discretised_Susceptible_Survival_Prob
                                                              ){
  
  Cumulative_incidence = matrix(0, nrow = length(Time) + 1, ncol = length(Age) + 1)
  
  Cumulative_incidence[ ,1] = rep(1, length(Age)+1)
  Cumulative_incidence[1 , ] = rep(1, length(Time)+1)
  
  for (tt in Time){
    for (aa in Age){
      
      Cumulative_incidence[tt + 1, aa + 1] = Survival_Prob[tt + 1, aa + 1] * Cumulative_incidence[tt, aa]
    }
  }
  return(Cumulative_incidence)
}

