
####### Remove dead ind. #######

remove_dead<-function(deaths_ID){subset(pop, !(ID %in% deaths_ID))}


###### Sampling (needed for partner match)
resamp  <- function(x, ...) x[sample(length(x),...)]

###### Convert rate to probability and adjust probabilities to time step
adjust_prob <- function(rate, t_per_year){
            t_per_year <- 1 #set to one because adjustment needs to be done to rate
            tp <- 1.0 / t_per_year
            prob = 1-exp(-rate*tp)
            return(prob)
} 



##### Time interval

end_interval <- function(start_date, time_unit){
  
  if (time_unit == 'day') {
    
    end_of_interval <- as.Date(model_param$time_steps_date[i_time_step]) 
  }
  
  else if (time_unit == 'week') {
    
    end_of_interval <- ceiling_date(as.Date(model_param$time_steps_date[i_time_step]), 'week') - days(1)
  }
  
  else if (time_unit == 'month') {
    
    end_of_interval <- ceiling_date(as.Date(model_param$time_steps_date[i_time_step]), 'month') - days(1)
  }
  
  else if (time_unit == 'year') {
    
    end_of_interval <- ceiling_date(as.Date(model_param$time_steps_date[i_time_step]), 'year') - days(1)
  }
  
  return (end_of_interval)
}
