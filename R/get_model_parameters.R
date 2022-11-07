
get_model_parameters <- function(run_config){
  
  # initiate the parameter list
  # note: a 'list' can aggregate single numbers and vectors/matrices
  model_param <- list()
  
  # add run configuration
  model_param[names(run_config)] <- run_config
  
  # time perspective and parametes [NEW] 
  # note: using the 'timeDate' package
  model_param$date_start       <- timeDate(start_date, zone='CET', FinCenter = 'Brussels')
  model_param$time_unit        <- run_config$time_unit
  model_param$time_steps_date  <- timeSequence(from = model_param$date_start, 
                                             to = end_date, 
                                             #length.out = model_param$num_time_steps, 
                                             by         = model_param$time_unit)
  model_param$time_steps_char  <- as.character(model_param$time_steps)
  model_param$time_step_year   <- format(as.Date(model_param$time_steps_date, format="%Y-%m-%d"),"%Y")
  model_param$t_per_year       <- ifelse(model_param$time_unit=='week', 52, ifelse(model_param$time_unit=='day', 365, ifelse(model_param$time_unit=='month' ,12,1))) 

  # temporary?
  model_param$time_steps_int <- seq(1,length(model_param$time_steps_date))
  

  #Lactational amenorrhea (LA) in days -> How long do women in Belgium breastfeed?
  model_param$LA<- 365/12*12 # 9 months pregnancy + 3 months
  
  # age and household parameters
  model_param$max_age_child <- 17
  
  # return the model parametes
  return(model_param)
    
}










