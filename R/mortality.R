####################################
#            Mortality             #
####################################

mortality <- function(pop, i_time_step) {
  
  # random number
  random_death       <- runif(nrow(pop),0,1)
  pop$random_death   <- random_death
  
  # surviving prob 
  surv_prob_t        <- surv_prob[ , which(names(surv_prob) %in% c('Age', 'sex', model_param$time_step_year[i_time_step]))] 
  surv_prob_t$sex    <- ifelse(surv_prob_t$sex==1,'M','F')
  colnames(surv_prob_t)[2] <- 'surv_prob'
  
  # assign mortality rates
  pop                <- left_join(pop, surv_prob_t, by = c("age"="Age", "sex"="sex"))
  pop$hh_match       <- ifelse(pop$hh_position %in% c('union','union_w_child','multi_gen_union'),'union', ifelse(pop$hh_position %in% c('single','other','non_family','child','multi_gen_single','single_parent'),'non_union','collective'))
  pop                <- left_join(pop, mort_adj[,-3], by=c('sex','age','hh_match'='hh_position'))
  pop$ratio[pop$age<30] <- 1
  pop$mort_rate      <- pop$surv_prob*pop$ratio
  pop$surv_prob      <- 1-mxtoqx(pop$mort_rate)

  # set prob. to zero for emigrants
  emi_pop             <- emi_pop[order(emi_pop$ID),]
  pop                 <- pop[order(pop$ID),]            
  pop$surv_prob[pop$ID %in% emi_pop$ID] <- 1   # avoid that an individual dies after emigrating

  # ID of dead individuals
  deaths             <- pop[pop$random_death > pop$surv_prob, c('ID','ID_partner', 'ID_mother', 'ID_father', 'mother', 'father', 'HH_ID', 'event_date','age')]

  # Sample a random date for our death events
  date_death_events  <- sample(seq(as.Date( model_param$time_steps_date[i_time_step]), as.Date(end_time_step), by="day"), nrow(deaths), replace = TRUE)
  
  # Assign date to our events
  deaths$event_date_mort  <- date_death_events

  # Adjust event_date if before other transition in household
  adj_event_mort     <- deaths[deaths$event_date_mort < deaths$event_date & !is.na(deaths$event_date),]
  
  
  if (nrow(adj_event_mort)>0) {
  
    for (i in 1:nrow(adj_event_mort)) {
    
      adj_event_mort$new_event_date[i]       <- resamp(seq(adj_event_mort$event_date[i], as.Date(end_time_step), by="day"), 1)
      adj_event_mort$event_date[i]           <- as.Date(adj_event_mort$new_event_date[i], origin = '1970-01-01')
  } 
  
  deaths$event_date_mort[deaths$ID %in% adj_event_mort$ID] <- as.Date(adj_event_mort$new_event_date, origin = '1970-01-01')
  
  }
  
  deaths                                   <- deaths[,-8]
  colnames(deaths)[9]                      <- 'event_date'
  
  # Add event type
  deaths$event_type  <- 'death'
 
  # remove the added columns
  pop                <- pop[ , -which(names(pop) %in% c("random_death","surv_prob","ratio"))]

  return(deaths)
}









