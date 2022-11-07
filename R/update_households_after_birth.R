
create_unions_after_birth <- function(pop, df_leaving){
  
  #select individuals without a partner who are eligible for entering into a union
  pop_single_flag    <- pop$union==0 & pop$age >= union_age_lim & !(pop$hh_position %in% c('union','multi_gen_union','union_w_child'))               
  pop_single_columns <- c("ID","HH_ID","sex","age","age_group","transition","hh_position","ID_partner","union", "event_date",'age_group_pop','age_group_hh')
  pop                <- setDF(pop)
  pop_single         <- pop[pop_single_flag,pop_single_columns]
  pop_single$age_group_trans   <- findInterval(pop_single$age, seq(0,open_age_trans_rates, age_int))
  pop_single$row_id            <- 1:nrow(pop_single)
  pop_single$age_partner       <- NA
  
  # select males entering union
  flag_male_singles  <- (pop_single$transition %in% c('union','new_union') ) & (pop_single$sex=='F')            
  pop_single_males   <- pop_single[flag_male_singles,]
  
  # HH trans rate for potential male partners -> adjust age group
  hh_trans_male       <- hh_trans[hh_trans$sex=='M' & hh_trans$hh_position_target=='union' & hh_trans$parents==1,]

  # add hh-trans probability
  pop_single          <- left_join(pop_single, hh_trans_male[,c('age','hh_position','rate')], by=c('age_group_trans'='age','hh_position'='hh_position'))
  
  # if no individuals in this population to apply this transitions => stop
  if(!any(flag_male_singles)){
    
    return(list(pop, event_hh))
    
  }
  # else... continue
  
  # select unique male [age] combinations
  age_opt <- as.data.frame(unique(pop_single_males$age_group))
  
  # get local copy of max hh id
  max_hh_id <- max(pop$HH_ID)
  
  # loop over all female age groups
  for(i_opt in 1:nrow(age_opt)){
    
    # get indices of the target male subpopulation    
    flag_subpop_single_males <- pop_single_males$age_group == age_opt[i_opt,]
    flag_subpop_single_males <- ifelse(is.na(flag_subpop_single_males), FALSE, flag_subpop_single_males)
    
    # get age specific probability
    prob_vec_age             <- partner_age_prob[partner_age_prob$age==age_opt[i_opt,], c('age_partner','prop')]
    prob_vec_age$prop[prob_vec_age$age_partner==8] <- 0
    
    # sample ages of male partners 
    sample_age_male_partner  <- resamp(prob_vec_age$age_partner, size=sum(flag_subpop_single_males), prob=prob_vec_age$prop, replace=T) 
    
    female_partner_row_id    <- c()
    
    for(age_partner_i in unique(sample_age_male_partner)){
      
      age_freq              <- sum(sample_age_male_partner==age_partner_i)
      
      # candidate partners
      partner_pop           <- pop_single$sex == 'M' & pop_single$union==0 & pop_single$age_group_trans == age_partner_i    # use only females with 'no_trans' otherwise there is a risk of females going from one union to another but without corresponding ordering of dates of event
      candidates            <- pop_single[partner_pop,]
      candidates$rate[is.na(candidates$rate)] <- 0
      
      sample_id_male_partner<- resamp(candidates$row_id, size=age_freq, candidates$rate, replace=F)
      female_partner_row_id <- c(female_partner_row_id , sample_id_male_partner)
      
    } # end loop male age group
    
    male_partner_row_id     <- pop_single_males$row_id[flag_subpop_single_males]
    
    # update pop data: single males 
    pop_single$ID_partner[male_partner_row_id]   <- pop_single$ID[female_partner_row_id]
    pop_single$HH_ID[male_partner_row_id]        <- (max_hh_id+1):((max_hh_id+length(male_partner_row_id)) )   #New HH ID
    pop_single$age_partner[male_partner_row_id]  <- pop_single$age[female_partner_row_id]
    
    
    # update pop data: single females 
    pop_single$event_date[female_partner_row_id] <- pop_single$event_date[male_partner_row_id]
    pop_single$ID_partner[female_partner_row_id] <- pop_single$ID[male_partner_row_id]
    
    # add female match to event_hh (needs to be here in order to not change household-ID and ID-partner. The new values for these variables are added later)
    event_female_match                           <- pop_single[female_partner_row_id, c('ID','HH_ID','hh_position', 'ID_partner','transition','event_date','sex','age', 'age_group_pop','age_group_hh')]
    event_female_match$event_date                <- as.Date(event_female_match$event_date, origin = '1970-01-01')
    event_female_match$transition                <- 'union_w_child'
    df_leaving                                   <- rbind.fill(df_leaving, event_female_match)
    
    pop_single$HH_ID[female_partner_row_id]      <- pop_single$HH_ID[male_partner_row_id]
    
    # update household details
    partners_row_id                              <- c(male_partner_row_id,female_partner_row_id)
    pop_single$hh_position[partners_row_id]      <- 'union_w_child'
    pop_single$union[partners_row_id]            <- 1
    pop_single$transition[partners_row_id]       <- 'updated'
    
    # update max household ID
    max_hh_id                                    <- max_hh_id+length(female_partner_row_id)
    
  } # end for-loop: single male ages
  
  # copy single pop data back to 'pop' now with updated union information
  pop[pop_single_flag,pop_single_columns] <- pop_single[,pop_single_columns]     
  
  # record partner match
  record_match                                   <- pop_single[flag_male_singles,c('age','age_partner')]
  
  # event-log KOMMET HERTIL
  df_leaving                                     <- merge(df_leaving, pop_single[pop_single$hh_position=='union_w_child',c(1,2,7,8)], by='ID', all.x = TRUE, all.y = FALSE)
  names(df_leaving)                              <- c('ID','HH_ID','hh_position','sex','age','transition','event_date','age_group_pop','age_group_hh','ID_partner','HH_ID_target','hh_position_target','ID_partner_target')
  
  return(list(pop, df_leaving, record_match))
}
