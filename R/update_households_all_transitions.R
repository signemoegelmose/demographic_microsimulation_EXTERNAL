#################################################
#          Household transitions                #
#################################################


# function to update household transitions for each individual

update_transitions <- function(pop){
  
  pop$transition   <- ifelse((pop$age >= child_age_lim & !(pop$ID %in% fertility_events$ID) & !(pop$ID %in% birth_events)), NA, 'no_trans')   # Only individuals older than 16 can change household
  
  # random number
  random_trans     <- runif(nrow(pop),0,1)
  pop$random_trans <- random_trans
  
  # HH transition probability (overall)
  pop                                             <- left_join(pop, hh_event_rate, by = c("age_group_trans"="age", "sex"="sex", "hh_position"="hh_position", 'parent_pop'='parents'))
  pop[c("rate")][is.na(pop[c("rate")])]           <- 0
  flag_hh_trans                                   <- (pop$random_trans < pop$rate) & is.na(pop$transition)
  flag_hh_trans[is.na(flag_hh_trans)]             <- FALSE
  subset_hh_trans                                 <- pop[flag_hh_trans,]
  pop$transition[flag_hh_trans==FALSE]            <- 'no_trans'
  
  # select unique [age, sex, lipro] combinations
  age_sex_opt      <- as.data.frame(unique(subset_hh_trans[subset_hh_trans$age >= child_age_lim, c('age_group_trans','sex', 'hh_position','parent_pop')]))

      for(i_opt in 1:nrow(age_sex_opt)){

      # get indices of the targeted subpopulation
      flag_subpop  <-  subset_hh_trans$age_group_trans == age_sex_opt$age_group_trans[i_opt] & subset_hh_trans$sex == age_sex_opt$sex[i_opt] & subset_hh_trans$hh_position == age_sex_opt$hh_position[i_opt] & subset_hh_trans$parent_pop == age_sex_opt$parent_pop[i_opt]  # This seems to take a long time.

      # get age, sex and lipro specific transition probability
      hh_pos       <- age_sex_opt$hh_position[i_opt]
      i_age        <- age_sex_opt$age_group_trans[i_opt]
      i_sex        <- age_sex_opt$sex[i_opt]
      i_parents    <- age_sex_opt$parent_pop[i_opt]
      
      # Rates conditional on transition taking place
      prob_transitions   <- hh_trans[hh_trans$age==i_age & hh_trans$sex==i_sex & hh_trans$hh_position==hh_pos & hh_trans$parents==i_parents,] 
      
      # sample transitions
      if (sum(prob_transitions$rate>0)){
        
      sample_transitions <- sample(prob_transitions$hh_position_target, sum(flag_subpop), prob=prob_transitions$rate, replace=T)

      } else {
        
      sample_transitions <- rep('no_trans', sum(flag_subpop))
        
      }
      
      # update pop_data
      subset_hh_trans$transition[flag_subpop] <- sample_transitions
      
      # date of events
      date_hh_events                          <- sample(seq(as.Date(model_param$time_steps_date[i_time_step]), as.Date(end_time_step), by="day"), sum(flag_subpop), replace = TRUE)
      subset_hh_trans$event_date[flag_subpop] <- date_hh_events

      }

  # return the transition column
  subset_hh_trans$event_date                                                                    <- as.Date(subset_hh_trans$event_date, origin='1970-01-01')
  pop$transition[flag_hh_trans]                                                                 <- subset_hh_trans$transition
  pop$transition[pop$transition %in% c('union','multi_gen_union','new_union','union_w_child') & pop$sex=='M']     <- 'no_trans'        # Males entering union are set to 0 (no transition -> indirectly through males)
  pop$transition[pop$hh_position %in% c('union','multi_gen_union','union_w_child') & pop$sex=='M']                <- 'no_trans'        # Males leaving union are set to 0 (no transition -> indirectly through males)
  pop$transition[pop$transition=='union' & pop$age < union_age_lim]                             <- 'no_trans'                          # No union formation of males younger than 15
  pop$transition[pop$transition=='child' & pop$parent_pop==0]                                   <- 'no_trans'                          # No trans to parents if they are not in pop
  
  incl_subset                                                                  <- pop$transition!='no_trans'
  subset_hh_trans                                                              <- subset_hh_trans[subset_hh_trans$ID %in% pop$ID[pop$transition!='no_trans'],]
  pop$event_date[incl_subset]                                                  <- subset_hh_trans$event_date
  
  
  # record event
  event_hh            <-  pop[flag_hh_trans, c('ID','HH_ID','hh_position', 'ID_partner','transition', 'event_date','sex','age')]
  event_hh$event_date <- as.Date(event_hh$event_date, origin='1970-01-01')
  event_hh            <- event_hh[event_hh$transition!='no_trans',]
  
  # remove the added columns
  pop                 <- pop[ , -which(names(pop) %in% c("random_trans","rate"))]
  
  # return
  return(list(pop, event_hh))
  
}




# Function for union dissolution

dissolve_unions <- function(pop, event_hh){
  
  # females leaving union
  flag_pop    <- !(pop$transition %in% c('updated','no_trans','union','multi_gen_union','union_collective', 'union_w_child')) &  ( pop$sex=='F' & pop$hh_position %in% c('union', 'multi_gen_union', 'union_w_child'))
  flag_pop_id <- pop$ID[flag_pop]
  
  # if no individuals in this population to apply this transitions => stop
  if(!any(flag_pop)){
    return(list(pop, event_hh))
  }
  # else... continue
  
  # update pop (individual and former partner)
  flag_dis                                     <- flag_pop | (pop$ID %in% pop$ID_partner[flag_pop])
  subset_hh_trans                              <- pop[pop$ID %in% pop$ID_partner[flag_pop],] 
  
  
  # add to event_hh file
  pop$event_date[(pop$ID %in% pop$ID_partner[flag_pop])] <- pop$event_date[flag_pop]
  event_female_match                                     <- pop[(pop$ID %in% pop$ID_partner[flag_pop]), c('ID','HH_ID','hh_position', 'ID_partner','transition', 'event_date','sex','age')]
  event_female_match$event_date                          <- as.Date(event_female_match$event_date, origin='1970-01-01')
  event_female_match$transition                          <- 'single'
  
  
  ### Draw transition for males 
  # select unique [age, sex, lipro] combinations
  age_sex_opt      <- as.data.frame(unique(subset_hh_trans[, c('age_group_trans','sex', 'hh_position', 'parent_pop')]))
  
  for(i_opt in 1:nrow(age_sex_opt)){
    
    # get indices of the targeted subpopulation
    flag_subpop    <-  subset_hh_trans$parent_pop == age_sex_opt$parent_pop[i_opt] & subset_hh_trans$age_group_trans == age_sex_opt$age_group_trans[i_opt] & subset_hh_trans$sex == age_sex_opt$sex[i_opt] & subset_hh_trans$hh_position == age_sex_opt$hh_position[i_opt] 
    
    # get age, sex and lipro specific transition probability
    hh_pos         <- age_sex_opt$hh_position[i_opt]
    i_age          <- age_sex_opt$age_group_trans[i_opt]
    i_sex          <- age_sex_opt$sex[i_opt]
    i_parent       <- age_sex_opt$parent_pop[i_opt]
    
    # Rates conditional on transition taking place
    prob_transitions       <- hh_trans[hh_trans$age==i_age & hh_trans$sex==i_sex & hh_trans$hh_position==hh_pos & hh_trans$parents==i_parent,]
    prob_transitions$rate[prob_transitions$hh_position_target=='single'] <- ifelse(sum(prob_transitions$rate)==0, 1,prob_transitions$rate[prob_transitions$hh_position_target=='single'])
    
    if(nrow(prob_transitions)==0){
      
      prob_transitions[1,] <- c(i_age,hh_pos, 'single',1,i_sex, i_parent)
      
    }
    
    # sample transitions
    sample_transitions     <- resamp(prob_transitions$hh_position_target, sum(flag_subpop), prob=prob_transitions$rate, replace=T)
    
    # update pop_data
    subset_hh_trans$transition[flag_subpop] <- sample_transitions
    subset_hh_trans$transition[flag_subpop & subset_hh_trans$transition=='multi_gen_union']                            <- 'multi_gen_single'
    subset_hh_trans$transition[flag_subpop & subset_hh_trans$transition %in% c('union','new_union','single_parent')]   <- 'single'    
    subset_hh_trans$transition[flag_subpop & subset_hh_trans$transition %in% c('union_collective')]                    <- 'collective'     
  }
  
  #############
  
  event_female_match$transition[event_female_match$ID %in% subset_hh_trans$ID] <- subset_hh_trans$transition
  event_hh                                                                     <- rbind.fill(event_hh, event_female_match)

  # Assign children to parent
  flag_child                                                         <- pop[pop$hh_position=='child' & pop$main_parent %in% pop$ID[flag_dis],c('ID','HH_ID','main_parent','ID_mother','ID_father')]
  perc_single_mother                                                 <- 0.8
  sample_parent_sex                                                  <- sample(c('F','M'), nrow(flag_child), c(perc_single_mother, (1-perc_single_mother)), replace = T)
  flag_child$sex_par                                                 <- sample_parent_sex
  flag_child                                                         <- left_join(flag_child, pop[flag_dis, c('ID','HH_ID')], by='HH_ID')
  flag_child                                                         <- flag_child[flag_child$ID_mother == flag_child$ID.y & !is.na(flag_child$ID_mother) | flag_child$ID_father == flag_child$ID.y & !is.na(flag_child$ID_father),]
  flag_child$ID_mother[!flag_child$ID_mother %in% flag_child$ID.y]   <- NA
  flag_child$ID_father[!flag_child$ID_father %in% flag_child$ID.y]   <- NA
  flag_child$new_main_parent                                         <- NA
  flag_child$new_main_parent[is.na(flag_child$ID_mother) | is.na(flag_child$ID_father)]   <- flag_child$main_parent[is.na(flag_child$ID_mother) | is.na(flag_child$ID_father)]
  flag_child$new_main_parent[flag_child$sex_par=='F' & is.na(flag_child$new_main_parent)] <- flag_child$ID_mother[flag_child$sex_par=='F' & is.na(flag_child$new_main_parent)]
  flag_child$new_main_parent[flag_child$sex_par=='M' & is.na(flag_child$new_main_parent)] <- flag_child$ID_father[flag_child$sex_par=='M' & is.na(flag_child$new_main_parent)]
  flag_child                                                                              <- flag_child[flag_child$ID.y==flag_child$new_main_parent,]
  pop$main_parent[pop$ID %in% flag_child$ID.x]                                            <- flag_child$new_main_parent
  
  # change info.
  male_single_parent                                                 <- flag_dis & pop$ID %in% flag_child$new_main_parent & pop$sex=='F' # has to be here because an ordering later on will mess up flag_dis
  female_single_parent                                               <- flag_dis & pop$ID %in% flag_child$new_main_parent & pop$sex=='M'
  pop$hh_position[flag_dis & !pop$ID %in% flag_child$new_main_parent] <- 'single'              # for those not moving to single HH, this will be changed when they are assigned another HH type                                    
  pop$union[flag_dis]                                                <- 0                                                    # not in union
  pop$age_partner[flag_dis]                                          <- NA  
  pop$ID_partner[flag_dis]                                           <- NA  

  # assign main parental household
  pop$hh_position[male_single_parent]                               <- 'single_parent'
  pop$hh_position[female_single_parent]                             <- 'single_parent'
  pop$father[flag_dis & !male_single_parent]                        <- 0
  pop$mother[flag_dis & !female_single_parent]                      <- 0
  pop$father[flag_dis & male_single_parent]                         <- 1
  pop$mother[flag_dis & female_single_parent]                       <- 1
  
  # Only change HH ID at this point
  pop$HH_ID[pop$ID %in% flag_pop_id]                                 <- (max(pop$HH_ID)+1):(max(pop$HH_ID)+sum(flag_pop))    # new HH ID
  
  # set transition to 0 in order to not include these individuals later on
  pop$transition[pop$ID %in% flag_pop_id & (pop$transition %in% c('single','single_parent')| (pop$ID %in% flag_child$new_main_parent & pop$transition %in% c('single','single_parent','non_family','other')))]             <- 'updated'
  pop$transition[pop$ID %in% event_female_match$ID]                                                                                                                                                                        <- event_female_match$transition
  pop$transition[pop$ID %in% event_female_match$ID & (pop$transition %in% c('single','single_parent') |  (pop$ID %in% flag_child$new_main_parent & pop$transition %in% c('single','single_parent','non_family','other')))] <- 'updated'
  
  return(list(pop, event_hh))
  
}



# function to create unions

create_unions <- function(pop, event_hh){
  
  # select individuals without a partner who are eligible for entering into a union
  pop_single_flag    <- pop$union==0 & pop$age >= union_age_lim & !(pop$hh_position %in% c('union','multi_gen_union','union_w_child')) & !(pop$ID %in% fertility_events$ID) & !(pop$ID %in% birth_events)               
  pop_single_columns <- c("ID","HH_ID","sex","age","age_group",'age_group_trans',"transition","hh_position","ID_partner","union", "event_date")
  pop                <- setDF(pop)
  pop_single         <- pop[pop_single_flag,pop_single_columns]
  pop_single$row_id  <- 1:nrow(pop_single)
  pop_single$age_partner                       <- NA
  
  # select males entering union
  flag_male_singles  <- (pop_single$transition %in% c('union','new_union') ) & (pop_single$sex=='F')            
  pop_single_males   <- pop_single[flag_male_singles,]

  
  ## HH trans rate for potential male partners -> adjust age group
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
    partner_pop           <- pop_single$sex == 'M' & pop_single$union==0 & pop_single$transition=='no_trans' & pop_single$age_group_trans == age_partner_i    # use only females with 'no_trans' otherwise there is a risk of females going from one union to another but without corresponding ordering of dates of event
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
        event_female_match                           <- pop_single[female_partner_row_id, c('ID','HH_ID','hh_position', 'ID_partner','transition','event_date','sex','age')]
        event_female_match$event_date                <- as.Date(event_female_match$event_date, origin = '1970-01-01')
        event_female_match$transition                <- 'union'
        event_hh                                     <- rbind.fill(event_hh, event_female_match)
    
    pop_single$HH_ID[female_partner_row_id]      <- pop_single$HH_ID[male_partner_row_id]
    
    # update household details
    partners_row_id                              <- c(male_partner_row_id,female_partner_row_id)
    pop_single$hh_position[partners_row_id]      <- 'union'
    pop_single$union[partners_row_id]            <- 1
    pop_single$transition[partners_row_id]       <- 'updated'
    
    # update max household ID
    max_hh_id                                    <- max_hh_id+length(female_partner_row_id)
    
  } # end loop female age group


  pop_single$event_date[pop_single$transition=='no_trans'] <- NA
  
  # copy single pop data back to 'pop' now with updated union information
  pop[pop_single_flag,pop_single_columns] <- pop_single[,pop_single_columns]     
  
  # record partner match
  record_match                                   <- pop_single[flag_male_singles,c('age','age_partner')]
  
  return(list(pop, event_hh, record_match))
}




### Move multi_gen_union to union

move_from_multi_gen_union <- function(pop, event_hh){
  
  flag                  <- pop$transition=='union' & pop$hh_position=='multi_gen_union' & !is.na(pop$ID_partner)
  
  if(sum(flag)>0){
  
  flag_pop              <- pop[flag,c('ID','ID_partner', 'event_date')]
  flag_pop$HH_ID        <- (max(pop$HH_ID)+1):(max(pop$HH_ID)+nrow(flag_pop)) 
  flag_pop              <- flag_pop[order(flag_pop$ID_partner),]
  pop                   <- pop[order(pop$ID),]
  pop[pop$ID %in% flag_pop$ID_partner, c('event_date')]  <- flag_pop[,c('event_date')]
  
  event                 <- pop[pop$ID %in% flag_pop$ID_partner,c('ID','HH_ID','hh_position','ID_partner','event_date','sex','age')]
  #event$hh_position.y  <- 'union'
  event_hh              <- rbind.fill(event_hh, event)
  flag_all              <- pop$ID %in% c(flag_pop$ID, flag_pop$ID_partner)
  pop$hh_position[flag_all]                     <- 'union'
  pop$union[flag_all]                           <- 1                                                    
  pop$HH_ID[pop$ID %in% flag_pop$ID_partner]    <- flag_pop$HH_ID
  flag_pop                                      <- flag_pop[order(flag_pop$ID),]
  pop$HH_ID[pop$ID %in% flag_pop$ID]            <- flag_pop$HH_ID
  pop$transition[flag_all]                      <- 'updated'

  }
  
  return(list(pop, event_hh))
  
}


### Function for transition back to parents' household 

move_back_to_parents <- function(pop){
  
flag_child                          <- pop$transition == 'child' & ((!is.na(pop$ID_father) & pop$ID_father %in% pop$ID) | (!is.na(pop$ID_mother) & pop$ID_mother %in% pop$ID) | (!is.na(pop$main_parent) & pop$main_parent %in% pop$ID) )
parents_hh                          <- pop[flag_child, c('ID','mother','father','ID_mother','ID_father','main_parent')]
parents_hh                          <- left_join(parents_hh, pop[,c('ID','HH_ID','hh_position','transition')], by=c('ID_mother'='ID'), na_matches="never")
parents_hh                          <- left_join(parents_hh, pop[,c('ID','HH_ID','hh_position','transition')], by=c('ID_father'='ID'), na_matches="never")
parents_hh                          <- left_join(parents_hh, pop[,c('ID','HH_ID','hh_position','transition')], by=c('main_parent'='ID'), na_matches="never")
parents_hh$HH_ID[parents_hh$hh_position %in% c('collective') | is.na(parents_hh$hh_position) | parents_hh$transition %in% c('collective') | is.na(parents_hh$transition)]            <- NA
parents_hh$HH_ID.x[parents_hh$hh_position.x %in% c('collective') | is.na(parents_hh$hh_position.x) |  parents_hh$transition.x %in% c('collective') | is.na(parents_hh$transition.x)] <- NA
parents_hh$HH_ID.y[parents_hh$hh_position.y %in% c('collective') | is.na(parents_hh$hh_position.y) |  parents_hh$transition.y %in% c('collective') | is.na(parents_hh$transition.y)] <- NA
parents_hh$new_hh                   <- ifelse(!is.na(parents_hh$HH_ID), parents_hh$HH_ID, ifelse(is.na(parents_hh$HH_ID.x), parents_hh$HH_ID.y, parents_hh$HH_ID.x))
parents_hh$new_main_parent          <- ifelse(!is.na(parents_hh$HH_ID), parents_hh$main_parent, ifelse(is.na(parents_hh$HH_ID.x), parents_hh$ID_father, parents_hh$ID_mother))
parents_hh$par_hh_pos               <- ifelse(!is.na(parents_hh$hh_position), parents_hh$hh_position, ifelse(is.na(parents_hh$hh_position.x), parents_hh$hh_position.y, parents_hh$hh_position.x))
#OBS! reset transition of parent if parents also is moving to parental hh
parents_hh$new_hh[parents_hh$ID %in% parents_hh$new_main_parent] <- NA
flag_child_trans                    <- pop$ID %in% parents_hh$ID[!is.na(parents_hh$new_hh) & parents_hh$mother==0 & parents_hh$father==0]
flag_child_trans_multi              <- pop$ID %in% parents_hh$ID[!is.na(parents_hh$new_hh) & ((parents_hh$mother==1 | parents_hh$father==1) | parents_hh$par_hh_pos=='child')]
flag_child_no_trans                 <- pop$transition == 'child' & !flag_child_trans & !flag_child_trans_multi
pop$HH_ID[flag_child_trans]         <- parents_hh$new_hh[!is.na(parents_hh$new_hh) & parents_hh$mother==0 & parents_hh$father==0]
pop$main_parent[flag_child_trans]   <- parents_hh$new_main_parent[!is.na(parents_hh$new_hh) & parents_hh$mother==0 & parents_hh$father==0]
pop$hh_position[flag_child_trans]   <- 'child' 
pop$transition[flag_child_trans]    <- 'updated'
pop$HH_ID[flag_child_trans_multi]         <- parents_hh$new_hh[!is.na(parents_hh$new_hh) & ((parents_hh$mother==1 | parents_hh$father==1) | parents_hh$par_hh_pos=='child')]
pop$hh_position[flag_child_trans_multi]   <- 'single_parent'
pop$transition[flag_child_trans_multi]    <- 'updated'
pop$transition[flag_child_no_trans]       <- 'no_trans'
pop$event_date[flag_child_no_trans]       <- NA

# Set parents transition to 'no_trans' to avoid "double" transitions
pop$transition[(pop$ID %in% parents_hh$new_main_parent[!is.na(parents_hh$new_hh)] | pop$ID_partner %in% parents_hh$new_main_parent[!is.na(parents_hh$new_hh)]) & pop$transition!='updated']  <- 'no_trans'

return(pop)

}


### Function for transition to household of child's family node

multi_generational_hh <- function(pop, event_hh, fam_nuc){

#union
flag_parent_par                     <- (pop$transition == 'multi_gen_union' & (pop$hh_position %in% c('union','union_w_child')) & pop$ID %in% c(pop$ID_mother, pop$ID_father))
child_hh                            <- data.frame(pop[flag_parent_par, c('ID','ID_partner', 'event_date')])
names(child_hh)                     <- c('ID_parent','ID_parent2', 'event_date')
child_hh                            <- left_join(child_hh, pop[,c('ID','ID_mother','hh_position','HH_ID')], by=c('ID_parent'='ID_mother'), na_matches="never")
child_fam                           <- child_hh[child_hh$hh_position %in% c('single_parent','union','union_w_child') & child_hh$ID %in% fam_nuc$ID,]
child_fam                           <- child_fam[!duplicated(child_fam$ID_parent),]
child_fam                           <- child_fam[order(child_fam$ID_parent2),]

pop                                              <- pop[order(pop$ID),]
pop$event_date[pop$ID %in% child_fam$ID_parent2] <- child_fam$event_date
event                                            <- pop[pop$ID %in% child_fam$ID_parent2, c('ID','HH_ID','hh_position','ID_partner','event_date','sex','age')]
event_hh                                         <- rbind.fill(event_hh, event)

child_fam                           <- cbind.data.frame(stack(child_fam[,c('ID_parent','ID_parent2')]), child_fam[,'HH_ID'])
names(child_fam)                    <- c('ID','ind','HH_ID')
child_fam                           <- child_fam[order(child_fam$ID),]
flag_parent_trans1                  <- pop$ID %in% child_fam$ID
pop$HH_ID[flag_parent_trans1]       <- child_fam$HH_ID
pop$hh_position[flag_parent_trans1] <- 'multi_gen_union'
pop$transition[flag_parent_trans1]  <- 'updated'

#single
flag_parent_par                     <- (pop$transition == 'multi_gen_single' & !(pop$hh_position %in% c('union','union_w_child')) & pop$ID %in% c(pop$ID_mother, pop$ID_father))
child_hh                            <- data.frame(pop[flag_parent_par, c('ID')])
names(child_hh)                     <- 'ID_parent'
child_hh                            <- left_join(child_hh, pop[,c('ID','ID_mother','hh_position','HH_ID')], by=c('ID_parent'='ID_mother'), na_matches="never")
child_hh                            <- left_join(child_hh, pop[,c('ID','ID_father','hh_position','HH_ID')], by=c('ID_parent'='ID_father'), na_matches="never")
child_hh$hh_position                <- ifelse(is.na(child_hh$hh_position.x), child_hh$hh_position.y, child_hh$hh_position.x)
child_hh$new_hh                     <- ifelse(is.na(child_hh$HH_ID.x), child_hh$HH_ID.y, child_hh$HH_ID.x)
child_hh$ID                         <- ifelse(is.na(child_hh$ID.x), child_hh$ID.y, child_hh$ID.x)
child_fam                           <- child_hh[child_hh$hh_position %in% c('single_parent','union','union_w_child') & child_hh$ID %in% fam_nuc$ID,]
child_fam                           <- child_fam[!duplicated(child_fam$ID_parent),]
child_fam                           <- child_fam[order(child_fam$ID_parent),]
flag_parent_trans2                  <- pop$ID %in% child_fam$ID_parent
pop$HH_ID[flag_parent_trans2]       <- child_fam$new_hh
pop$hh_position[flag_parent_trans2] <- 'multi_gen_single'
pop$transition[flag_parent_trans2]  <- 'updated'

flag_no_trans                       <- pop$transition %in% c('multi_gen_single','multi_gen_union') & !flag_parent_trans1 & !flag_parent_trans2
pop$transition[flag_no_trans]       <- 'no_trans'
pop$event_date[flag_no_trans]       <- NA   

return(list(pop, event_hh))

}



# Function for transition to new household

update_new_HH <- function(pop, event_hh, median_coll_hh){
  
  flag_nfhh                       <- pop$transition == 'non_family' & pop$age>15 & pop$hh_position!='single_parent'
  nfhh                            <- unique(pop[pop$hh_position %in% c('union','single_parent','union_w_child'),c('HH_ID','HH_children','HH_size')]) 
  nfhh_HH_children                <-  resamp(non_family_hh_size$Var1, sum(flag_nfhh), prob=non_family_hh_size$Freq, replace = TRUE)
  nfhh_0                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==2], sum(nfhh_HH_children==2), replace = FALSE)
  nfhh_1                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==3], sum(nfhh_HH_children==3), replace = FALSE)
  nfhh_2                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==4], sum(nfhh_HH_children==4), replace = FALSE)
  nfhh_3                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==5], sum(nfhh_HH_children==5), replace = FALSE)
  nfhh_4                          <-  resamp(nfhh$HH_ID[nfhh$HH_size>5], sum(nfhh_HH_children==6), replace = FALSE)
  nfhh_ID                         <- c(nfhh_0, nfhh_1, nfhh_2, nfhh_3, nfhh_4)
  pop$HH_ID[flag_nfhh]            <- nfhh_ID
  pop$hh_position[flag_nfhh]      <- 'non_family'
  pop$transition[flag_nfhh]       <- 'updated'
  pop$ID_partner[flag_nfhh]       <- NA
  pop$union[flag_nfhh]            <- 0
  
  ## Transition to 'Other' HH
  flag_other                      <- (pop$hh_position %in% c('child','single','non_family','collective')) & pop$transition == 'other' & !is.na(pop$transition) & ((pop$mother!=1 & pop$father!=1) | pop$lipro_child_hh==0) & pop$age>15  #ind. with children do not move to 'other' households
  
  if (sum(flag_other)>0){
    
  other_cand                      <- pop[flag_other,]
  other_cand$new_HH_ID            <- NA
  new_hh2                         <- 0.8
  current_hh                      <- 0.2
  df_other                        <- pop[pop$hh_position=='other',]
  df_other                        <- df_other[!df_other$HH_ID %in% unique(pop$HH_ID[pop$hh_position!='other']),]
  mean_age                        <- ddply(df_other, .(HH_ID), summarize,  mean_age=mean(age))

  # New 'other' households size 2
  cand_hh2                        <- resamp(other_cand$ID, new_hh2*nrow(other_cand)/2, replace=FALSE)
  
  max_hh_id                       <- max(pop$HH_ID)
  
  for (i in 1:length(cand_hh2)){
    
    age_i                         <- other_cand$age[other_cand$ID==cand_hh2[i]]
    candidates                    <- other_cand[!other_cand$ID %in% cand_hh2 & is.na(other_cand$new_HH_ID),]
    candidates$diff               <- abs(candidates$age-age_i)
    match_min                     <- candidates$ID[which.min(candidates$diff)]
    candidates                    <- candidates[candidates$ID!=match_min,]
    other_cand$new_HH_ID[other_cand$ID %in% c(match_min, cand_hh2[i])] <- max_hh_id+i
    
  }  
  
  # Existing 'other' households
  cand_hh                         <- other_cand$ID[is.na(other_cand$new_HH_ID)]
  
  for (i in 1:length(cand_hh)){
    
    age_i                         <- other_cand$age[other_cand$ID==cand_hh[i]]
    mean_age                      <- mean_age[!mean_age$HH_ID %in% other_cand$new_HH_ID,]
    mean_age$diff                 <- abs(mean_age$mean_age-age_i)
    match_min                     <- mean_age$HH_ID[which.min(mean_age$diff)]
    other_cand$new_HH_ID[other_cand$ID %in% cand_hh[i]] <- match_min
    
  }  
  
  
  pop$hh_position[flag_other]     <- 'other'
  pop$transition[flag_other]      <- 'updated'
  pop$ID_partner[flag_other]      <- NA
  pop$union[flag_other]           <- 0
  pop$HH_ID[flag_other]           <- other_cand$new_HH_ID
  
  
  }
  
  
  ## Transition to single HH
  flag_single                     <- (pop$transition %in% c('single')) & (pop$age>14) & (pop$union==0)
  single_ID                       <- (max(pop$HH_ID)+1):(max(pop$HH_ID)+sum(flag_single))    #New HH ID
  pop$HH_ID[flag_single]          <- single_ID
  pop$hh_position[flag_single]    <- 'single'
  pop$transition[flag_single]     <- 'updated'
  pop$ID_partner[flag_single]     <- NA
  pop$union[flag_single]          <- 0
  
  
  
  #### Transition to collective HH 
  flag_coll                      <- pop[pop$transition %in% c('collective','union_collective'), c('ID','age')]
  ID_union_coll                  <- pop[pop$transition=='union_collective', c('ID','ID_partner','event_date')] # union moving to same collective HH -> update at end
  
  # Capacity in collective households
  hh_capacity                    <- pop[pop$hh_position=='collective', c('HH_ID','HH_size','age')]
  median_age                     <- ddply(hh_capacity, .(HH_ID), summarize, median_age=median(age))
  hh_capacity                    <- hh_capacity[!duplicated(hh_capacity$HH_ID), c('HH_ID','HH_size')]
  hh_capacity                    <- merge(hh_capacity, median_coll_hh[,c('HH_ID','capacity')], by='HH_ID', all.x=FALSE, all.y=FALSE)
  hh_capacity$available          <- hh_capacity$capacity-hh_capacity$HH_size
  hh_capacity                    <- hh_capacity[hh_capacity$available>0,] 
  hh_capacity$hh_group           <- findInterval(hh_capacity$capacity, c(0,10,50,100))
  hh_capacity                    <- merge(hh_capacity, median_age, by='HH_ID', all.x = TRUE, all.y = FALSE)
  hh_capacity$min                <- hh_capacity$median_age-15
  hh_capacity$max                <- hh_capacity$median_age+15
  
  # Draw HH-size for ind.
  flag1                          <- flag_coll$age %in% c(0:29)
  flag2                          <- flag_coll$age %in% c(30:59)
  flag3                          <- flag_coll$age >= 60
  age1                           <- resamp(collective_age_size$hh_size[collective_age_size$age_group==1], sum(flag1), prob=collective_age_size$prop[collective_age_size$age_group==1], replace=TRUE)
  age2                           <- resamp(collective_age_size$hh_size[collective_age_size$age_group==2], sum(flag2), prob=collective_age_size$prop[collective_age_size$age_group==2], replace=TRUE)
  age3                           <- resamp(collective_age_size$hh_size[collective_age_size$age_group==3], sum(flag3), prob=collective_age_size$prop[collective_age_size$age_group==3], replace=TRUE)
  flag_coll$new_hh               <- NA
  flag_coll$new_hh[flag1]        <- age1    
  flag_coll$new_hh[flag2]        <- age2 
  flag_coll$new_hh[flag3]        <- age3 
  
  
  # Match
  flag_coll$new_HH_ID            <- NA
  missing_match                  <- c()
  
  for (i in 1:nrow(flag_coll)){
    
    size_i                       <- flag_coll$new_hh[i]
    age_i                        <- flag_coll$age[i]
    available_HHs                <- hh_capacity[hh_capacity$available > 0 & hh_capacity$hh_group==size_i & hh_capacity$min < age_i & hh_capacity$max > age_i,'HH_ID']
    
    if (length(available_HHs) > 0){
      
      draw                         <- resamp(available_HHs,1)
      flag_coll$new_HH_ID[i]       <- draw
      hh_capacity$available[hh_capacity$HH_ID==draw] <- hh_capacity$available[hh_capacity$HH_ID==draw]-1
      
    } else {
      
      missing_match <- c(missing_match, flag_coll$ID[i])
      
    }
    
  }
  
  # Create new collective household: 3 age groups for hh-size <25 and >=100, 2 age groups for the rest because these have very few ind: younger than 70
  missing_match      <- flag_coll[flag_coll$ID %in% missing_match,]
  flag_coll$capacity <- NA
  
  for (i in c(1:5)){ 
    
    
    df_i_1 <- missing_match[missing_match$new_hh==i & missing_match$age < 30,]
    df_i_2 <- missing_match[missing_match$new_hh==i & missing_match$age %in% c(30:59),]
    df_i_3 <- missing_match[missing_match$new_hh==i & missing_match$age >= 60,]
    
    
    while (nrow(df_i_1) + nrow(df_i_2) + nrow(df_i_3) > 0){
      
      if (i==1){
        
        size   <- sample(c(2:4), 3, replace=TRUE)
        
      } else if (i==2){
        
        size   <- sample(c(5:24), 3, replace=TRUE)
        
        #} else if (i==3){
        
        #  size   <- sample(c(10:24), 3, replace=TRUE)          
        
      } else if (i==3){
        
        size   <- sample(c(25:49), 3, replace=TRUE)   
        
      } else if (i==4){
        
        size   <- sample(c(50:99), 3, replace=TRUE)   
        
      } else if (i==5){
        
        size   <- sample(c(100:400), 3, replace=TRUE)   
        
        #} else if (i==7){
        
        #  size   <- sample(100:400, 3, replace=TRUE)
        
      }
      
      if (nrow(df_i_1) >= size[1]){ # youngest age group -> df_i_1
        
        hh_1   <- resamp(df_i_1$ID, size[1], replace=F)
        
      } else {
        
        hh_1   <- resamp(df_i_1$ID, nrow(df_i_1), replace=F)
        
      }
      
      if (length(hh_1)>0){
        
        max_id                                      <- max(c(pop$HH_ID, flag_coll$new_HH_ID), na.rm=T)
        created_ID                                  <- rep((max_id+1), length(hh_1))
        flag_coll$new_HH_ID[flag_coll$ID %in% hh_1] <- created_ID
        flag_coll$capacity[flag_coll$ID %in% hh_1]  <- size[1]+ceiling(0.15*size[1])      
        df_i_1                                      <- df_i_1[!df_i_1$ID %in% hh_1,]
        
      }
      
      if (nrow(df_i_2) >= size[2]){ # middle age group -> df_i_2
        
        hh_2   <- resamp(df_i_2$ID, size[2], replace=F)
        
      } else {
        
        hh_2   <- resamp(df_i_2$ID, nrow(df_i_2), replace=F)
        
      }
      
      if (length(hh_2)>0){
        
        max_id                                      <- max(c(pop$HH_ID, flag_coll$new_HH_ID), na.rm=T)
        created_ID                                  <- rep((max_id+1), length(hh_2))
        flag_coll$new_HH_ID[flag_coll$ID %in% hh_2] <- created_ID
        flag_coll$capacity[flag_coll$ID %in% hh_2]  <- size[2]+ceiling(0.15*size[2])  
        df_i_2                                      <- df_i_2[!df_i_2$ID %in% hh_2,]
        
      }
      
      if (nrow(df_i_3) >= size[3]){ # oldest age group -> df_i_3
        
        hh_3   <- resamp(df_i_3$ID, size[3], replace=F)
        
      } else {
        
        hh_3   <- resamp(df_i_3$ID, nrow(df_i_3), replace=F)
        
      }
      
      if (length(hh_3)>0){
        
        max_id                                      <- max(c(pop$HH_ID, flag_coll$new_HH_ID), na.rm=T)
        created_ID                                  <- rep((max_id+1), length(hh_3))
        flag_coll$new_HH_ID[flag_coll$ID %in% hh_3] <- created_ID
        flag_coll$capacity[flag_coll$ID %in% hh_3]  <- size[3]+ceiling(0.15*size[3])   
        df_i_3                                      <- df_i_3[!df_i_3$ID %in% hh_3,]
        
      }
      
    }#end while loop
    
  } # end for loop
  
  # Add newly created HHs to median_coll_hh
  newly_created           <- flag_coll[flag_coll$new_HH_ID > max(pop$HH_ID),]
  median_new              <- ddply(newly_created, .(new_HH_ID), summarize, median_age=median(age))
  median_new              <- merge(median_new, newly_created[,c('new_HH_ID','capacity')], all.x = TRUE, all.y = FALSE)
  median_new              <- median_new[!duplicated(median_new$new_HH_ID),]
  names(median_new)       <- c('HH_ID','median_age','capacity')
  median_coll_hh          <- rbind.fill(median_coll_hh, median_new)
  
  ID_union_coll                   <- merge(ID_union_coll, flag_coll[,c('ID','new_HH_ID')], by='ID')
  ID_union_coll                   <- ID_union_coll[,-1]
  names(ID_union_coll)            <- c('ID','event_date','new_HH_ID')
  flag_coll                       <- rbind.fill(flag_coll, ID_union_coll)
  pop                             <- pop[order(pop$ID),]
  flag_coll                       <- flag_coll[order(flag_coll$ID),]
  ID_union_coll                   <- ID_union_coll[order(ID_union_coll$ID),]
  flag                            <- pop$ID %in% flag_coll$ID
  flag_partner                    <- pop$ID %in% ID_union_coll$ID
  pop$HH_ID[flag]                 <- flag_coll$new_HH_ID
  pop$hh_position[flag]           <- 'collective'
  pop$ID_partner[flag & pop$transition=='collective'] <- NA
  pop$union[flag & pop$transition=='collective']      <- 0
  pop$transition[flag]            <- 'updated'
  pop$event_date[flag_partner]    <- ID_union_coll$event_date
  event                           <- pop[flag_partner, c('ID','HH_ID','hh_position','ID_partner','event_date','sex','age')]
  event_hh                        <- rbind.fill(event_hh, event)
  
  return(list(pop, event_hh, median_coll_hh))
  
}



# Function to update children's household-ID (mother's household is default)

update_child_staying <- function(pop, event_hh){
  
  # children following main parent's household -> update event_hh
  mother_hh_trans  <- pop[(pop$main_parent %in% event_hh$ID) & (pop$hh_position=='child') & !(pop$ID %in% event_hh$ID[event_hh$hh_position_target!='child' & !is.na(event_hh$hh_position_target)]), c('ID', 'HH_ID', 'main_parent','hh_position','ID_partner')]
  mother_hh_trans  <- mother_hh_trans[order(mother_hh_trans$main_parent),]
  event_hh         <- event_hh[order(event_hh$ID),]                       # order by ID to find correct parent
  mother_info      <- event_hh[event_hh$ID %in% mother_hh_trans$main_parent, c('HH_ID.y','ID','event_date')]
  mother_hh_trans  <- left_join(mother_hh_trans, mother_info, by=c('main_parent'='ID'))
  mother_hh_trans  <- mother_hh_trans[!(mother_hh_trans$HH_ID == mother_hh_trans$HH_ID.y),]
  mother_hh_trans  <- mother_hh_trans[,-3]
  colnames(mother_hh_trans)[2] <- 'HH_ID.x'
  colnames(mother_hh_trans)[3] <- 'hh_position.x'
  colnames(mother_hh_trans)[4] <- 'ID_partner.x'
  mother_hh_trans$hh_position.y <- mother_hh_trans$hh_position.x
  mother_hh_trans$ID_partner.y  <- mother_hh_trans$ID_partner.x
  event_hh                      <- rbind.fill(event_hh, mother_hh_trans)
  pop                           <- pop[order(pop$ID),]
  mother_hh_trans               <- mother_hh_trans[order(mother_hh_trans$ID),]
  pop$HH_ID[pop$ID %in% mother_hh_trans$ID] <- mother_hh_trans$HH_ID.y
  pop$transition[pop$ID %in% mother_hh_trans$ID] <- 'updated'

          # If younger than 16 and no parents in population -> adoption
          flag_adopted               <-  pop$hh_position=='child' & pop$age < child_age_lim & (!(pop$ID_father %in% pop$ID) | is.na(pop$ID_father)) & (!(pop$ID_mother %in% pop$ID) | is.na(pop$ID_mother)) & (!(pop$main_parent %in% pop$ID) | is.na(pop$main_parent)) 
          
          if (sum(flag_adopted)>0){
          
          adopted                    <- as.data.frame(pop[flag_adopted,])
          adoptive_parents           <- resamp(pop$ID[pop$union==1 & pop$age<50 & pop$sex=='F'], sum(flag_adopted), replace=F)  # Children are adopted by females in a union who are younger than 50 years
          adoptive_parents           <- adoptive_parents[order(adoptive_parents)]
          partner_adoptive_mother    <- pop$ID_partner[pop$ID %in% adoptive_parents]
          adopted$mother             <- adoptive_parents
          adopted$father             <- partner_adoptive_mother
          adopted$hh                 <- pop$HH_ID[pop$ID %in% adopted$mother]
          flag_adopted_pop           <- pop$ID %in% adopted$ID
          pop[flag_adopted_pop, c('HH_ID','ID_mother', 'ID_father')] <- adopted[,c('hh','mother','father')]
          
          adoption_event             <- pop[flag_adopted_pop, c('ID','HH_ID')]
          names(adoption_event)      <- c('ID','HH_ID.x')
          adoption_event$HH_ID.y     <- adoption_event$HH_ID.x
          #adoption_event$event_type  <- 'adoption'
          event_hh                   <- rbind.fill(event_hh, adoption_event)
          
          pop$transition[flag_adopted_pop] <- 'updated'
          
          
          }
          

          
  return(list(pop, event_hh))
}




### In case of union-dissolution of child to multi-gen

update_multi_gen_staying <- function(pop, event_hh){
  
  # multi-gen follow child
  child_union_dis  <- event_hh[(event_hh$hh_position %in% c('union','union_w_child') & event_hh$transition %in% c('single','single_parent')) | (event_hh$hh_position %in% c('single_parent') & event_hh$transition %in% c('union','union_w_child')),c('ID','HH_ID','event_date')]
  child_union_dis  <- left_join(child_union_dis, pop[,c('ID','HH_ID','ID_mother','ID_father','main_parent')], by='ID')
  multi_gen        <- pop[pop$hh_position %in% c('multi_gen_single','multi_gen_union'),c('ID','HH_ID','transition','event_date','ID_partner','hh_position')]
  multi_gen        <- multi_gen[multi_gen$HH_ID %in% child_union_dis$HH_ID.x & multi_gen$transition %in% c('no_trans'),]
  multi_gen        <- left_join(multi_gen, child_union_dis, by=c('HH_ID'='HH_ID.x'))
  multi_gen2       <- multi_gen[(multi_gen$ID.x==multi_gen$ID_mother & !is.na(multi_gen$ID_mother)) | (multi_gen$ID.x==multi_gen$ID_father & !is.na(multi_gen$ID_father)) | (multi_gen$ID.x==multi_gen$main_parent & !is.na(multi_gen$main_parent)),]
  multi_gen2       <- multi_gen2[multi_gen2$HH_ID!=multi_gen2$HH_ID.y,]
  multi_gen2       <- multi_gen2[!duplicated(multi_gen2$ID.x),]
  multi_gen_partner<- multi_gen[multi_gen$ID_partner %in% multi_gen2$ID.x & !multi_gen$ID.x %in% multi_gen2$ID.x,]
  multi_gen_partner <- inner_join(multi_gen_partner[c('ID.x','ID_partner','ID.y')], multi_gen2, by=c('ID_partner'='ID.x', 'ID.y'))
  multi_gen2       <- rbind.fill(multi_gen2, multi_gen_partner)
  pop              <- pop[order(pop$ID),]
  multi_gen2       <- multi_gen2[order(multi_gen2$ID.x),]
  pop[pop$ID %in% multi_gen2$ID.x, c('HH_ID','event_date')] <- multi_gen2[,c('HH_ID.y','event_date.y')]
  pop$transition[pop$ID %in% multi_gen2$ID.x] <- 'updated'
  event_multi_gen  <- multi_gen2[,c('ID.x','HH_ID','event_date.y','hh_position')]
  names(event_multi_gen) <- c('ID','HH_ID','event_date','hh_position')
  event_multi_gen  <- left_join(event_multi_gen, pop[,c('sex','age','ID')], by='ID')  
  event_hh         <- rbind.fill(event_hh, event_multi_gen)
  
  return(list(pop, event_hh))
}

# Function to update household information

update_HH_comp <- function(pop){
  
  # HH size
  pop                   <- pop[ , -which(names(pop) %in% c('HH_size','HH_children'))]
  HHS                   <- pop[ , c('ID','HH_ID')]
  HH_sizes              <- setDT(HHS)[, .N, by = HH_ID]
  colnames(HH_sizes)[2] <- 'HH_size'
  pop                   <- merge(pop, HH_sizes, by='HH_ID')

  # Number of children (<18 years) in household
  pop$child             <- as.numeric(pop$age<18)
  setDT(pop)[, HH_children := sum(child), by = .(HH_ID)]     
  
  # Count Lipro-children in household
  pop                <- setDT(pop)
  pop                <- pop[, !"lipro_child_hh"] 
  lipro_child        <- as.data.table(pop)[hh_position == 'child', .N, by = HH_ID]
  names(lipro_child) <- c('HH_ID','lipro_child_hh')
  pop                <- merge(pop, lipro_child, by='HH_ID', all.x = TRUE)
  pop                <- setDF(pop)
  pop$lipro_child_hh[is.na(pop$lipro_child_hh)] <- 0
  
  
  return(pop)
}


check_multi_gen_hhs <- function(pop){
  
  
  children <- pop[( !is.na(pop$ID_father) | !is.na(pop$ID_mother) | !is.na(pop$main_parent) ) & pop$hh_position %in% c('union','single_parent','multi_gen_union','multi_gen_single','union_w_child'), c('ID', 'HH_ID','hh_position','ID_mother','ID_father')]
  parents  <- pop[pop$ID %in% c(children$ID_father, children$ID_mother), c('ID','HH_ID','hh_position')]
  children <- left_join(children, parents, by=c('HH_ID'), na_matches="never")
  children <- children[children$ID_mother==children$ID.y & !is.na(children$ID_mother) & !is.na(children$ID.y) | children$ID_father==children$ID.y & !is.na(children$ID_father) & !is.na(children$ID.y),]
  pop$hh_position[pop$ID %in% children$ID.y & pop$hh_position %in% c('union','union_w_child')]         <- 'multi_gen_union'
  pop$hh_position[pop$ID %in% children$ID.y & pop$hh_position!='multi_gen_union'] <- 'multi_gen_single'
  
  wrong_multi <- pop$hh_position %in% c('multi_gen_union', 'multi_gen_single') & !(pop$ID %in% children$ID.y)
  pop$hh_position[wrong_multi & !is.na(pop$ID_partner)] <- 'union'
  pop$hh_position[wrong_multi & is.na(pop$ID_partner)]  <- 'non_family'
  
  partner <- pop$ID[pop$hh_position=='multi_gen_union']
  pop$hh_position[pop$ID_partner %in% partner & !is.na(pop$ID_partner)] <- 'multi_gen_union'
  
  return(pop)
  
}


parent_indicator <- function(pop){
  
  child_match                    <- merge(pop[,c('ID','HH_ID','ID_mother','ID_father')], pop[,c('ID','HH_ID')], by.x = 'ID_mother', by.y = 'ID', all.x=T,incomparables=NA)
  child_match                    <- merge(child_match, pop[,c('ID','HH_ID')], by.x = 'ID_father', by.y = 'ID', all.x=T,incomparables=NA)
  child_match$hh_match_mother_11 <- as.numeric(child_match$HH_ID.x==child_match$HH_ID.y)
  child_match$hh_match_mother_11[is.na(child_match$hh_match_mother_11)] <- 0
  child_match$hh_match_father_11 <- as.numeric(child_match$HH_ID.x==child_match$HH_ID)
  child_match$hh_match_father_11[is.na(child_match$hh_match_father_11)] <- 0
  pop$mother                     <- 0
  pop$father                     <- 0
  pop$mother                     <- as.numeric(pop$ID %in% child_match$ID_mother[child_match$hh_match_mother_11==1])
  pop$father                     <- as.numeric(pop$ID %in% child_match$ID_father[child_match$hh_match_father_11==1])
  
  return(pop)
  
}


### Main function for household transition

hh_transition <- function(pop, median_coll_hh){

  # age groups
    pop$age_group         <- findInterval(pop$age, seq(0,open_age_trans, age_int))  # Last group is 75+
    pop$age_group_trans   <- findInterval(pop$age, seq(0,open_age_trans_rates, age_int))  # Last group is 85+ for better results in the elderly pop (e.g. collective HHs). For couple formation the max age of 75 is still used.
  
  # update main-parent for children
    pop$main_parent[pop$hh_position=='child' & is.na(pop$main_parent)] <- ifelse(is.na(pop$ID_mother[pop$hh_position=='child' & is.na(pop$main_parent)]), pop$ID_father[pop$hh_position=='child' & is.na(pop$main_parent)], pop$ID_mother[pop$hh_position=='child' & is.na(pop$main_parent)])
  
  # update main-parent for non-children
    pop$main_parent[(!pop$main_parent %in% pop$ID) & !is.na(pop$main_parent)] <- ifelse(!is.na(pop$ID_mother[(!pop$main_parent %in% pop$ID) & !is.na(pop$main_parent)]) & pop$ID_mother[(!pop$main_parent %in% pop$ID) & !is.na(pop$main_parent)] %in% pop$ID, pop$ID_mother[(!pop$main_parent %in% pop$ID) & !is.na(pop$main_parent)], pop$ID_father[(!pop$main_parent %in% pop$ID) & !is.na(pop$main_parent)])

  # Update parent status
    pop <- parent_indicator(pop)
  
  # Parents in pop
    pop$parent_pop <- as.numeric(pop$ID_father %in% pop$ID & !is.na(pop$ID_father) | pop$ID_mother %in% pop$ID & !is.na(pop$ID_mother) | pop$main_parent %in% pop$ID & !is.na(pop$main_parent)) | pop$hh_position=='child'
    
  # Count Lipro-children in household
    pop                <- setDT(pop)
    pop                <- pop[, !"lipro_child_hh"] 
    lipro_child        <- as.data.table(pop)[hh_position == 'child', .N, by = HH_ID]
    names(lipro_child) <- c('HH_ID','lipro_child_hh')
    pop                <- merge(pop, lipro_child, by='HH_ID', all.x = TRUE)
    pop                <- setDF(pop)
    pop$lipro_child_hh[is.na(pop$lipro_child_hh)] <- 0

  # Already existing family nuclei (needed for multi-gen to avoid double-transitions in one step)
    fam_nuc            <- pop[pop$hh_position %in% c('union','union_w_child','single_parent'), c('ID','HH_ID')]
    
  # hh position transition
    trans              <- update_transitions(pop)
    pop                <- trans[[1]]                             
    event_hh           <- trans[[2]]
    pop_dt             <- pop[,c('age','sex','hh_position','transition')]
    setDT(pop_dt)[ , count := .N, by = list(age, hh_position, sex, transition)]
    df_union           <- pop[pop$hh_position=='union_w_child' & pop$transition!='no_trans',]
    
  # union formation and dissolution
    new_dissolution    <- dissolve_unions(pop, event_hh)
    pop                <- new_dissolution[[1]]
    event_hh           <- new_dissolution[[2]]

    new_unions         <- create_unions(pop, event_hh) 
    pop                <- new_unions[[1]]
    event_hh           <- new_unions[[2]]
    record_match       <- new_unions[[3]]

  # transition to/from multi-gen and parents hh
    from_multi         <- move_from_multi_gen_union(pop, event_hh)
    pop                <- from_multi[[1]]
    event_hh           <- from_multi[[2]]
    to_multi           <- multi_generational_hh(pop, event_hh, fam_nuc) 
    pop                <- to_multi[[1]]
    event_hh           <- to_multi[[2]]

  # transition to nfhh, other, collective and single person household
    new_hhs            <- update_new_HH(pop, event_hh, median_coll_hh)
    pop                <- new_hhs[[1]]
    event_hh           <- new_hhs[[2]]
    median_coll_hh     <- new_hhs[[3]]
    

  # Child to parental hh
    pop                <- move_back_to_parents(pop)

    
  # track new HH-ID
    new_HH_ID          <- pop[pop$ID %in% event_hh$ID, c('ID', 'HH_ID', 'ID_partner','hh_position')]
    event_hh           <- left_join(event_hh, new_HH_ID, by=c('ID'))
    
    
  # update household information for children
    child_update1      <- update_child_staying(pop, event_hh)
    pop                <- child_update1[[1]]
    event_hh           <- child_update1[[2]]
    child_update2      <- update_child_staying(pop, event_hh)
    pop                <- child_update2[[1]]
    event_hh           <- child_update2[[2]] # Two times in order to catch multi-generation households

  # adjust colnames of event_hh
    event_hh           <- event_hh[,-which(names(event_hh)=='transition')]
    names(event_hh)    <- c('ID','HH_ID', 'hh_position', 'ID_partner', 'event_date','sex','age','HH_ID_target','ID_partner_target','hh_position_target')
    event_hh$event_type<- 'hh_transition'
  
  # Update household information
    pop                <- update_HH_comp(pop)

  # add date of event to pop -> too many have a date in pop (e.g. females entering/leaving union)
    pop$event_date[!(pop$ID %in% fertility_events$ID) & !(pop$ID %in% birth_events)]   <- NA
    pop                <- pop[order(pop$ID),]
    event_hh           <- event_hh[order(event_hh$ID),]
    
    # If more than one event -> make sure that event/date are ok
    event_dupl         <- event_hh[event_hh$ID %in% event_hh$ID[duplicated(event_hh$ID)],]
    print(c('event_dupl', nrow(event_dupl)))
    event_dupl         <- event_dupl[event_dupl$hh_position=='child' & event_dupl$hh_position_target=='child',c('ID','event_date','HH_ID_target')]
    event_hh           <- event_hh[!(event_hh$ID %in% event_dupl$ID & event_hh$hh_position=='child' & event_hh$hh_position_target=='child'),]
    event_hh[event_hh$ID %in% event_dupl$ID, c('event_date','HH_ID_target')] <- event_dupl[,c('event_date','HH_ID_target')]
    
    pop$event_date[pop$ID %in% event_hh$ID] <- event_hh$event_date
    pop$event_date                          <- as.Date(pop$event_date, origin='1970-01-01')


  # change false single status
    false_single <- pop$hh_position=='single' & pop$HH_size>1
    pop$hh_position[false_single & pop$HH_children==pop$HH_size-1]  <- 'single_parent'
    pop$hh_position[false_single & pop$HH_children < pop$HH_size-1] <- 'non_family'

  # change false union
    pop$union <- as.numeric(pop$hh_position %in% c('union', 'union_w_child', 'multi_gen_union'))
    #pop$union[pop$hh_position!='union'] <- 0    
    pop$ID_partner[pop$union==0]         <- NA
    pop$ID_partner[pop$HH_size==1]       <- NA
    pop$hh_position[pop$HH_size==1]      <- 'single'

  # change false multi-gen HHS
    pop <- check_multi_gen_hhs(pop)

  # Update parent status
    pop <- parent_indicator(pop)
    pop$hh_position[pop$hh_position=='union' & (pop$mother==1 | pop$father==1)]         <- 'union_w_child'
    pop$hh_position[pop$hh_position=='union_w_child' & (pop$mother==0 & pop$father==0)] <- 'union'
    id_union_w_child                                                                    <- pop$ID[pop$hh_position=='union_w_child']
    pop$hh_position[pop$ID_partner %in% id_union_w_child]                               <- 'union_w_child'                              

  # NH dummy
    pop$NH      <- as.numeric(pop$hh_position == 'collective')
    event_hh$NH <- as.numeric(event_hh$hh_position_target == 'collective' & !is.na(event_hh$hh_position_target))
    
    
    return(list(pop, pop_dt, event_hh, record_match, median_coll_hh))
  
}



###############################################
#    Population update at end of interval     #
############################################### 


# (i) Remove individuals

remove_ind <- function(pop, deaths, emi_pop, median_coll_hh){
  
  pop$age_group_trans   <- findInterval(pop$age, seq(0,open_age_trans_rates, age_int))  # Last group is 85+ for better results in the elderly pop (e.g. collective HHs). For couple formation the max age of 75 is still used.
  

  # update household size first
  setDT(pop)[, HH_size := .N, by = .(HH_ID)]
  pop <- setDF(pop)
  
  # Individuals to remove
  remove_pop                                 <- c(deaths$ID, emi_pop$ID)
  flag_remove_ind                            <- pop$ID %in% remove_pop
  #pop_dead_emi                               <- pop[flag_remove_ind, c()]
  
  # HH_ID of ind. to remove
  remove_pop_HH                              <- pop$HH_ID[flag_remove_ind]
  date_death                                 <- pop[flag_remove_ind, c('ID','event_date')]
  
  # remove dead and emigrated individuals
  pop                                        <- pop[!flag_remove_ind,]
  
  # update info partners to dead individuals
  flag_remove                                <- (pop$ID_partner %in% deaths$ID) & !(pop$ID %in% remove_pop)
  
  partner_mort                               <- pop[flag_remove, c('ID', 'HH_ID', 'hh_position','age','sex')]
  
  if (nrow(partner_mort)>0) {
    
    partner_mort$event_type                 <- 'hh_transition_dead_spouse' #'hh_transition'
    
  }
  
  partner_mort                               <- partner_mort[order(partner_mort$ID),]
  deaths                                     <- deaths[order(deaths$ID_partner),]
  partner_mort$event_date                    <- deaths$event_date[deaths$ID_partner %in% partner_mort$ID]
  
  pop$union[flag_remove]                     <- 0
  pop$age_partner[flag_remove]               <- NA
  pop$ID_partner[flag_remove]                <- NA
  multi_gen_union                            <- pop$ID[pop$hh_position=='multi_gen_union']
  pop$hh_position[flag_remove & pop$hh_position=='union_w_child']                       <- 'single_parent'
  pop$hh_position[flag_remove &  pop$HH_size==2]                                        <- 'single' 
  pop$hh_position[flag_remove &  pop$ID %in% multi_gen_union]                           <- 'multi_gen_single' 
  pop$hh_position[flag_remove &  pop$lipro_child_hh==0 & pop$HH_size>2]                 <- 'non_family' #HH size 2 because it has not been updated yet
  pop$hh_position[flag_remove &  pop$mother==0 & pop$father==0 & pop$HH_size>2]         <- 'non_family' 
  
  # Draw new household position if HH size 2
  flag_new_hh                             <- flag_remove & pop$HH_size==2
  age_sex_opt                             <- as.data.frame(unique(pop[flag_new_hh, c('age_group_trans')]))
  names(age_sex_opt)                      <- 'age_group_trans'
  
  for(i_opt in 1:nrow(age_sex_opt)){
    
    # get indices of the targeted subpopulation
    flag_subpop <-  flag_new_hh & pop$age_group_trans == age_sex_opt$age_group_trans[i_opt] 
    
    # get age, sex and lipro specific transition probability
    i_age  <- age_sex_opt$age_group_trans[i_opt]
    
    # Rates conditional on transition taking place
    prob_transitions   <- trans_dead_spouse[trans_dead_spouse$age==i_age,]
    if_no_trans        <- data.frame(age=i_age, hh_position='single',rate=1)
    
    if (nrow(prob_transitions)==0){
     
      prob_transitions <- if_no_trans 
      
    }
    
    # sample transitions
    sample_transitions <- sample(prob_transitions$hh_position, sum(flag_subpop), prob=prob_transitions$rate, replace=T)
    
    # update pop_data
    pop$transition[flag_subpop] <- as.character(sample_transitions)
    
    
  }
  
  # Move back to parents after spouse dies
  flag_child                          <- pop$transition == 'child' & flag_new_hh
  parents_hh                          <- pop[flag_child, c('ID','main_parent','ID_mother','ID_father')]
  parents_hh                          <- left_join(parents_hh, pop[,c('ID','HH_ID','hh_position')], by=c('main_parent'='ID'), na_matches="never")
  parents_hh                          <- left_join(parents_hh, pop[,c('ID','HH_ID','hh_position')], by=c('ID_mother'='ID'), na_matches="never")
  parents_hh                          <- left_join(parents_hh, pop[,c('ID','HH_ID','hh_position')], by=c('ID_father'='ID'), na_matches="never")
  parents_hh$new_hh                   <- ifelse(!is.na(parents_hh$HH_ID.x), parents_hh$HH_ID.x, ifelse(!is.na(parents_hh$HH_ID.y), parents_hh$HH_ID.y, parents_hh$HH_ID))
  parents_hh$par_hh_pos               <- ifelse(!is.na(parents_hh$hh_position.x), parents_hh$hh_position.x, ifelse(!is.na(parents_hh$hh_position.y), parents_hh$hh_position.y, parents_hh$HH_ID))
  flag_child_trans                    <- pop$ID %in% parents_hh$ID[!is.na(parents_hh$new_hh) & !(parents_hh$par_hh_pos %in% c('collective','multi_gen_union','multi_gen_single'))]
  flag_child_no_trans                 <- pop$transition == 'child' & !flag_child_trans
  pop$HH_ID[flag_child_trans]         <- parents_hh$new_hh[!is.na(parents_hh$new_hh) & !(parents_hh$par_hh_pos %in% c('collective','multi_gen_union','multi_gen_single'))]
  pop$hh_position[flag_child_trans & pop$hh_position!='union_w_child']   <- 'child'
  pop$hh_position[flag_child_trans & pop$hh_position=='union_w_child']   <- 'single_parent'
  pop$transition[flag_child_trans]    <- 'updated'
  pop$transition[flag_child_no_trans] <- 'no_trans'
  
  # Move to nfra/other/collective after spouse dies 
  flag_nfhh                       <- pop$transition == 'non_family' & flag_new_hh
  nfhh                            <- unique(pop[pop$hh_position %in% c('union','single_parent','union_w_child'),c('HH_ID','HH_children','HH_size')]) # find 'families' of union                            # Household ID of non-family related households
  nfhh_HH_children                <-  resamp(non_family_hh_size$Var1, sum(flag_nfhh), prob=non_family_hh_size$Freq, replace = TRUE)
  nfhh_0                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==2], sum(nfhh_HH_children==2), replace = FALSE)
  nfhh_1                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==3], sum(nfhh_HH_children==3), replace = FALSE)
  nfhh_2                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==4], sum(nfhh_HH_children==4), replace = FALSE)
  nfhh_3                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==5], sum(nfhh_HH_children==5), replace = FALSE)
  nfhh_4                          <-  resamp(nfhh$HH_ID[nfhh$HH_size>5], sum(nfhh_HH_children==6), replace = FALSE)
  nfhh_ID                         <- c(nfhh_0, nfhh_1, nfhh_2, nfhh_3, nfhh_4)
  pop$HH_ID[flag_nfhh]            <- nfhh_ID
  pop$hh_position[flag_nfhh]      <- 'non_family'
  pop$transition[flag_nfhh]       <- 'updated'
  pop$ID_partner[flag_nfhh]       <- NA
  pop$union[flag_nfhh]            <- 0
  
  # transition to Other HH
  flag_other                      <- pop$transition == 'other' & flag_new_hh
  
  if (sum(flag_other)>0){
    
    other_cand                      <- pop[flag_other,]
    other_hh_size                   <- resamp(other_dist$Var1, sum(flag_other), prob=other_dist$Freq, replace=T)
    other_cand$new_hh_size          <- other_hh_size
    
    # if new household size is 2 -> find match
    if (sum(other_cand$new_hh_size==2)>0){
      
      other_hh2                       <- other_cand[other_cand$new_hh_size==2,c('ID')]
      
      if (sum(other_cand$new_hh_size==2)==1){
        
        other_cand$new_hh_size        <- 3
        other_hh2                     <- NULL
        
      } else {
        
        new_HH_ID_hh2                   <- c((max(pop$HH_ID)+1):(max(pop$HH_ID)+floor(length(other_hh2)/2)))
        new_HH_ID_hh2                   <- rep(new_HH_ID_hh2, times=2)
        other_hh2                       <- cbind.fill(other_hh2, new_HH_ID_hh2, fill = NA)
        names(other_hh2)                <- c('ID','object')
        
      }} else {
        
        other_hh2                   <- NULL
      } 
    
    other_matched                   <- NULL
    
    for (i in unique(other_cand$new_hh_size[other_cand$new_hh_size!=2])) {
      
      max_id                      <- max(c(other_hh2$object, other_matched$object, pop$HH_ID))
      
      # find individuals
      other_ind                     <- data.frame(other_cand[other_cand$new_hh_size==i,c('ID')])
      
      # find matching HHs
      other_hhs                     <- pop[pop$hh_position=='other' & pop$HH_size==(i-1), c('HH_ID')] # i-1 because the size will increase by one
      
      # match HHs
      if (nrow(other_ind) <= length(unique(other_hhs))){
        
        new_hh_ID                   <- resamp(unique(other_hhs), size=sum(other_cand$new_hh_size==i), replace=FALSE)
        other_ind$object            <- new_hh_ID
        names(other_ind)            <- c('ID','object')
        
      } else {
        
        new_hh_ID1                  <- unique(other_hhs)
        num_new_hhs                 <- floor((nrow(other_ind)-length(new_hh_ID1)) / i)
        new_hh_ID2                  <- c((max_id+1):(max_id+num_new_hhs))
        new_hh_ID2                  <- rep(new_hh_ID2, times=i)
        new_hh_ID                   <- c(new_hh_ID1, new_hh_ID2)
        other_ind                   <- rowr::cbind.fill(other_ind, new_hh_ID, fill = NA)
        names(other_ind)            <- c('ID','object')
        
      }
      
      other_matched                 <- rbind.data.frame(other_matched, other_ind)
      names(other_matched)          <- c('ID','object')
    }
    
    other_hh_match                  <- rbind.data.frame(other_hh2, other_matched)
    
    # match residual -> size 2 is best because most 'other' households are size 2
    residual_ID                     <- is.na(other_hh_match$object)
    
    if (sum(residual_ID)>0){
      
      if (sum(residual_ID)==1){
        
        other_hh_match$object[residual_ID] <- other_hh_match$object[1]
        
      } else {
        
        other_hh2                       <- other_hh_match[residual_ID,'ID']
        new_HH_ID_hh2                   <- c(max(other_hh_match$object+1):(max(other_hh_match$object)+floor(sum(residual_ID)/2)))
        new_HH_ID_hh2                   <- rep(new_HH_ID_hh2, times=2)
        other_hh2                       <- rowr::cbind.fill(other_hh2, new_HH_ID_hh2, fill = NA)
        
        if(sum(is.na(other_hh2$object))>0){
          
          no_hh                         <- is.na(other_hh2$object)
          other_hh2$object[no_hh]       <- other_hh2$object[1]
          
        }
        
        other_hh_match$object[residual_ID] <- other_hh2$object
        
      } 
      
    }
    
    other_hh_match                  <- other_hh_match[!(is.na(other_hh_match$object) | is.na(other_hh_match$ID)),]
    pop$HH_ID[flag_other]           <- other_hh_match$object
    pop$hh_position[flag_other]     <- 'other'
    pop$transition[flag_other]      <- 'updated'
    
  }
  
  pop$ID_partner[flag_other]       <- NA
  pop$union[flag_other]            <- 0
  
  # transition to collective HH (single person)
  flag_coll                      <- pop$transition %in% c('collective') & flag_new_hh
  flag_coll                      <- pop[flag_coll, c('ID','age')]
  ID_union_coll                  <- pop[pop$transition=='union_collective', c('ID','ID_partner','event_date')] # union moving to same collective HH -> update at end
  
  if (nrow(flag_coll) > 0){
    
  # Capacity in collective households
  hh_capacity                    <- pop[pop$hh_position=='collective', c('HH_ID','HH_size','age')]
  median_age                     <- ddply(hh_capacity, .(HH_ID), summarize, median_age=median(age))
  hh_capacity                    <- hh_capacity[!duplicated(hh_capacity$HH_ID), c('HH_ID','HH_size')]
  hh_capacity                    <- merge(hh_capacity, median_coll_hh[,c('HH_ID','capacity')], by='HH_ID', all.x=FALSE, all.y=FALSE)
  hh_capacity$available          <- hh_capacity$capacity-hh_capacity$HH_size
  hh_capacity                    <- hh_capacity[hh_capacity$available>0,] 
  hh_capacity$hh_group           <- findInterval(hh_capacity$capacity, c(0,5,25,50,100))
  hh_capacity                    <- merge(hh_capacity, median_age, by='HH_ID', all.x = TRUE, all.y = FALSE)
  hh_capacity$min                <- hh_capacity$median_age-20
  hh_capacity$max                <- hh_capacity$median_age+20
  
  # Draw HH-size for ind.
  flag1                          <- flag_coll$age %in% c(0:29)
  flag2                          <- flag_coll$age %in% c(30:59)
  flag3                          <- flag_coll$age >= 60
  age1                           <- resamp(collective_age_size$hh_size[collective_age_size$age_group==1], sum(flag1), prob=collective_age_size$prop[collective_age_size$age_group==1], replace=TRUE)
  age2                           <- resamp(collective_age_size$hh_size[collective_age_size$age_group==2], sum(flag2), prob=collective_age_size$prop[collective_age_size$age_group==2], replace=TRUE)
  age3                           <- resamp(collective_age_size$hh_size[collective_age_size$age_group==3], sum(flag3), prob=collective_age_size$prop[collective_age_size$age_group==3], replace=TRUE)
  flag_coll$new_hh               <- NA
  flag_coll$new_hh[flag1]        <- age1    
  flag_coll$new_hh[flag2]        <- age2 
  flag_coll$new_hh[flag3]        <- age3 
  
  
  # Match
  flag_coll$new_HH_ID            <- NA
  missing_match                  <- c()
  
  for (i in 1:nrow(flag_coll)){
    
    size_i                       <- flag_coll$new_hh[i]
    age_i                        <- flag_coll$age[i]
    available_HHs                <- hh_capacity[hh_capacity$available > 0 & hh_capacity$hh_group==size_i & hh_capacity$min < age_i & hh_capacity$max > age_i,'HH_ID']
    
    if (length(available_HHs) > 0){
      
      draw                         <- resamp(available_HHs,1)
      flag_coll$new_HH_ID[i]       <- draw
      hh_capacity$available[hh_capacity$HH_ID==draw] <- hh_capacity$available[hh_capacity$HH_ID==draw]-1
      
    } else {
      
      missing_match <- c(missing_match, flag_coll$ID[i])
      
    }
    
  }
  
  # Create new collective household: 3 age groups for hh-size <25 and >=100, 2 age groups for the rest because these have very few ind: younger than 70
  missing_match      <- flag_coll[flag_coll$ID %in% missing_match,]
  flag_coll$capacity <- NA
  
  if (nrow(missing_match) > 0){
    
  for (i in c(1:5)){ 
    
    
    df_i_1 <- missing_match[missing_match$new_hh==i & missing_match$age < 30,]
    df_i_2 <- missing_match[missing_match$new_hh==i & missing_match$age %in% c(30:59),]
    df_i_3 <- missing_match[missing_match$new_hh==i & missing_match$age >= 60,]
    
    
    while (nrow(df_i_1) + nrow(df_i_2) + nrow(df_i_3) > 0){
      
      if (i==1){
        
        size   <- sample(c(2:4), 3, replace=TRUE)
        
      } else if (i==2){
        
        size   <- sample(c(5:24), 3, replace=TRUE)
        
        #} else if (i==3){
        
        #  size   <- sample(c(10:24), 3, replace=TRUE)          
        
      } else if (i==3){
        
        size   <- sample(c(25:49), 3, replace=TRUE)   
        
      } else if (i==4){
        
        size   <- sample(c(50:99), 3, replace=TRUE)   
        
      } else if (i==5){
        
        size   <- sample(c(100:400), 3, replace=TRUE)   
        
        #} else if (i==7){
        
        #  size   <- sample(100:400, 3, replace=TRUE)
        
      }
      
      if (nrow(df_i_1) >= size[1]){ # youngest age group -> df_i_1
        
        hh_1   <- resamp(df_i_1$ID, size[1], replace=F)
        
      } else {
        
        hh_1   <- resamp(df_i_1$ID, nrow(df_i_1), replace=F)
        
      }
      
      if (length(hh_1)>0){
        
        max_id                                      <- max(c(pop$HH_ID, flag_coll$new_HH_ID), na.rm=T)
        created_ID                                  <- rep((max_id+1), length(hh_1))
        flag_coll$new_HH_ID[flag_coll$ID %in% hh_1] <- created_ID
        flag_coll$capacity[flag_coll$ID %in% hh_1]  <- size[1]+ceiling(0.15*size[1])      
        df_i_1                                      <- df_i_1[!df_i_1$ID %in% hh_1,]
        
      }
      
      if (nrow(df_i_2) >= size[2]){ # middle age group -> df_i_2
        
        hh_2   <- resamp(df_i_2$ID, size[2], replace=F)
        
      } else {
        
        hh_2   <- resamp(df_i_2$ID, nrow(df_i_2), replace=F)
        
      }
      
      if (length(hh_2)>0){
        
        max_id                                      <- max(c(pop$HH_ID, flag_coll$new_HH_ID), na.rm=T)
        created_ID                                  <- rep((max_id+1), length(hh_2))
        flag_coll$new_HH_ID[flag_coll$ID %in% hh_2] <- created_ID
        flag_coll$capacity[flag_coll$ID %in% hh_2]  <- size[2]+ceiling(0.15*size[2])  
        df_i_2                                      <- df_i_2[!df_i_2$ID %in% hh_2,]
        
      }
      
      if (nrow(df_i_3) >= size[3]){ # oldest age group -> df_i_3
        
        hh_3   <- resamp(df_i_3$ID, size[3], replace=F)
        
      } else {
        
        hh_3   <- resamp(df_i_3$ID, nrow(df_i_3), replace=F)
        
      }
      
      if (length(hh_3)>0){
        
        max_id                                      <- max(c(pop$HH_ID, flag_coll$new_HH_ID), na.rm=T)
        created_ID                                  <- rep((max_id+1), length(hh_3))
        flag_coll$new_HH_ID[flag_coll$ID %in% hh_3] <- created_ID
        flag_coll$capacity[flag_coll$ID %in% hh_3]  <- size[3]+ceiling(0.15*size[3])   
        df_i_3                                      <- df_i_3[!df_i_3$ID %in% hh_3,]
        
      }
      
    }#end while loop
    
  } # end for loop
  
  # Add newly created HHs to median_coll_hh
  newly_created           <- flag_coll[flag_coll$new_HH_ID > max(pop$HH_ID),]
  median_new              <- ddply(newly_created, .(new_HH_ID), summarize, median_age=median(age))
  median_new              <- merge(median_new, newly_created[,c('new_HH_ID','capacity')], all.x = TRUE, all.y = FALSE)
  median_new              <- median_new[!duplicated(median_new$new_HH_ID),]
  names(median_new)       <- c('HH_ID','median_age','capacity')
  median_coll_hh          <- rbind.fill(median_coll_hh, median_new)
  
  }
  
  ID_union_coll                   <- merge(ID_union_coll, flag_coll[,c('ID','new_HH_ID')], by='ID')
  ID_union_coll                   <- ID_union_coll[,-1]
  names(ID_union_coll)            <- c('ID','event_date','new_HH_ID')
  flag_coll                       <- rbind.fill(flag_coll, ID_union_coll)
  pop                             <- pop[order(pop$ID),]
  flag_coll                       <- flag_coll[order(flag_coll$ID),]
  ID_union_coll                   <- ID_union_coll[order(ID_union_coll$ID),]
  flag                            <- pop$ID %in% flag_coll$ID
  flag_partner                    <- pop$ID %in% ID_union_coll$ID
  pop$HH_ID[flag]                 <- flag_coll$new_HH_ID
  pop$hh_position[flag]           <- 'collective'
  pop$ID_partner[flag & pop$transition=='collective'] <- NA
  pop$union[flag & pop$transition=='collective']      <- 0
  pop$transition[flag]            <- 'updated'

}
  
  
  # event_log widow(er)s
  update_spouse                   <- pop[pop$ID %in% partner_mort$ID, c('ID','HH_ID','hh_position')]
  partner_mort                    <- merge(partner_mort, update_spouse, by='ID')
  names(partner_mort)             <- c("ID", "HH_ID","hh_position","age", "sex","event_type","event_date","HH_ID_target","hh_position_target")
  

  # update children if no parents in pop
  pop                        <-  setDF(pop)
  flag_adopted               <-  pop$hh_position=='child' &  pop$age < child_age_lim & (!pop$ID_father %in% pop$ID | is.na(pop$ID_father)) & (!pop$ID_mother %in% pop$ID | is.na(pop$ID_mother)) & (!pop$main_parent %in% pop$ID | is.na(pop$main_parent)) 
  child_mort                 <-  pop[flag_adopted, c('ID','HH_ID','main_parent','ID_mother','ID_father')]
  child_mort                 <-  left_join(child_mort, deaths[,c('ID','event_date')], by=c('main_parent'='ID'))
  child_mort                 <-  left_join(child_mort, deaths[,c('ID','event_date')], by=c('ID_mother'='ID'))
  child_mort                 <-  left_join(child_mort, deaths[,c('ID','event_date')], by=c('ID_father'='ID'))
  child_mort[, "event_date"] <-  apply(child_mort[, 6:8], 1, max)

  
  # add date of adoption
  
  if (sum(flag_adopted)>0){
    
    adopted                    <- as.data.frame(pop[flag_adopted,])
    adoptive_parents           <- resamp(pop$ID[pop$union==1 & pop$age<50 & pop$sex=='F'], sum(flag_adopted), replace=F)  # Children are adopted by females in a union who are younger than 50 years
    adoptive_parents           <- adoptive_parents[order(adoptive_parents)]
    partner_adoptive_mother    <- pop$ID_partner[pop$ID %in% adoptive_parents]
    adopted$mother             <- adoptive_parents
    adopted$father             <- partner_adoptive_mother
    adopted$hh                 <- pop$HH_ID[pop$ID %in% adopted$mother]
    flag_adopted_pop           <- pop$ID %in% adopted$ID
    pop[flag_adopted_pop, c('HH_ID','ID_mother', 'ID_father')] <- adopted[,c('hh','mother','father')]
    pop$main_parent[flag_adopted_pop] <- pop$ID_mother[flag_adopted_pop]
    
    adoption_event             <- pop[flag_adopted_pop, c('ID','HH_ID')]
    names(adoption_event)      <- c('ID','HH_ID.y')
    adoption_event             <- cbind.data.frame(child_mort[,-c(3:7)], adoption_event[,-1])
    names(adoption_event)      <- c('ID','HH_ID','event_date','HH_ID_target')

  }
  
  
  # move to other parent's household if main parent is dead
  flag_child                   <- pop$hh_position=='child' & pop$main_parent %in% deaths$ID & !(flag_adopted) & !is.na(pop$main_parent)
  child_mort                   <- pop[flag_child, c('ID','HH_ID')]
  child_dead_parent            <- pop[flag_child, c('ID','HH_ID','age','ID_mother','ID_father','main_parent')]
  child_dead_parent            <- left_join(child_dead_parent, pop[,c('ID','HH_ID','hh_position')], by=c('ID_mother'='ID'), na_matches = "never")
  child_dead_parent            <- left_join(child_dead_parent, pop[,c('ID','HH_ID','hh_position')], by=c('ID_father'='ID'), na_matches = "never")
  child_dead_parent$HH_ID.y[child_dead_parent$hh_position.y == 'collective'] <- NA
  child_dead_parent$HH_ID[child_dead_parent$hh_position.x == 'collective'] <- NA
  
  child_dead_parent$new_HH     <- ifelse(is.na(child_dead_parent$HH_ID.y), child_dead_parent$HH_ID, child_dead_parent$HH_ID.y)
  child_dead_parent            <- left_join(child_dead_parent, deaths[,c('ID','event_date')], by=c('main_parent'='ID'))
  child_dead_parent$main_parent <- ifelse(is.na(child_dead_parent$HH_ID.y) & !is.na(child_dead_parent$HH_ID), child_dead_parent$ID_father, ifelse(!is.na(child_dead_parent$HH_ID.y) & is.na(child_dead_parent$HH_ID), child_dead_parent$ID_mother, NA))
  pop$main_parent[pop$ID %in% child_dead_parent$ID[!is.na(child_dead_parent$new_HH)]] <- child_dead_parent$main_parent[!is.na(child_dead_parent$new_HH)]
  pop$main_parent[pop$ID %in% child_dead_parent$ID[is.na(child_dead_parent$new_HH) & child_dead_parent$age >= child_age_lim]]  <- NA 
  pop$hh_position[pop$ID %in% child_dead_parent$ID[is.na(child_dead_parent$new_HH) & child_dead_parent$age >= child_age_lim]]  <- 'single' 
  child_dead_parent            <- child_dead_parent[child_dead_parent$HH_ID.x!=child_dead_parent$new_HH & !is.na(child_dead_parent$new_HH),]
  pop$HH_ID[pop$ID %in% child_dead_parent$ID]         <- child_dead_parent$new_HH
  pop$ID_mother[pop$ID_mother %in% deaths$ID]         <- NA
  pop$ID_father[pop$ID_father %in% deaths$ID]         <- NA
  child_new_hh                 <- child_dead_parent[,c('ID','HH_ID.x','new_HH','event_date')]
  names(child_new_hh)          <- c('ID','HH_ID','HH_ID_target','event_date')
  
  child_mort                   <- rbind.fill(adoption_event, child_new_hh)
  
  if (nrow(child_mort)>0) {
    
    child_mort$event_type    <- 'hh_transition'
    
  }
  

  # combine event logs
  partner_mort$event_date      <- as.Date(partner_mort$event_date)
  child_mort$event_date        <- as.Date(child_mort$event_date)
  hh_trans_mort                <- rbind.fill(partner_mort, child_mort)
  
  # update household size and number of children
  sub_pop_flag                   <- pop$HH_ID %in% c(remove_pop_HH, child_mort$HH_ID_target)
  sub_pop                        <- pop[sub_pop_flag, c('ID','HH_ID','child')]
  setDT(sub_pop)[, HH_size := .N, by = .(HH_ID)]
  setDT(sub_pop)[, HH_children := sum(child), by = .(HH_ID)]
  
  pop$HH_size[sub_pop_flag]      <- sub_pop$HH_size
  pop$HH_children[sub_pop_flag]  <- sub_pop$HH_children    
  
  # HH-size 1 -> single HH
  pop$hh_position[pop$HH_size==1] <- 'single'
  
  # No child in HH -> single-parent -> single
  pop$hh_position[pop$hh_position=='single_parent' & pop$lipro_child_hh==0] <- 'single'
  
  # change non-family to other if only non_family in HH or household doesn't contain union/single_parent
  pop <- setDT(pop)
  pop[, num_nonfam := sum(!(hh_position %in% c('union','single_parent','collective','union_w_child'))), by=HH_ID]
  pop$hh_position[pop$num_nonfam==pop$HH_size & pop$HH_size>1 & pop$age>child_age_lim] <- 'other'
  pop <- setDF(pop)
  pop <- pop[ ,-ncol(pop)]
  
  
  # NH dummy
  pop$NH           <- as.numeric(pop$hh_position == 'collective')
  hh_trans_mort$NH <- as.numeric(hh_trans_mort$hh_position_target == 'collective' & !is.na(hh_trans_mort$hh_position_target))
  
  
  condition <- sum(is.na(pop$HH_ID))>0
  if (condition) {
    print('Reason for stopping')
    UNDECLARED()
  }
  
  
  return(list(pop, hh_trans_mort, median_coll_hh))
  
  
  
}


# (ii) Update individuals

update_pop <- function(pop){
  
  pop$age                       <- floor(time_length(difftime(as.Date(end_time_step), pop$birth_date), "years"))                   

  pop$birth_date[pop$age < 0]   <- as.Date(model_param$time_steps_date[i_time_step], origin = "1970-01-01")
  pop$age[pop$age < 0]          <- 0
  
  pop$age_group_hh              <- findInterval(pop$age, age_cut_hh) 
  pop$age_group_pop             <- findInterval(pop$age, age_cut_pop) 
  
  flag_pop                      <- !(is.na(pop$ID_partner))
  partner_info                  <- pop[flag_pop,c('ID_partner','age')]
  pop$age_partner[flag_pop]     <- partner_info[ ,c('age')]
  
  child_match                    <- merge(pop[,c('ID','HH_ID','ID_mother','ID_father')], pop[,c('ID','HH_ID')], by.x = 'ID_mother', by.y = 'ID', all.x=T,incomparables=NA)
  child_match                    <- merge(child_match, pop[,c('ID','HH_ID')], by.x = 'ID_father', by.y = 'ID', all.x=T,incomparables=NA)
  child_match$hh_match_mother_11 <- as.numeric(child_match$HH_ID.x==child_match$HH_ID.y)
  child_match$hh_match_mother_11[is.na(child_match$hh_match_mother_11)] <- 0
  child_match$hh_match_father_11 <- as.numeric(child_match$HH_ID.x==child_match$HH_ID)
  child_match$hh_match_father_11[is.na(child_match$hh_match_father_11)] <- 0
  pop$mother                     <- 0
  pop$father                     <- 0
  pop$mother                     <- as.numeric(pop$ID %in% child_match$ID_mother[child_match$hh_match_mother_11==1])
  pop$father                     <- as.numeric(pop$ID %in% child_match$ID_father[child_match$hh_match_father_11==1])
  pop$parent                     <- 0
  pop$parent                     <- as.numeric(pop$mother==1 | pop$father==1)
  
  pop$union                      <- as.numeric(pop$hh_position %in% c('union','multi_gen_union','union_w_child'))
  pop$ID_partner[pop$union==0]   <- NA
  
  ### Update main parent
  main_parent     <- left_join(pop[pop$age < child_age_lim, c('ID','main_parent','HH_ID')], pop[,c('ID','HH_ID')], by=c('main_parent'='ID'), na_matches = "never")
  main_parent     <- main_parent[main_parent$HH_ID.x != main_parent$HH_ID.y | is.na(main_parent$HH_ID.y),]
   
  # Find HH member that can be main-parent
  child_check                                <-  pop[pop$age < child_age_lim, c('ID','HH_ID','main_parent')]
  child_check                                <- left_join(child_check, pop[,c('ID','HH_ID')], by=c('main_parent'='ID'))
  child_check                                <- child_check[child_check$HH_ID.x != child_check$HH_ID.y & !is.na(child_check$HH_ID.y) | is.na(child_check$main_parent),]
  child_check                                <- left_join(child_check, pop[pop$age>17, c('ID','HH_ID','age','hh_position')], by=c('HH_ID.x'='HH_ID'))
  child_check                                <- child_check[!duplicated(child_check[,1:2]),]
  pop$main_parent[pop$ID %in% child_check$ID.x] <- child_check$ID.y

  return(pop)
  
  }







