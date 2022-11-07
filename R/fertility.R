######################################
###           Fertility            ###
######################################


###### (i) Births

births <- function(pop, i_time_step, fertility_age_adj){

  ### Update info and eligibility
  
  # Females of fertile age and applicable household position
  fertile_pop                         <- pop[pop$sex=='F' & !pop$hh_position %in% c('collective', 'multi_gen_single','multi_gen_union') , c('ID', 'age', 'date_prev_birth','hh_position', 'HH_size', 'num_births', 'union', 'age_first_birth','birth_date')]
  fertile_pop$age                     <- floor(difftime(paste(model_param$time_step_year[i_time_step],'-06-30',sep=''), fertile_pop$birth_date, units = 'days')/365) 
  fertile_pop                         <- fertile_pop[fertile_pop$age <= 50 & fertile_pop$age >= 14,]

  # Update time since previous birth
  fertile_pop$time_since_last_birth   <-  difftime(model_param$time_steps_date[i_time_step], fertile_pop$date_prev_birth, units = "days") + 365/2
  fertile_pop$time_since_last_birth   <- as.numeric(fertile_pop$time_since_last_birth/365)
  fertile_pop$age_first_birth         <- as.numeric(fertile_pop$age_first_birth)
  
  # Eligibility (14-50 years, time since previous birth)
  fertile_pop                          <- fertile_pop[(fertile_pop$time_since_last_birth > (model_param$LA)) | (is.na(fertile_pop$time_since_last_birth)),]   

  # Year indicator
  fertile_pop$year                     <- ceiling(fertile_pop$time_since_last_birth)
  fertile_pop$year[fertile_pop$year>5] <- 5
  fertile_pop$year                     <- as.factor(fertile_pop$year)
  
  # Parity
  fertile_pop$parity_begin             <- fertile_pop$num_births

  # Risk sets
  fertile_pop_parity1                  <- fertile_pop[fertile_pop$num_births==0 & fertile_pop$hh_position %in% c('single','single_parent','union','union_w_child','child','other','non_family'),]
  fertile_pop_parity1$hh_position[fertile_pop_parity1$hh_position %in% c('single','single_parent') & !is.na(fertile_pop_parity1$hh_position)] <- 'single'
  fertile_pop_parity1$hh_position[fertile_pop_parity1$hh_position %in% c('union',"union_w_child") & !is.na(fertile_pop_parity1$hh_position)] <- 'union'
  
  fertile_pop_parity2_single           <- fertile_pop[fertile_pop$num_births==1 & fertile_pop$hh_position %in% c('single','single_parent'),]
  fertile_pop_parity3_single           <- fertile_pop[fertile_pop$num_births==2 & fertile_pop$hh_position %in% c('single','single_parent'),]
  fertile_pop_parity4_single           <- fertile_pop[fertile_pop$num_births>2 & fertile_pop$hh_position %in% c('single','single_parent'),]
  
  fertile_pop_parity2_union            <- fertile_pop[fertile_pop$num_births==1 & fertile_pop$hh_position %in% c('union','union_w_child'),]
  fertile_pop_parity3_union            <- fertile_pop[fertile_pop$num_births==2 & fertile_pop$hh_position %in% c('union','union_w_child'),]
  fertile_pop_parity4_union            <- fertile_pop[fertile_pop$num_births>2 & fertile_pop$hh_position %in% c('union','union_w_child'),]

  ### Apply model
  
  # Parity 1
  prob_parity1                         <- exp(predict(logitgam1, fertile_pop_parity1))
  fertile_pop_parity1$prob             <- (prob_parity1/(1+prob_parity1))/model_param$t_per_year
  fertile_pop_parity1$random           <- runif(nrow(fertile_pop_parity1),0,1)
  fertile_pop_parity1$event            <- as.numeric(fertile_pop_parity1$random < fertile_pop_parity1$prob)
  
  # Parity 2
  prob_parity2                         <- exp(predict(logitgam2_single, fertile_pop_parity2_single))
  fertile_pop_parity2_single$prob      <- (prob_parity2/(1+prob_parity2))/model_param$t_per_year
  fertile_pop_parity2_single$random    <- runif(nrow(fertile_pop_parity2_single),0,1)
  fertile_pop_parity2_single$event     <- as.numeric(fertile_pop_parity2_single$random < fertile_pop_parity2_single$prob) 
  prob_parity2                         <- exp(predict(logitgam2_union, fertile_pop_parity2_union))
  fertile_pop_parity2_union$prob       <- (prob_parity2/(1+prob_parity2))/model_param$t_per_year
  fertile_pop_parity2_union$random     <- runif(nrow(fertile_pop_parity2_union),0,1)
  fertile_pop_parity2_union$event      <- as.numeric(fertile_pop_parity2_union$random < fertile_pop_parity2_union$prob)
  
  # Parity 3
  fertile_pop_parity3_single$index_birth <- fertile_pop_parity3_single$age_first_birth
  prob_parity3                           <- exp(predict(logitgam3_single, fertile_pop_parity3_single))
  fertile_pop_parity3_single$prob        <- (prob_parity3/(1+prob_parity3))/model_param$t_per_year
  fertile_pop_parity3_single$random      <- runif(nrow(fertile_pop_parity3_single),0,1)
  fertile_pop_parity3_single$event       <- as.numeric(fertile_pop_parity3_single$random < fertile_pop_parity3_single$prob)
  fertile_pop_parity3_union$index_birth  <- fertile_pop_parity3_union$age_first_birth
  prob_parity3                           <- exp(predict(logitgam3_union, fertile_pop_parity3_union))
  fertile_pop_parity3_union$prob         <- (prob_parity3/(1+prob_parity3))/model_param$t_per_year
  fertile_pop_parity3_union$random       <- runif(nrow(fertile_pop_parity3_union),0,1)
  fertile_pop_parity3_union$event        <- as.numeric(fertile_pop_parity3_union$random < fertile_pop_parity3_union$prob)
  
  # Parity 4+
  fertile_pop_parity4_single$age_index_birth <- fertile_pop_parity4_single$age_first_birth
  prob_parity4                           <- exp(predict(logitgam4_single, fertile_pop_parity4_single))
  fertile_pop_parity4_single$prob        <- (prob_parity4/(1+prob_parity4))/model_param$t_per_year
  fertile_pop_parity4_single$random      <- runif(nrow(fertile_pop_parity4_single),0,1)
  fertile_pop_parity4_single$event       <- as.numeric(fertile_pop_parity4_single$random < fertile_pop_parity4_single$prob)
  fertile_pop_parity4_union$age_index_birth <- fertile_pop_parity4_union$age_first_birth
  prob_parity4                           <- exp(predict(logitgam4_union, fertile_pop_parity4_union))
  fertile_pop_parity4_union$prob         <- (prob_parity4/(1+prob_parity4))/model_param$t_per_year
  fertile_pop_parity4_union$random       <- runif(nrow(fertile_pop_parity4_union),0,1)
  fertile_pop_parity4_union$event        <- as.numeric(fertile_pop_parity4_union$random < fertile_pop_parity4_union$prob)
  
  # Events
  birth_events                                  <- rbind.fill(fertile_pop_parity1, fertile_pop_parity2_single, fertile_pop_parity3_single, fertile_pop_parity4_single, fertile_pop_parity2_union, fertile_pop_parity3_union, fertile_pop_parity4_union)
  birth_events$event[is.na(birth_events$event)] <- 0
  
  # ID of females with birth event
  ID_mothers                                    <- c(birth_events$ID[birth_events$event==1])
  
  # Descriptive statistics of mothers
  mothers_info                                  <- birth_events[birth_events$event==1,]
  
  return(list(ID_mothers, mothers_info))

  }




###### (ii) Update info (mother)

update_mother <- function(pop, birth_events, i_time_step, event_hh){

  flag_pop                            <- pop$ID %in% birth_events
    
  # Twin births (1.8% of births)
  twins_mothers                       <- sample(pop$ID[flag_pop], round(0.018*length(pop$ID[flag_pop])), replace = FALSE)
    
  # Triplets (0.02% of births)
  triplets_mothers                    <- sample(pop$ID[flag_pop], round(0.0002*length(pop$ID[flag_pop])), replace = FALSE)
    
    
  # Increase parity    
    pop$num_births[flag_pop]                              <- pop$num_births[flag_pop]+1
    pop$num_births[flag_pop & is.na(pop$num_births)]      <- 1
    pop$num_births[pop$ID %in% twins_mothers]             <- pop$num_births[pop$ID %in% twins_mothers]+1
    pop$num_births[pop$ID %in% triplets_mothers]          <- pop$num_births[pop$ID %in% triplets_mothers]+1
  
  # Date of birth 
    date_birth_events                                     <- sample(seq(as.Date(model_param$time_steps_date[i_time_step]), as.Date(end_time_step), by="day"), sum(flag_pop), replace = TRUE)
    pop$date_prev_birth[flag_pop]                         <- date_birth_events
    pop$date_prev_birth                                   <- as.Date(as.numeric(pop$date_prev_birth),origin = '1970-01-01')
  
  # Age 1st birth -> changed to index birth but name kept the same for simplicity
    pop$age_first_birth[flag_pop]                          <- floor(time_length(interval(as.Date(pop$birth_date[flag_pop]), as.Date(pop$date_prev_birth[flag_pop])),"years"))    

  # Mother/father indicator
    pop$mother[flag_pop]                                  <- 1
    father_newborn                                        <- pop$ID_partner %in% birth_events
    pop$father[father_newborn]                            <- 1
  
  # Date event adjusted to date of birth
    pop$event_date[flag_pop]                              <- pop$date_prev_birth[flag_pop]
    father_birth_event                                    <- pop[pop$ID %in% birth_events, c('ID','ID_partner','date_prev_birth')]
    father_birth_event                                    <- father_birth_event[!is.na(father_birth_event$ID_partner),]
    father_birth_event                                    <- father_birth_event[order(father_birth_event$ID_partner),]
    pop$event_date[pop$ID %in% father_birth_event$ID_partner] <- father_birth_event$date_prev_birth
    
  # Single -> Single mother
    pop$hh_position[flag_pop & is.na(pop$ID_partner) & pop$hh_position %in% c('single')]     <- 'single_parent'
  
  # Probability of changing hh-pos after having a child
    candidates                                             <- pop[flag_pop & pop$hh_position %in% c('child','single','single_parent','other','non_family') & pop$age > child_age_lim, c('ID','age','hh_position','num_births')]
    candidates$age_group                                   <- findInterval(candidates$age, c(0,20,25,30,35,40,100))
    candidates$parity                                      <- ifelse(candidates$num_births>1,2,1)
    candidates$Var1                                        <- candidates$hh_position
    candidates$Var1[candidates$hh_position == 'single']    <- 'single_parent'
    candidates$Var1[candidates$hh_position == 'non_family']<- 'other'
    age_hh_freq                                            <- as.data.frame(unique(candidates[, c('age_group','Var1','parity')]))
    
    for(i_opt in 1:nrow(age_hh_freq)){
      
      flag_cand                            <- candidates$age_group==age_hh_freq$age_group[i_opt] & candidates$parity==age_hh_freq$parity[i_opt] & candidates$Var1==age_hh_freq$Var1[i_opt]
      prob_vec                             <- hh_after_birth[hh_after_birth$Var1==age_hh_freq$Var1[i_opt] & hh_after_birth$parity==age_hh_freq$parity[i_opt] & hh_after_birth$age==age_hh_freq$age_group[i_opt],]
      sample_hh_pos                        <- resamp(prob_vec$Var2, sum(flag_cand), prob=prob_vec$Freq, replace=TRUE)
      candidates$transition[flag_cand]     <- sample_hh_pos
      pop$transition[pop$ID %in% candidates$ID[flag_cand]] <- sample_hh_pos    
      
    }
    
    df_leaving                             <- pop[pop$ID %in% candidates$ID & pop$transition=='union',c('ID','HH_ID','hh_position','sex','age','transition','event_date','age_group_pop','age_group_hh')]
    
    ##### Transition to union
    new_unions                             <- create_unions_after_birth(pop, df_leaving)
    pop                                    <- new_unions[[1]]
    df_leaving                             <- new_unions[[2]]
    record_match                           <- new_unions[[3]]

    # transition to single-parent HH
    flag_single                            <- pop$ID %in% candidates$ID[candidates$transition=='single_parent']
    single_events                          <- pop[flag_single,c('ID','HH_ID','hh_position','age_group_pop','age_group_hh')]
    single_ID                       <- (max(pop$HH_ID)+1):(max(pop$HH_ID)+sum(flag_single))    #New HH ID
    pop$HH_ID[flag_single]          <- single_ID
    pop$hh_position[flag_single]    <- 'single_parent'
    pop$transition[flag_single]     <- 'updated'
    pop$ID_partner[flag_single]     <- NA
    pop$union[flag_single]          <- 0
    single_events                   <- merge(single_events, pop[,c('ID','HH_ID','hh_position','event_date')], by='ID', all.y = FALSE)
    names(single_events)            <- c('ID','HH_ID','hh_position','age_group_pop','age_group_hh','HH_ID_target','hh_position_target','event_date')
    df_leaving                      <- rbind.fill(df_leaving, single_events)
 
    # add children if currently living in household of mother 
    child_mother_trans                                 <- pop[pop$hh_position=='child' & pop$main_parent %in% df_leaving$ID, c('ID','main_parent','HH_ID','age_group_hh','age_group_pop')]
    
    if (nrow(child_mother_trans)>0) {
      
      names(child_mother_trans)                          <- c('ID_child', 'ID','HH_ID','age_group_pop','age_group_hh')
      new_HH_mother                                      <- pop[pop$ID %in% df_leaving$ID, c('ID','HH_ID', 'HH_size','event_date')]
      merge_mother_child                                 <- left_join(child_mother_trans, new_HH_mother, by='ID')
      pop                                                <- pop[order(pop$ID),]
      merge_mother_child                                 <- merge_mother_child[order(merge_mother_child$ID_child),]
      pop$HH_ID[pop$ID %in% merge_mother_child$ID_child] <- merge_mother_child$HH_ID.y
      pop$event_date[pop$ID %in% merge_mother_child$ID_child] <- merge_mother_child$event_date
      pop$HH_size[pop$ID %in% merge_mother_child$ID_child]    <- merge_mother_child$HH_size
      
      child_trans            <- merge_mother_child[,-c(2,7)]
      names(child_trans)     <- c('ID','HH_ID','age_group_hh','age_group_pop','HH_ID_target','event_date')
      child_trans$event_type <- 'hh_transition'
      
    } else {
      
      child_trans <- data.frame()
    }
    

    # add grandchildren if currently living in multi-gen
    child_mother_trans2                                 <- pop[pop$hh_position=='child' & pop$main_parent %in% child_mother_trans$ID_child & pop$age<18, c('ID','main_parent','HH_ID','age_group_hh','age_group_pop')]
    
    if (nrow(child_mother_trans2)>0) {
      
      names(child_mother_trans2)                          <- c('ID_child', 'ID','HH_ID','age_group_pop','age_group_hh')
      new_HH_mother                                      <- pop[pop$ID %in% child_mother_trans2$ID, c('ID','HH_ID', 'HH_size','event_date')]
      merge_mother_child                                 <- left_join(child_mother_trans2, new_HH_mother, by='ID')
      pop                                                <- pop[order(pop$ID),]
      merge_mother_child                                 <- merge_mother_child[order(merge_mother_child$ID_child),]
      pop$HH_ID[pop$ID %in% merge_mother_child$ID_child] <- merge_mother_child$HH_ID.y
      pop$event_date[pop$ID %in% merge_mother_child$ID_child] <- merge_mother_child$event_date
      pop$HH_size[pop$ID %in% merge_mother_child$ID_child]    <- merge_mother_child$HH_size
      child_trans2                                            <- merge_mother_child[,-c(2,7)]
      names(child_trans2)                                     <- c('ID','HH_ID','age_group_hh','age_group_pop','HH_ID_target','event_date')
      child_trans2$event_type                                 <- 'hh_transition'
      
    } else {
      
      child_trans2 <- data.frame()
    }
    
    # add children affected by move
    mother_trans                                            <- rbind.fill(df_leaving, child_trans, child_trans2)  
    mother_trans$event_type                                 <- 'hh_transition'
    mother_trans                                            <- mother_trans[,-which(names(mother_trans) %in% c('transition','main_parent'))]
    
    # update union/union_w_child                            
    pop$hh_position[pop$hh_position=='union' & (pop$mother==1 | pop$father==1)]    <- 'union_w_child'
    
    # child -> Single mother (but staying in parental household) -> parental household changing to multi-gen
    pop$hh_position[pop$ID %in% birth_events & is.na(pop$ID_partner) & pop$hh_position %in% c('child')]     <- 'single_parent'
    
  return(list(pop, mother_trans, twins_mothers, triplets_mothers, record_match))

    }


###### (iib) Update info of new multi-gen HHs

update_multi_gen <- function(pop){
  
  children <- pop[( !is.na(pop$ID_father) | !is.na(pop$ID_mother) ) & pop$ID %in% birth_events, c('ID', 'HH_ID','hh_position','ID_mother','ID_father')]
  parents  <- pop[pop$ID %in% c(children$ID_father, children$ID_mother), c('ID','HH_ID','hh_position')]
  children <- left_join(children, parents, by=c('HH_ID'), na_matches="never")
  children <- children[children$ID_mother==children$ID.y & !is.na(children$ID_mother) & !is.na(children$ID.y) | children$ID_father==children$ID.y & !is.na(children$ID_father) & !is.na(children$ID.y),]
  pop$hh_position[(pop$ID %in% children$ID.y | pop$ID_partner %in% children$ID.y) & pop$hh_position %in% c('union','union_w_child')]   <- 'multi_gen_union'
  pop$hh_position[pop$ID %in% children$ID.y & pop$hh_position!='multi_gen_union']                                                      <- 'multi_gen_single'
  
  # No need to update event-log since there is no household change
  
  return(pop) 
}


##### (iii) Generate children 

generate_children <- function(pop, mother_trans, twins_mothers, triplets_mothers){
  
   flag_pop         <- pop$ID %in% birth_events
   new_children     <- data.frame()

      # variables at birth
      HH_ID          <- pop$HH_ID[flag_pop]
      age            <- num_births <- union <- mother <- father <- rep(0, length(birth_events))
      sex            <- sample(c('M', 'F'), length(birth_events), prob=c(0.5121951, 0.4878049), replace = T)  # sex-ratio at birth 105 males to 100 females
      hh_position    <- child <- rep('child', length(birth_events))
      ID             <- seq((max(pop$ID) + 1), (max(pop$ID) +length(birth_events)))
      ID_mother      <- pop$ID[flag_pop]
      main_parent    <- ID_mother
      ID_father      <- pop$ID_partner[flag_pop]
      birth_date     <- event_date <- pop$date_prev_birth[flag_pop] 
      HH_size        <- pop$HH_size[flag_pop]
      age_group_hh   <- age_group_pop <- age_group <- 1
      child          <- 1
      ind            <- cbind.data.frame(HH_ID, age, sex, hh_position, ID, num_births, union, child,mother, father, ID_mother, ID_father, main_parent, birth_date, child, HH_size, age_group_hh, age_group_pop, age_group, event_date)
      new_children   <- as.data.frame(ind)
      
      twins          <- new_children[new_children$ID_mother %in% twins_mothers,]
      twins$ID       <- seq((max(new_children$ID) + 1), (max(new_children$ID) + nrow(twins)))
      triplets       <- new_children[new_children$ID_mother %in% triplets_mothers,]
      triplets$ID    <- seq((max(twins$ID) + 1), (max(twins$ID) + nrow(triplets)))
      multiple_births <- rbind.fill(twins, triplets)
      new_children   <- rbind.fill(new_children, multiple_births)
      
      newborns_event                 <- new_children[,c('ID','HH_ID','ID_mother','ID_father','main_parent','birth_date', 'sex', 'age_group_hh','age_group_pop')]
      newborns_event$event_date      <- newborns_event$birth_date
      newborns_event$event_type      <- 'birth'
      birth_events                   <- rbind.fill(newborns_event, mother_trans)

      

  return(list(new_children, birth_events))
}


##### (iv) Update info for households with newborns

update_hh_newborns <- function(pop, birth_events){
  
  flag_hh                                 <- pop$HH_ID[pop$ID %in% birth_events]
  pop$HH_size[pop$HH_ID %in% flag_hh]     <- pop$HH_size[pop$HH_ID %in% flag_hh] + 1
  pop$HH_children[pop$HH_ID %in% flag_hh] <- pop$HH_children[pop$HH_ID %in% flag_hh] + 1
  
  return(pop)
}




      
      