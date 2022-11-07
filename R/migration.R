###########################################
#              Migration                  #
###########################################



############# Emigration ##################

emigration <- function(pop){
  
emi_rate            <- migration_rate$emi_rate[migration_rate$year == model_param$time_step_year[i_time_step]] # emigration rate
emi_rate            <- adjust_prob(emi_rate, model_param$t_per_year)                                           # emigration rate adjusted by time-step
emi_num             <- emi_rate*sum(pop_age_sex[[i_time_step]]$Freq)                                           # number of emigrants
emi_pop             <- c()

num_left            <- emi_num-length(emi_pop)

while (num_left>0){
  
# unions
union_num           <- prop_emi_hh_pos$prop[prop_emi_hh_pos$hh_pos=='union']*num_left 
risk_set            <- pop[pop$sex=='F' & pop$hh_position=='union' & !pop$ID %in% emi_pop,]
risk_set            <- left_join(risk_set, emi_dist_union, by=c('age'))
risk_set$prob[is.na(risk_set$prob)] <- 0
sample_emi_union    <- resamp(risk_set$ID, size=union_num, prob=risk_set$prob, replace=F)
union_sim           <- pop$ID[pop$HH_ID %in% pop$HH_ID[pop$ID %in% sample_emi_union]] 
emi_pop             <- c(emi_pop, union_sim) 

# unions w: child
union_num           <- prop_emi_hh_pos$prop[prop_emi_hh_pos$hh_pos=='union_w_child']*num_left
risk_set            <- pop[pop$sex=='F' & pop$hh_position=='union_w_child' & !pop$ID %in% emi_pop,]
risk_set            <- left_join(risk_set, emi_dist_union_w_child, by=c('age'))
risk_set$prob[is.na(risk_set$prob)] <- 0
sample_emi_union    <- resamp(risk_set$ID, size=union_num, prob=risk_set$prob, replace=F)
union_sim           <- pop$ID[pop$HH_ID %in% pop$HH_ID[pop$ID %in% sample_emi_union]] 
emi_pop             <- c(emi_pop, union_sim)

# Child
union_num           <- prop_emi_hh_pos$prop[prop_emi_hh_pos$hh_pos=='child']*num_left
risk_set            <- pop[pop$hh_position=='child' & pop$age>=16 & !pop$ID %in% emi_pop,]
risk_set            <- left_join(risk_set, emi_dist_child, by=c('age','sex'))
risk_set$prob[is.na(risk_set$prob)] <- 0
sample_emi_union    <- resamp(risk_set$ID, size=union_num, prob=risk_set$prob, replace=F)
union_sim           <- sample_emi_union
emi_pop             <- c(emi_pop, union_sim)


# Single
union_num           <- ceiling(prop_emi_hh_pos$prop[prop_emi_hh_pos$hh_pos=='single']*num_left)
risk_set            <- pop[pop$hh_position=='single' & pop$age>=16 & !pop$ID %in% emi_pop,]
risk_set            <- left_join(risk_set, emi_dist_single, by=c('age','sex'))
risk_set$prob[is.na(risk_set$prob)] <- 0
sample_emi_union    <- resamp(risk_set$ID, size=union_num, prob=risk_set$prob, replace=F)
union_sim           <- sample_emi_union
emi_pop             <- c(emi_pop, union_sim)

# NFRA
union_num           <- prop_emi_hh_pos$prop[prop_emi_hh_pos$hh_pos=='nfra']*num_left
risk_set            <- pop[pop$hh_position=='non_family' & pop$age>=16 & !pop$ID %in% emi_pop & pop$mother==0 & pop$father==0,]
risk_set            <- left_join(risk_set, emi_dist_nfra, by=c('age','sex'))
risk_set$prob[is.na(risk_set$prob)] <- 0
sample_emi_union    <- resamp(risk_set$ID, size=union_num, prob=risk_set$prob, replace=F)
union_sim           <- sample_emi_union
emi_pop             <- c(emi_pop, union_sim)

# Other
union_num           <- prop_emi_hh_pos$prop[prop_emi_hh_pos$hh_pos=='other']*num_left
risk_set            <- pop[pop$hh_position=='other' & pop$age>=16  & !pop$ID %in% emi_pop & pop$mother==0 & pop$father==0,]
risk_set            <- left_join(risk_set, emi_dist_other, by=c('age','sex'))
risk_set$prob[is.na(risk_set$prob)] <- 0
sample_emi_union    <- resamp(risk_set$ID, size=union_num, prob=risk_set$prob, replace=F)
union_sim           <- sample_emi_union
emi_pop             <- c(emi_pop, union_sim)

# Single parent
union_num           <- prop_emi_hh_pos$prop[prop_emi_hh_pos$hh_pos=='single_parent']*num_left
risk_set            <- pop[pop$hh_position=='single_parent' & pop$age>=16  & !pop$ID %in% emi_pop,]
risk_set            <- left_join(risk_set, emi_dist_single_parent, by=c('age','sex'))
risk_set$prob[is.na(risk_set$prob)] <- 0
sample_emi_union    <- resamp(risk_set$ID, size=union_num, prob=risk_set$prob, replace=F)
union_sim           <- pop$ID[pop$HH_ID %in% pop$HH_ID[pop$ID %in% sample_emi_union]] 
emi_pop             <- c(emi_pop, union_sim) 

num_left            <- emi_num-length(emi_pop)

}

# remove individual emigrating if whole household also is emigrating
emi_pop                                        <- unique(emi_pop)
emi_pop                                        <- pop[pop$ID %in% emi_pop, c('ID','age','hh_position','HH_size', 'HH_ID','sex','event_date')]

# Date of emigration
emi_pop$event_date[is.na(emi_pop$event_date)]  <- as.Date('2001-10-01')
prev_event_hh                                  <- emi_pop %>% group_by(HH_ID) %>% slice(which.max(event_date))
emi_hhs                                        <- data.frame(unique(emi_pop$HH_ID))
names(emi_hhs)                                 <- 'HH_ID'
emi_dates                                      <- sample(seq(as.Date(model_param$time_steps_date[i_time_step]), as.Date(end_time_step), by="day"), nrow(emi_hhs), replace = TRUE)
emi_hhs$event_date                             <- emi_dates
emi_hhs$event_type                             <- 'emigration'
emi_hhs                                        <- left_join(emi_hhs, prev_event_hh[,c('HH_ID','event_date')], by='HH_ID')

    # Adjust event_date if before other transition in household
      adj_event_emi                            <- emi_hhs[emi_hhs$event_date.x < emi_hhs$event_date.y,]
      
      if (nrow(adj_event_emi)>0) {
        
      for (i in 1:nrow(adj_event_emi)) {
        
        adj_event_emi$new_event_date[i]        <- resamp(seq(adj_event_emi$event_date.y[i], as.Date(end_time_step), by="day"), 1)
      
      }
      
      adj_event_emi$new_event_date             <- as.Date(adj_event_emi$new_event_date, origin='1970-01-01')
      adj_event_emi                            <- adj_event_emi[order(adj_event_emi$HH_ID),]
      emi_hhs                                  <- emi_hhs[order(emi_hhs$HH_ID),]      
      emi_hhs$event_date.x[emi_hhs$HH_ID %in% adj_event_emi$HH_ID] <- adj_event_emi$new_event_date   
      emi_hhs                                  <- emi_hhs[,-4]
      names(emi_hhs)                           <- c('HH_ID','event_date','event_type')      
      
      }
      
# merge   
emi_pop            <- left_join(emi_pop[,-7], emi_hhs, by='HH_ID')
emi_pop            <- emi_pop[order(emi_pop$ID),]
names(emi_pop)[8]  <- 'event_type'

return(emi_pop)


}


############# Immigration ##################


create_immi_pop <- function(migration_rate, immi_pop_surplus){
  
  
  # Number of immigrants
  immi_rate            <- migration_rate$mig_rate[migration_rate$year == model_param$time_step_year[i_time_step]]
  immi_rate            <- adjust_prob(immi_rate, model_param$t_per_year)
  immi_num             <- round(immi_rate*sum(pop_age_sex[[i_time_step]]$Freq))
  
  
  # create individuals
  age_sex_row          <- sample(age_sex_dist_immi$rownr, immi_num, age_sex_dist_immi$prop, replace=T)
  age_sex_dist_immi$age <- as.numeric(as.character(age_sex_dist_immi$age))
  age_sex              <- age_sex_dist_immi[age_sex_row, c('age','sex')]
  immi_pop             <- data.frame(age = age_sex$age, sex = age_sex$sex)
  immi_pop$age_group   <- findInterval(immi_pop$age, seq(0,open_age_trans, age_int)) 
  immi_pop$age         <- as.numeric(as.character(immi_pop$age))
  immi_pop$hh_position <- 'single'
  immi_pop$union       <- 0
  immi_pop$birth_date  <- paste((as.numeric(model_param$time_step_year[i_time_step]) - immi_pop$age), '-01-01', sep='')
  immi_pop$birth_date  <- as.Date(immi_pop$birth_date)
  
  # add random number of days to birthday to avoid everyone having 1/1-xxxx a birthday
  random_day                                <- sample(0:364, nrow(immi_pop), replace = TRUE)
  immi_pop$birth_date                       <- immi_pop$birth_date + random_day    
  
  # Add age group
  immi_pop$age_group_hh                                               <- findInterval(immi_pop$age, age_cut_hh) 
  immi_pop$age_group_pop                                              <- findInterval(immi_pop$age, age_cut_pop) 
  
  # Add immi-pop surplus from previous time-step
  immi_pop                                                            <- rbind.fill(immi_pop, immi_pop_surplus)
  
  # Add ID and HH_ID
  immi_pop$ID              <- (max(pop$ID)+1):(max(pop$ID)+nrow(immi_pop))
  immi_pop$HH_ID           <- (max(pop$HH_ID)+1):(max(pop$HH_ID)+nrow(immi_pop))
  
  return(immi_pop)
  
}


# create immi unions

create_immi_unions_new_matching <- function(immi_pop){

  immi_pop_leftover<- c()
  
  # females entering union
  age_dist_immi_all_union$Var1        <- as.numeric(as.character(age_dist_immi_all_union$Var1))
  immi_pop                            <- left_join(immi_pop, age_dist_immi_all_union, by=c('age'='Var1', 'sex'='sex'))
  
  immi_pop$random         <- runif(nrow(immi_pop),0,1)
  immi_pop$Freq[is.na(immi_pop$Freq)] <- 0
  immi_sub_pop            <- immi_pop[immi_pop$random<immi_pop$Freq,]
  immi_pop                <- setDT(immi_pop)
  immi_pop                <- immi_pop[, !c('random','Freq','Var2')]
  immi_pop                <- setDF(immi_pop)
  
  # females
  flag_pop_f              <- (immi_pop$ID %in% immi_sub_pop$ID) & immi_pop$sex=='F'
  
  # males
  flag_pop_m              <- (immi_pop$ID %in% immi_sub_pop$ID) & immi_pop$sex=='M'
  
  # select unique [age] combinations
  age_opt_f               <- unique(immi_pop[flag_pop_f, c('age_group')])
  
  
  if (length(age_opt_f)>0){ 
    
    for(i_opt in 1:length(age_opt_f)){
      
      # get indices of the targeted subpopulation    
      flag_subpop <- flag_pop_f & immi_pop$age_group == age_opt_f[i_opt]
      
      # get age specific probability
      prob_vec_age <- partner_age_prob[partner_age_prob$age==age_opt_f[i_opt], c('age_partner','prop')]
      
      # candidate partners
      partner_pop           <- flag_pop_m & immi_pop$sex=='M' & immi_pop$union==0 & immi_pop$age>15
      
      if(sum(partner_pop)<sum(flag_subpop)){
        
        partner_pop         <- immi_pop$sex=='M' & immi_pop$union==0 & immi_pop$age>15
        
      }
      
      if (sum(partner_pop) > 0) {         
         
         candidates            <- immi_pop[partner_pop, c('ID', 'age_group')]
         candidates$prob_age   <- NA
         partner_opt           <- unique(candidates[,c('age_group')])

        for(i_partner in 1:length(partner_opt)){

          # get indices of targeted candidates
          flag_partner <- candidates$age_group==partner_opt[i_partner]

          # get age specific probability
          prob_age                                    <-  prob_vec_age$prop[prob_vec_age$age_partner == partner_opt[i_partner]]
          candidates$prob_age[flag_partner]           <- prob_age
          candidates$prob_age[candidates$prob_age==0] <- 0.0000000000000000000001

        }
        
         candidates$prob         <- as.numeric(candidates$prob_age)
         candidates$prob_union   <- candidates$prob/sum(candidates$prob, na.rm = TRUE)
         
          if (nrow(candidates) > 0) {

          # sample from candidates
          sample_partners   <- resamp(candidates$ID, sum(flag_subpop), prob=candidates$prob_union, replace=F)
         
         # update pop_data (ind. and partner)
         immi_pop$ID_partner[flag_subpop]   <- sample_partners
         
         flag_partner                       <- flag_subpop | (immi_pop$ID %in% sample_partners) 
         partner_f                          <- immi_pop[flag_partner & immi_pop$sex=='M', c('ID', 'age')]
         partner_m                          <- immi_pop[flag_partner & immi_pop$sex=='F', c('HH_ID','ID', 'age', 'ID_partner')]
         merge_partner                      <- merge(partner_f, partner_m, by.x='ID', by.y = 'ID_partner') 
         immi_pop                           <- merge(immi_pop, merge_partner[,c('ID.y','ID', 'age.y','HH_ID')], by.x = 'ID', by.y = 'ID', all.x = T)
         immi_pop                           <- merge(immi_pop, merge_partner[,c('ID.y','age.x')], by.x = 'ID', by.y = 'ID.y', all.x = T)
         immi_pop$ID_partner                <- ifelse(is.na(immi_pop$ID_partner), immi_pop$ID.y, immi_pop$ID_partner)
         immi_pop$age_partner[flag_partner] <- ifelse(is.na(immi_pop$age.y[flag_partner]), immi_pop$age.x[flag_partner], immi_pop$age.y[flag_partner])
         immi_pop$HH_ID.x                   <- ifelse(is.na(immi_pop$HH_ID.y), immi_pop$HH_ID.x, immi_pop$HH_ID.y)
         names(immi_pop)[names(immi_pop)=="HH_ID.x"] <- "HH_ID"
         immi_pop                           <- immi_pop[ , -which(names(immi_pop) %in% c('HH_ID.y','age.y','age.x','ID.y'))]
         immi_pop$hh_position[flag_partner] <- 'union'
         immi_pop$union[flag_partner]       <- 1
         
       } else {
         
         immi_pop_leftover                  <- c(immi_pop_leftover, immi_pop$ID[flag_subpop])  # immigrants to save for next time-step
         
         
       } } }
   
   immi_pop_surplus                    <- immi_pop[(immi_pop$ID %in% immi_pop_leftover),]
   immi_pop                            <- immi_pop[!(immi_pop$ID %in% immi_pop_leftover),]
   
   return(list(immi_pop, immi_pop_surplus))
  
  
  }
}



assign_children <- function(immi_pop, pop){
  
  age_dist_F_immi_unionplus$Var1    <- as.numeric(as.character(age_dist_F_immi_unionplus$Var1))
  unions_HH                         <- immi_pop[!is.na(immi_pop$ID_partner) & immi_pop$sex=='F' & immi_pop$age>16, c('ID','age')] 
  unions_HH                         <- left_join(unions_HH, age_dist_F_immi_unionplus, by=c('age'='Var1'))
  unions_HH$Freq[is.na(unions_HH$Freq)] <- 0
  single_F_HH                       <- immi_pop[immi_pop$hh_position %in% c('single','other','non_family') & immi_pop$sex=='F' & immi_pop$age>17, c('ID','age')]
  single_F_HH                       <- left_join(single_F_HH, age_dist_F_immi_unionplus, by=c('age'='Var1'))
  risk_child                        <- immi_pop[is.na(immi_pop$ID_partner),c('age','ID')]
  risk_child                        <- left_join(risk_child, age_dist_child_immi, by=c('age'='Var2'))
  risk_child$Freq[is.na(risk_child$Freq)] <-0
  risk_child$random                 <- runif(nrow(risk_child), 0, 1)
  children_immi                     <- risk_child[risk_child$random<risk_child$Freq,c('age','ID')]
  children_immi$ID_mother            <- NA
  unions_HH$random                  <- runif(nrow(unions_HH), 0, 1)
  unions                            <- unions_HH[unions_HH$random<unions_HH$Freq & !unions_HH$ID %in% children_immi$ID,c('ID','age')]
  single_F_HH$random                <- runif(nrow(single_F_HH), 0, 1)
  single_F_HH$Freq[is.na(single_F_HH$Freq)] <- 0
  single                            <- single_F_HH[single_F_HH$random<single_F_HH$Freq & !single_F_HH$ID %in% children_immi$ID,]
  single_row                        <- sample(1:nrow(single), size=nrow(single_F_HH)*0.1, replace = F)  
  single                            <- single[single_row,c('ID','age')]
  cand_mothers_all                  <- rbind.fill(unions, single)
  
  # some children should be assigned to native parents
  risk_immi_child_native          <- left_join(children_immi, prop_immi_child_w_native_parents[prop_immi_child_w_native_parents$native_parent==1,], by=c('age'))
  risk_immi_child_native$proportion[is.na(risk_immi_child_native$proportion)] <- 1
  risk_immi_child_native$random   <- runif(nrow(risk_immi_child_native), 0, 1)
  risk_immi_child_native$native   <- as.numeric(risk_immi_child_native$random < risk_immi_child_native$proportion)
  children_immi_native            <- risk_immi_child_native[risk_immi_child_native$native==1,]
  children_immi                   <- children_immi[!children_immi$ID %in% children_immi_native$ID,]
  
  
  ## Assign to immigrant parents
  for (i in unique(children_immi$age)){
    
    age_child <- i 
    flag_child <- children_immi$age == i
    
    # age_group for matching mother
    age_group <- ifelse(age_child %in% c(0:9),'0_9',ifelse(age_child %in% c(10:19), '10_19', '20+'))
    
    
    cand_mothers <- cand_mothers_all[(cand_mothers_all$age-age_child)>14 & (cand_mothers_all$age-age_child) < 45  & !cand_mothers_all$ID %in% children_immi$ID_mother[!is.na(children_immi$ID_mother)], c('ID','age')]  
    
    if (nrow(cand_mothers)<sum(flag_child)){
      
      cand_mothers <- cand_mothers_all[(cand_mothers_all$age-age_child)>14 & (cand_mothers_all$age-age_child) < 45, c('ID','age')]  

      
    }
    
    if (nrow(cand_mothers)<sum(flag_child)){
      
      cand_mothers <- cand_mothers_all[(cand_mothers_all$age-age_child)>14 & !cand_mothers_all$ID %in% children_immi$ID_mother[!is.na(children_immi$ID_mother)], c('ID','age')]  

    }
    
    if (nrow(cand_mothers)<sum(flag_child)){
      
      cand_mothers <- cand_mothers_all[(cand_mothers_all$age-age_child)>14, c('ID','age')]  

    }
    
    if (nrow(cand_mothers)<sum(flag_child)){
      
      children_immi_native  <- rbind.fill(children_immi_native, children_immi[flag_child,])
      children_immi         <- children_immi[!flag_child,]
      
    } else {
    
    cand_mothers$age  <- cand_mothers$age-age_child
    cand_mothers      <- left_join(cand_mothers, mother_child_age_diff[mother_child_age_diff$age_group==age_group,c('Var1','pred')], by=c('age'='Var1'))
    cand_mothers$pred[is.na(cand_mothers$pred) | cand_mothers$pred<0] <- 0.0000000001
    sample_mother     <- resamp(cand_mothers$ID, prob=cand_mothers$pred,  sum(flag_child), replace=F)
    children_immi$ID_mother[flag_child] <- sample_mother

    }
  }
  
  
  ## Assign to native parents
  cand_mothers_native1         <- pop[pop$sex=='F' & pop$hh_position %in% c('single_parent','union_w_child','union','single') & !pop$ID %in% emi_pop$ID, c('ID','HH_ID','age','ID_partner','num_births')]
  cand_mothers_native2         <- c() #pop[pop$ID %in% sample_union & !pop$ID %in% emi_pop$ID, c('ID','HH_ID','age','ID_partner')]
  cand_mothers_native          <- rbind.fill(cand_mothers_native1, cand_mothers_native2)
  
  if (nrow(children_immi_native)){
    
    for (i in unique(children_immi_native$age)){
    
    age_child                         <- i 
    
    # age_group for matching mother
    age_group <- ifelse(age_child %in% c(0:9),'0_9',ifelse(age_child %in% c(10:19), '10_19', '20+'))
    
    flag_child                        <- children_immi_native$age==i
    cand_mothers                      <- cand_mothers_native[(cand_mothers_native$age-age_child)>16 & (cand_mothers_native$age-age_child) < 45 & !cand_mothers_native$ID %in% children_immi_native$ID_mother[!is.na(children_immi_native$ID_mother)], c('ID','num_births','age')]  
    cand_mothers$age                  <- cand_mothers$age-age_child
    cand_mothers                      <- left_join(cand_mothers, mother_child_age_diff[mother_child_age_diff$age_group==age_group,c('Var1','pred')], by=c('age'='Var1'))
    cand_mothers$pred[is.na(cand_mothers$pred)| cand_mothers$pred<0] <- 0.0000001
  
    sample_mother                     <- resamp(cand_mothers$ID,prob=cand_mothers$pred, sum(flag_child))
    
    children_immi_native$ID_mother[flag_child] <- sample_mother
    
  }
  }
  
  assigned_child       <- pop$ID %in% children_immi_native$ID_mother
  children_immi_native <- left_join(children_immi_native, cand_mothers_native[,c('ID','ID_partner','HH_ID')], by=c('ID_mother'='ID'))
  children_immi        <- left_join(children_immi, immi_pop[,c('ID','ID_partner','HH_ID')], by=c('ID_mother'='ID'))
  children_immi        <- rbind.data.frame(children_immi, children_immi_native[,c('age','ID','ID_mother','ID_partner','HH_ID')])
  children_immi        <- children_immi[order(children_immi$ID),]
  immi_pop             <- immi_pop[order(immi_pop$ID),]
  immi_pop[immi_pop$ID %in% children_immi$ID, c('HH_ID','ID_mother','ID_father')] <- children_immi[,c('HH_ID','ID_mother','ID_partner')]

  immi_pop$hh_position[immi_pop$ID %in% children_immi$ID | immi_pop$age < 14 ]    <- 'child'
  immi_pop$child                                <- as.numeric(immi_pop$hh_position=='child')
  
  setDT(immi_pop)[, HH_children := sum(child), by = .(HH_ID)]
  immi_pop$mother                                                        <- as.numeric(immi_pop$ID %in% immi_pop$ID_mother)
  immi_pop$father                                                        <- as.numeric(immi_pop$ID %in% immi_pop$ID_father)
  immi_pop$num_births                                                    <- 0
  immi_pop$num_births[immi_pop$mother==1]                                <- immi_pop$HH_children[immi_pop$mother==1]
  pop$num_births[assigned_child & !is.na(pop$num_births)]                <- pop$num_births[assigned_child & !is.na(pop$num_births)]+1
  pop$num_births[assigned_child & is.na(pop$num_births)]                 <- 1
  pop$HH_size[pop$HH_ID %in% pop$HH_ID[assigned_child]]                  <- pop$HH_size[pop$HH_ID %in% pop$HH_ID[assigned_child]]+1
  pop$hh_position[pop$hh_position=='union' & (assigned_child | pop$ID_partner %in% pop$ID[assigned_child])] <- 'union_w_child'
  pop$hh_position[pop$hh_position=='single' & assigned_child]            <- 'single_parent'
  assigned_child_birth_hist                                              <- pop[assigned_child, c('ID','date_prev_birth')]
  assigned_child_birth_hist                                              <- left_join(assigned_child_birth_hist, children_immi_native[,c('ID','ID_mother')], by=c('ID'='ID_mother'))
  assigned_child_birth_hist                                              <- left_join(assigned_child_birth_hist, immi_pop[,c('ID','birth_date')], by=c('ID.y'='ID'))
  assigned_child_adjust_prev_birth                                       <- assigned_child_birth_hist[assigned_child_birth_hist$date_prev_birth < assigned_child_birth_hist$birth_date | is.na(assigned_child_birth_hist$date_prev_birth),]
  pop                                                                    <- pop[order(pop$ID),]
  assigned_child_adjust_prev_birth                                       <- assigned_child_adjust_prev_birth[order(assigned_child_adjust_prev_birth$ID),]
  pop$date_prev_birth[pop$ID %in% assigned_child_adjust_prev_birth$ID]   <- assigned_child_adjust_prev_birth$birth_date
  
  setDT(immi_pop)[, HH_size := .N, by = .(HH_ID)]
  
  if (sum(immi_pop$hh_position == 'child')>0 ) {
  
  immi_pop$date_prev_birth                      <- NA
  setDT(immi_pop)[, birth := max(birth_date), by = .(HH_ID)]
  
  children_pop                                  <- immi_pop[immi_pop$child==1,]
  children_pop                                  <- children_pop %>% group_by(HH_ID) %>% slice(which.min(birth_date))

  immi_pop                                      <- merge(immi_pop, children_pop[,c('HH_ID', 'birth_date')], by='HH_ID', all.x=TRUE)
  immi_pop$age_first_birth[immi_pop$mother==1]  <- (ymd(immi_pop$birth_date.y[immi_pop$mother==1])-ymd(immi_pop$birth_date.x[immi_pop$mother==1]))/365
  immi_pop$age_first_birth[immi_pop$mother==1]  <- ifelse(immi_pop$age_first_birth[immi_pop$mother==1] < 14, 14, immi_pop$age_first_birth[immi_pop$mother==1])
  immi_pop$date_prev_birth[immi_pop$mother==1]  <- immi_pop$birth[immi_pop$mother==1]
  immi_pop$date_prev_birth                      <-  as.Date(immi_pop$date_prev_birth, origin = '1970-01-01')
  colnames(immi_pop)[which(names(immi_pop) == "birth_date.x")]  <- 'birth_date'
  
  immi_pop                                      <- immi_pop[,-c(22:23)]
  immi_pop$hh_position[immi_pop$mother==1 | immi_pop$father==1] <- 'union_w_child'
  immi_pop$hh_position[(immi_pop$mother==1 | immi_pop$father==1) & is.na(immi_pop$ID_partner)] <- 'single_parent'
  }

  return(list(immi_pop, pop))
  
}



immigrants_nfra_other_collective <- function(immi_pop, median_coll_hh){
  
  
  ### collective
  perc_coll               <- 0.014
  num_coll                <- nrow(immi_pop)*perc_coll
  coll_sample             <- resamp(immi_pop$ID[immi_pop$hh_position=='single' & immi_pop$age>=16], num_coll, replace=F)
  coll_hhs                <- ceiling(length(coll_sample)/50)
  coll_hh_id              <- c((max(pop$HH_ID)+1):((max(pop$HH_ID)+1)+(coll_hhs-1)))
  coll_hh_id              <- rep(coll_hh_id, ceiling(num_coll/coll_hhs))
  flag_coll               <- immi_pop$ID %in% coll_sample
  immi_pop$HH_ID[flag_coll] <- coll_hh_id[1:num_coll]
  immi_pop$hh_position[flag_coll] <- 'collective'
  
  # Add newly created HHs to median_coll_hh
  newly_created           <- immi_pop[flag_coll]
  colnames(newly_created)[1] <- 'new_HH_ID'
  capacity                <- data.frame(table(newly_created$new_HH_ID))
  names(capacity)         <- c('new_HH_ID','capacity')
  capacity$new_HH_ID      <- as.numeric(as.character(capacity$new_HH_ID))
  newly_created           <- left_join(newly_created, capacity, by='new_HH_ID')
  median_new              <- ddply(newly_created, .(new_HH_ID), summarize, median_age=median(age))
  median_new              <- merge(median_new, newly_created[,c('new_HH_ID','capacity')], all.x = TRUE, all.y = FALSE)
  median_new              <- median_new[!duplicated(median_new$new_HH_ID),]
  names(median_new)       <- c('HH_ID','median_age','capacity')
  median_coll_hh          <- rbind.fill(median_coll_hh, median_new)
  
  
  
  ### other
  
  perc_other                      <- 0.06

  # non-family related adults age dist.
  risk_nfra                       <- immi_pop[immi_pop$hh_position=='single', c('age','ID')]
  risk_nfra                       <- left_join(risk_nfra, age_dist_nfra, by=c('age'='Var1'))
  risk_nfra$Freq[is.na(risk_nfra$Freq)] <-0
  risk_nfra$random                <- runif(nrow(risk_nfra), 0, 1)
  sample_nfra_pop                 <- risk_nfra[risk_nfra$random<risk_nfra$Freq,c('ID')]
  
  sample_other_pop                <- resamp(immi_pop$ID[immi_pop$hh_position=='single' & !immi_pop$ID %in% sample_nfra_pop], size=perc_other*nrow(immi_pop), replace=F)  
  
  # Non-family related adult
  flag_nfhh                       <- immi_pop$ID %in% sample_nfra_pop$ID
  nfhh                            <-  unique(pop[pop$hh_position %in% c('union','single_parent','union_w_child'),c('HH_ID','HH_children','HH_size')]) 
  nfhh_HH_children                <-  resamp(non_family_hh_size$Var1, sum(flag_nfhh), prob=non_family_hh_size$Freq, replace = TRUE)
  nfhh_0                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==2], sum(nfhh_HH_children==2), replace = FALSE)
  nfhh_1                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==3], sum(nfhh_HH_children==3), replace = FALSE)
  nfhh_2                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==4], sum(nfhh_HH_children==4), replace = FALSE)
  nfhh_3                          <-  resamp(nfhh$HH_ID[nfhh$HH_size==5], sum(nfhh_HH_children==5), replace = FALSE)
  nfhh_4                          <-  resamp(nfhh$HH_ID[nfhh$HH_size>5], sum(nfhh_HH_children==6), replace = FALSE)
  nfhh_ID                         <- c(nfhh_0, nfhh_1, nfhh_2, nfhh_3, nfhh_4)
  immi_pop$HH_ID[flag_nfhh]       <- nfhh_ID
  immi_pop$hh_position[flag_nfhh] <- 'non_family'
  
  # Other
  flag_other                      <- immi_pop$ID %in% sample_other_pop
  
  if (sum(flag_other)>0){
    
    other_cand                      <- immi_pop[flag_other,]
    other_hh_size                   <- resamp(other_dist$Var1, sum(flag_other), prob=other_dist$Freq, replace=T)
    other_cand$new_hh_size          <- other_hh_size
    
    # if new household size is 2 -> find match
    if (sum(other_cand$new_hh_size==2)>0){
      
      other_hh2                       <- other_cand[other_cand$new_hh_size==2,c('ID')]
      
      if (sum(other_cand$new_hh_size==2)==1){
        
        other_cand$new_hh_size        <- 3
        other_hh2                     <- NULL
        
      } else {
        
        new_HH_ID_hh2                   <- c((max(immi_pop$HH_ID)+1):(max(immi_pop$HH_ID)+floor(nrow(other_hh2)/2)))
        new_HH_ID_hh2                   <- rep(new_HH_ID_hh2, times=2)
        other_hh2                       <- cbind.fill(other_hh2, new_HH_ID_hh2, fill = NA)
        names(other_hh2)                <- c('ID','object')
        
      }} else {
        
        other_hh2                   <- NULL
      } 
    
    other_matched                   <- NULL
    
    for (i in unique(other_cand$new_hh_size[other_cand$new_hh_size!=2])) {
      
      max_id                        <- max(c(other_hh2$object, other_matched$object, pop$HH_ID))
      
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
        
        new_hh_ID1                  <- unique(other_hhs$HH_ID)
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
    
    other_hh_match                   <- other_hh_match[!(is.na(other_hh_match$object) | is.na(other_hh_match$ID)),]
    immi_pop$HH_ID[flag_other]       <- other_hh_match$object
    immi_pop$hh_position[flag_other] <- 'other'

    
  }

  return(list(immi_pop, median_coll_hh))
}
  
  

immigration <- function(migration_rate, pop, i_time_step, immi_pop_surplus, median_coll_hh) {
  
  immi_pop              <- create_immi_pop(migration_rate, immi_pop_surplus)
  immi_unions           <- create_immi_unions_new_matching(immi_pop)
  immi_pop              <- immi_unions[[1]]
  immi_pop_surplus      <- immi_unions[[2]]
  
  if (sum(!is.na(immi_pop$ID_partner)) > 0) {
    
    immi_pop_child      <- assign_children(immi_pop, pop)
    immi_pop            <- immi_pop_child[[1]]
    pop                 <- immi_pop_child[[2]]
    
  } else {
    
    immi_pop_surplus    <- rbind.fill(immi_pop_surplus, immi_pop[immi_pop$age<18,]) # if no parents are found add children to surplus of immigrants for next time-step
    immi_pop            <- immi_pop[!(immi_pop$age < 18),]
  }
  
  immi_other            <- immigrants_nfra_other_collective(immi_pop, median_coll_hh) 
  immi_pop              <- immi_other[[1]]
  median_coll_hh        <- immi_other[[2]]
  immi_pop$immigrant    <- 1
  immi_hhs              <- data.frame(unique(immi_pop$HH_ID))
  names(immi_hhs)       <- 'HH_ID'
  immi_dates            <- sample(seq(as.Date(model_param$time_steps_date[i_time_step]), as.Date(end_time_step), by="day"), nrow(immi_hhs), replace = TRUE)
  immi_hhs$event_date   <- immi_dates
  immi_pop              <- left_join(immi_pop, immi_hhs, by='HH_ID')
  immi_pop$main_parent  <- immi_pop$ID_mother
  immi_pop$birth_date[immi_pop$event_date < immi_pop$birth_date] <- immi_pop$event_date[immi_pop$event_date < immi_pop$birth_date]
  immi_event            <- immi_pop[,c('ID','birth_date','sex','HH_ID','hh_position','ID_partner','ID_father','ID_mother','age_group_pop','age_group_hh', 'event_date')]
  immi_event$event_type <- 'immigration'
  immi_event$event_date <- as.Date(immi_event$event_date, origin='1970-01-01')
  
  
  return(list(immi_pop, immi_event, immi_pop_surplus, pop, median_coll_hh))
  
}




