#############################################
###               OUTPUT                  ###
#############################################

### Fertility

ASFR               <- c(13:51)        
birth_age          <- list()
ASFR_parity        <- data.frame(cbind(rep(c(14:50),12),rep(c(1:12),each=37)))
names(ASFR_parity) <- c('Var1','Var2')

fertility_output_func <- function(pop, birth_events, i_time_step){
  
  # Age-specific rate
    pop_at_risk                   <- pop_age_sex[[i_time_step]][pop_age_sex[[i_time_step]]$Var1 %in% c(13:51) & pop_age_sex[[i_time_step]]$Var2 =='F',c('Var1','Freq')]
    pop_at_risk$Freq2             <- pop_age_sex[[i_time_step+1]][pop_age_sex[[i_time_step+1]]$Var1 %in% c(13:51) & pop_age_sex[[i_time_step+1]]$Var2 =='F',c('Freq')]
    pop_at_risk$avg_pop           <- (pop_at_risk$Freq+pop_at_risk$Freq2)/2
    mothers_age_risk              <- merge(as.data.frame(mothers_age), pop_at_risk, by='Var1', all = T)
    mothers_age_risk              <- na.replace(mothers_age_risk, 0)
    mothers_age_risk$Var1         <- as.numeric(as.character(mothers_age_risk$Var1))
    mothers_age_risk              <- mothers_age_risk[order(mothers_age_risk$Var1),]
    mothers_age_risk$rate         <- mothers_age_risk$Freq.x/mothers_age_risk$avg_pop
    ASFR                          <- cbind(ASFR, mothers_age_risk$rate)
    colnames(ASFR)[ncol(ASFR)]    <- model_param$time_step_year[i_time_step]
    birth_age[[i_time_step]]      <- mothers_age
    
    # age-order specific rate
    mothers_age_risk              <- merge(mothers_parity, pop_at_risk, by='Var1', all = T)
    mothers_age_risk              <- na.replace(mothers_age_risk, 0)
    mothers_age_risk$Var1         <- as.numeric(as.character(mothers_age_risk$Var1))
    mothers_age_risk              <- mothers_age_risk[order(mothers_age_risk$Var1),]
    mothers_age_risk$rate         <- mothers_age_risk$Freq.x/mothers_age_risk$avg_pop
    mothers_age_risk              <- mothers_age_risk[order(mothers_age_risk$Var2, mothers_age_risk$Var1),]
    ASFR_parity                   <- merge(ASFR_parity, mothers_age_risk[,-c(3:6)], by=c('Var1','Var2'), all.x=TRUE)
    ASFR_parity                   <- na.replace(ASFR_parity, 0)
    ASFR_parity                   <- ASFR_parity[order(ASFR_parity$Var2, ASFR_parity$Var1),]
    colnames(ASFR_parity)[ncol(ASFR_parity)]    <- model_param$time_step_year[i_time_step]

    return(list(ASFR, birth_age, ASFR_parity))
}


### Emigration

age_sex_dist_emi                    <- list()
hh_dist_emi                         <- list()
emigrant_pop                        <- list()

emigration_output_func <- function(emi_pop, i_time_step){
  
  # Age distribution
    age_sex_dist_emi[[i_time_step]]     <- data.frame(table(emi_pop$sex, emi_pop$age))          #Var1 = sex, Var2 = age
    hh_dist_emi[[i_time_step]]          <- data.frame(table(emi_pop$hh_position)/nrow(emi_pop)) #Var1=HH-position
    emigrant_pop[[i_time_step]]         <- emi_pop
    return(list(age_sex_dist_emi, hh_dist_emi, emigrant_pop))
   
 }


### Immigration

immigrant_pop <- list()

immigration_output_func <- function(immi_pop, i_time_step){
  
  immigrant_pop[[i_time_step]] <- immi_pop

  return(immigrant_pop)
}
  

### Mortality

Var1               <- rep(c(0:120),2)
Var2               <- c(rep(1,121), rep(2,121))
pop_at_risk        <- data.frame(as.factor(Var1), as.factor(Var2))
names(pop_at_risk) <- c('Var1','Var2')
mortality_output   <- pop_at_risk
deaths_age_sex     <- list() 
deaths_age_sex_hh  <- list()

mortality_output_func <- function(pop, i_time_step){
  
  risk_pop1                         <- pop_age_sex[[i_time_step]]
  risk_pop2                         <- pop_age_sex[[i_time_step+1]]
  colnames(risk_pop2)[3]            <- c('Freq2')
  Var1                              <- rep(c(0:120),2)
  Var2                              <- c(rep(1,121), rep(2,121))
  pop_at_risk                       <- data.frame(as.factor(Var1), as.factor(Var2))
  names(pop_at_risk)                <- c('Var1','Var2')
  pop_at_risk                       <- merge(pop_at_risk, risk_pop1, all.x=TRUE, by = c('Var1', 'Var2'))
  pop_at_risk                       <- merge(pop_at_risk, risk_pop2, all.x=TRUE, by = c('Var1', 'Var2'))
  pop_at_risk$avg_pop               <- (pop_at_risk$Freq+pop_at_risk$Freq2)/2
  deaths_age_sex                    <- merge(deaths_age_sex[[i_time_step]], pop_at_risk, by=c('Var1','Var2'), all.y = T)
  deaths_age_sex                    <- na.replace(deaths_age_sex, 0)
  deaths_age_sex$Var1               <- as.numeric(as.character(deaths_age_sex$Var1))
  deaths_age_sex                    <- deaths_age_sex[order(deaths_age_sex$Var2, deaths_age_sex$Var1),]
  rate                              <- ifelse(deaths_age_sex$avg_pop!=0, deaths_age_sex$Freq.x/deaths_age_sex$avg_pop,0)
  
  mortality_output                  <- cbind(mortality_output, rate)
  
  return(mortality_output)
}


### Population size (end of time-step)

# Starting pop
pop_age_sex     <- list()
pop_hh_position <- list()
pop_parity_age  <- list()

pop_age_sex[[1]]     <- data.frame(table(pop$age, pop$sex))         
pop_hh_position[[1]] <- data.frame(table(pop$hh_position, pop$age))
pop_parity_age[[1]] <- data.frame(table(pop$age[pop$sex=='F'], pop$num_births[pop$sex=='F']))

# Add over time
pop_dist_output <- function(pop, i_time_step){
  
  pop_age_sex[[i_time_step+1]]     <- data.frame(table(pop$age, pop$sex))  

  pop_hh_position[[i_time_step+1]] <- data.frame(table(pop$hh_position, pop$age))
  
  pop_parity_age[[i_time_step+1]] <- data.frame(table(pop$age[pop$sex=='F'], pop$num_births[pop$sex=='F']))
  
  return(list(pop_age_sex, pop_hh_position, pop_parity_age))
}


# Population growth
birth_growth   <- NULL
death_growth   <- NULL
nat_growth     <- NULL
immi_growth    <- NULL
overall_growth <- NULL


growth_output <- function(){

birth_rate     <- length(birth_events)/sum(pop_age_sex[[i_time_step]]$Freq)
birth_growth   <- cbind(birth_growth, birth_rate)
death_rate     <- nrow(deaths)/sum(pop_age_sex[[i_time_step]]$Freq)
death_growth   <- cbind(death_growth, death_rate)
growth         <- (length(birth_events)-nrow(deaths))/sum(pop_age_sex[[i_time_step]]$Freq)
nat_growth     <- cbind(nat_growth, growth)
immig_rate     <- nrow(immi_pop)/sum(pop_age_sex[[i_time_step]]$Freq)
immi_growth    <- cbind(immi_growth, immig_rate)
growth_rate    <- (nrow(pop)-sum(pop_age_sex[[i_time_step]]$Freq))/nrow(pop)
overall_growth <- cbind(overall_growth, growth_rate)

return(list(birth_growth, death_growth, nat_growth, immi_growth, overall_growth))

}


# Save ID, HH_ID, age, sex (for disease simulation)

pop_composition <- list()
pop_composition[[1]] <- pop[,c('ID','HH_ID','age','sex','hh_position', 'HH_size', 'HH_children', 'disease_state','inf_time', 'random_inf','enter_pop','child18_hh','ID_mother','ID_father','ID_partner','num_births','birth_date')]

pop_comp <- function(pop, i_time_step){
  
  pop_composition[[i_time_step + 1]] <- pop[,c('ID','HH_ID','age','sex','hh_position', 'HH_size', 'HH_children', 'disease_state','inf_time','random_inf','enter_pop','child18_hh','ID_mother','ID_father','ID_partner','num_births','birth_date')]
  
  return(pop_composition)
}


### Pop trans
pop_transitions <- list()

### Record match

partner_match   <- list()
