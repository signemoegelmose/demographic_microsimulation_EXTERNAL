
  #################################
  ##     LOAD FUNCTIONS          ##
  #################################
  
  source('R/load_packages.R')
  source('R/get_model_parameters.R')
  source('R/fertility.R')
  source('R/functions.R') 
  source('R/update_households_all_transitions.R')  
  source('R/update_households_after_birth.R')
  source('R/mortality.R')
  source('R/migration.R')
  source('R/collective_hh_capacity.R')



  #################################
  ##       LOAD PACKAGES         ##
  #################################

  # Load (and install) all required R packages
  load_packages()


  #################################
  ##     RUN CONFIGURATION       ##
  #################################
  ASFR_list                <- list()
  ASFR_parity_list         <- list()
  mothers_birth_int_list   <- list()
  mothers_info_list        <- list()
  pop_growth_list          <- list()
  pop_list                 <- list()

  random_vec               <- c(2424,4643,25377)                       # seeds
   
  for (i_random in random_vec){

    for (i_sim_year in c(2011)){                              

      start_date           <- paste(i_sim_year,'-01-01', sep='')       # The starting date of the simulation
      end_date             <- paste(2050,'-12-31', sep='')             # The end date of the simulation
      run_config           <- data.frame(run_tag         = '',         
                              time_unit         = 'year',              # Time unit
                              rng_seed          = i_random,            # Seed for the random number generator
                              stringsAsFactors  = F)                   # To prevent the conversion of strings into factors

      
  #######################################
  ## SET RANDM NUMBDER GENERATOR SEED  ##
  #######################################

  # to make sure the results can be reproduced
  set.seed(run_config$rng_seed)


  #####################################
  ## LOAD MODEL PARAMETERS AND DATA  ##
  #####################################

  # load model paramters
  model_param             <- get_model_parameters(run_config)

     
  # load data
  source('R/read_data.R')                               # Fertility rates, mortality rates etc.

  # Variables
  open_age_trans           <- 75
  open_age_trans_rates     <- 85
  age_int                  <- 2
  union_age_lim            <- 16                       # Minimum age for entering a union
  child_age_lim            <- 16                             
   
 
  #################################
  ## CREATE INITIAL POPULATION   ##
  #################################
   
  # Pop to analyse
  load("sample_pop.RData")
  setDT(pop)[, HH_size := .N, by = .(HH_ID)]
  pop <- setDF(pop)

  # Nursing home (collective home) dummy
  pop$NH <- as.numeric(pop$hh_position == 'collective')
  
  # Age (based on birth date)
  pop$age <- floor(time_length(interval(as.Date(pop$birth_date), as.Date("2011-01-01")),"years"))
  
  # If parent ID is not in pop -> set NA
   pop$ID_mother[!pop$ID_mother %in% pop$ID & !is.na(pop$ID_mother)] <- NA
   pop$ID_father[!pop$ID_father %in% pop$ID & !is.na(pop$ID_father)] <- NA

  # Collective households capacity and median age
  median_coll_hh       <- collective_hh_capacity(pop)
  collective_age_size  <- collective_age_size_dist(pop)
   
  # Add age group
  age_cut_hh                                                     <- c(0,13,19,61)
  age_cut_pop                                                    <- c(0,13,19,61)
  pop$age_group_hh                                               <- findInterval(pop$age, age_cut_hh)
  pop$age_group_pop                                              <- findInterval(pop$age, age_cut_pop)
  pop$age_group                                                  <- findInterval(pop$age, seq(0,open_age_trans, age_int))

  #################################
  ##    Prepare output files     ##
  #################################

  # Files to generate output
  source('R/output.R')

  # Output lists
  immi_pop_surplus          <- c()
  mothers_age_parity_status <- list()
  mother_trans_list         <- list()

  # Prepare log file

  if (file.exists('event_log.csv')) {

    file.remove('event_log.csv')

  }

  
  #################################
  ##        Simulation           ##
  #################################

  # The simulation contains the following steps:
  #
  # (i)   Households transitions
  # (ii)  Fertility
  # (iii) Emigration
  # (iv)  Immigration
  # (v)   Mortality
  #
  # These steps are repeated for each time-step.

  
for (i_time_step in model_param$time_steps_int) {   

    # Date of end of time-step
    end_time_step <- end_interval(model_param$time_steps_date, model_param$time_unit)

    # (i) Births
    pop$transition                   <- NA
    pop$event_date                   <- NA
    new_births                       <- births(pop, i_time_step, fertility_age)   # Function from 'Fertility.R'
    birth_events                     <- new_births[[1]]
    mothers_info                     <- new_births[[2]]


    if (length(birth_events) > 0) {

      update_info                    <- update_mother(pop, birth_events, i_time_step) # Information for mothers is updated -> 'Fertility.R' line 50
      pop                            <- update_info[[1]]
      mother_trans                   <- update_info[[2]]
      twins_mothers                  <- update_info[[3]]
      triplets_mothers               <- update_info[[4]]
      record_match                   <- update_info[[5]]
      pop                            <- update_multi_gen(pop)
      births_new                     <- generate_children(pop, mother_trans, twins_mothers, triplets_mothers)                        # Newborns are generated and added to population -> 'Fertility.R' line 83
      newborns                       <- births_new[[1]]
      fertility_events               <- births_new[[2]]
      pop                            <- rbind.fill(pop, newborns)
      pop                            <- update_hh_newborns(pop, birth_events)     # Update households with newborns -> 'Fertility.R' line 107
      pop                            <- update_HH_comp(pop)                       # Necessary to update all affected households and not just the one of the mother

    }

    print('fertility updated')
 
     
    # (ii) Household (LIPRO) transitions
    pop_trans                      <- hh_transition(pop, median_coll_hh)          # Function from 'update_households.R' line 306
    pop                            <- pop_trans[[1]]                              # Population after households are updated
    
    print('Transitions updated')

    # (iii) Emigration

    emi_pop                        <- emigration(pop)                         
    emi_events                     <- emi_pop[,which(names(emi_pop) %in% c('ID', 'HH_ID','event_date','event_type'))]
    emigration_output              <- emigration_output_func(emi_pop, i_time_step)
    age_sex_dist_emi               <- emigration_output[[1]]
    hh_dist_emi                    <- emigration_output[[2]]
    emigrant_pop                   <- emigration_output[[3]]
    pop                            <- pop[order(pop$ID),]
    emi_pop                        <- emi_pop[order(emi_pop$ID),]
    pop$event_date[pop$ID %in% emi_pop$ID] <- emi_pop$event_date

    print('emigration updated')

    # (iv) Immigration

    immi                           <- immigration(migration_rate, pop, i_time_step, immi_pop_surplus, median_coll_hh)
    immi_pop                       <- immi[[1]]
    immi_events                    <- immi[[2]]
    immi_pop_surplus               <- immi[[3]]
    pop                            <- immi[[4]]
    median_coll_hh                 <- immi[[5]]
    immigrant_pop                  <- immigration_output_func(immi_pop, i_time_step)
    pop                            <- rbind.fill(pop, immi_pop)

    # Births of immigrants
    new_births                     <- births_immi(pop, i_time_step, fertility_age)   # Function from 'Fertility.R'
    birth_events_immi              <- new_births[[1]]
    mothers_info_immi              <- new_births[[2]]
    mothers_info                   <- rbind.data.frame(mothers_info, mothers_info_immi)
    birth_events                   <- c(birth_events, birth_events_immi)
    mothers_info_list[[i_time_step]]<- mothers_info

    if (length(birth_events_immi) > 0) {

      update_info                    <- update_mother_immi(pop, birth_events_immi, i_time_step, immi_events) 
      pop                            <- update_info[[1]]
      pop                            <- update_multi_gen_immi(pop)
      births_new                     <- generate_children_immi(pop, mother_trans_immi, twins_mothers, triplets_mothers)                        
      newborns                       <- births_new[[1]]
      fertility_events_immi          <- births_new[[2]]
      fertility_events               <- rbind.fill(fertility_events, fertility_events_immi)
      pop                            <- rbind.fill(pop, newborns)
      pop                            <- update_hh_newborns_immi(pop, birth_events_immi) 
      pop                            <- update_HH_comp(pop)  
      
      }

    print('immigration updated')

    # (v) Deaths
    deaths                              <- mortality(pop, i_time_step)    # Function from 'mortality.R'
    deaths_age_sex[[i_time_step]]       <- data.frame(table(pop$age[pop$ID %in% deaths$ID], pop$sex[pop$ID %in% deaths$ID]))
    deaths_age_sex_hh[[i_time_step]]    <- data.frame(table(pop$age[pop$ID %in% deaths$ID], pop$hh_position[pop$ID %in% deaths$ID]))
    print('mortality updated')
     
    # Update population
    remove_ind_mort_emi                 <- remove_ind(pop, deaths, emi_pop, median_coll_hh)     
    pop                                 <- remove_ind_mort_emi[[1]]
    hh_trans_after_mort                 <- remove_ind_mort_emi[[2]]
    median_coll_hh                      <- remove_ind_mort_emi[[3]]
    pop                                 <- update_pop(pop)                                     
    pop                                 <- update_HH_comp(pop)                                

    # Descriptive
    pop_descr                           <- pop_dist_output(pop, i_time_step)
    pop_age_sex                         <- pop_descr[[1]]

    # Event-log
    event_log                           <- data.frame(matrix(ncol=19, nrow=0))
    names(event_log)                    <- c("ID", "HH_ID", "hh_position","ID_partner","HH_ID_target", "ID_partner_target", "hh_position_target", "event_type", "ID_mother",
                                             "ID_father", "sex", "birth_date", "mother", "father", "age_group_hh",'age_group_pop',"event_date",'age' , "NH")
    event_log$sex                       <- as.character(event_log$sex)
    event_log$event_date                <- as.Date(event_log$event_date)
    event_log                           <- rbind.fill(event_log, immi_events, fertility_events, household_events, emi_events, deaths, hh_trans_after_mort, birthday_events)
    event_log                           <- event_log[order(event_log$event_date),]
    event_log[,c('event_date','birth_date')] <- lapply(event_log[,c('event_date','birth_date')], as.Date, origin='1970-01-01')
    event_log$event_date                <- as.Date(event_log$event_date)

    if ('event_date.x' %in% colnames(event_log)) {

      event_log       <- event_log[,-which(names(event_log) == 'event_date.x')]
    
      }

    write.table(event_log, 'event_log.csv', sep = ",", col.names = !file.exists('event_log.csv'), append = T, row.names = FALSE)


    # Save population (ID, HH_ID, age, sex)
    if(i_time_step < length(model_param$time_steps_int)) {

        fertility_output                 <- fertility_output_func(pop, birth_events, i_time_step) # Function from 'output.R' line 10
        ASFR                             <- fertility_output[[1]]
        birth_age                        <- fertility_output[[2]]
        ASFR_parity                      <- fertility_output[[3]]
        mortality_output                 <- mortality_output_func(pop, i_time_step)
      
      }

    pop_growth                           <- growth_output()                                        # Function from 'output.R' line 119
    death_growth                         <- pop_growth[[2]]
    birth_growth                         <- pop_growth[[1]]
    nat_growth                           <- pop_growth[[3]]
    overall_growth                       <- pop_growth[[5]]
    pop_composition                      <- pop_comp(pop, i_time_step)


    print('pop saved')
    
    }
  }
}
