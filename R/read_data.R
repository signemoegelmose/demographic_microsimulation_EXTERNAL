########################################
###              DATA                ###
########################################

### Household transition
load("data/age_hh_pos_male.RData")
trans_dead_spouse <- read_excel("data/trans_dead_spouse.xlsx")
load("data/hh_trans.RData")
load("data/hh_event_rate.RData")
load('data/partner_age_prob.RData') 
load('data/other_dist.RData')
load("data/non_family_hh_children.RData")
load("data/non_family_hh_size.RData")

# adjust for time-step and convert to probability
hh_event_rate$rate      <- hh_event_rate$rate/model_param$t_per_year
hh_event_rate$rate      <- adjust_prob(hh_event_rate$rate, 1)
hh_trans$rate           <- adjust_prob(hh_trans$rate, 1)

### Fertility

# HH trans after birth
load('data/hh_after_birth.RData')

# GAM
load('data/logitgam1.rda')
load('data/logitgam2_union.rda')
load('data/logitgam3_union.rda')
load('data/logitgam4_union.rda')
load('data/logitgam2_single.rda')
load('data/logitgam3_single.rda')
load('data/logitgam4_single.rda')


### Migration
migration_rate <- read_excel("data/migration_rates_new.xlsx")
migration_rate[,-1] <- migration_rate[,-1]/model_param$t_per_year

# Immigration
load("data/age_dist_nfra_immigrants.RData")
load('data/immigrants_2011.RData')
load('data/age_sex_dist_immi.RData')
load("data/age_dist_immi_all_union.RData")
load("data/age_dist_F_immi_unionplus_2011.RData")
load("data/prop_immi_child_w_native_parents.RData")
load('data/mother_child_age_diff_immi.RData')
load("data/age_dist_child_immi_2011.RData")

# Emigration 
load('data/age_dist_emi_ind.RData')
load('data/age_lipro_dist.RData')
load('data/emigrants_2011.RData')
load('data/HH_emig.RData')
load('data/emi_freq.RData')
load("data/emi_dist_other.RData")
load("data/emi_dist_nfra.RData")
load("data/emi_dist_single.RData")
load("data/emi_dist_single_parent.RData")
load("data/emi_dist_union.RData")
load("data/emi_dist_union_w_child.RData")
load("data/emi_dist_child.RData")
load("data/prop_emi_hh_pos.RData")

### Mortality
load("data/mort_adj.RData")
mortality_rates_female <- read_excel("data/death_prob_female.xlsx")
mortality_rates_male   <- read_excel("data/death_prob_male.xlsx")

# mortality probs to mortality rates (mx to qx) and adjusted to t_per_year
death_probs_female <- cbind(mortality_rates_female[,1], lapply(mortality_rates_female[,-1]/model_param$t_per_year, qx2mx))
death_probs_male   <- cbind(mortality_rates_male[,1], lapply(mortality_rates_male[,-1]/model_param$t_per_year, qx2mx))

# reshape files
surv_prob                                 <- rbind(death_probs_female, death_probs_male)
surv_prob$sex                             <- c(rep(2, nrow(death_probs_female)),rep(1, nrow(death_probs_male)))
