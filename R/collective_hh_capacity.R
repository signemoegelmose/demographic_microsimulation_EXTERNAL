### Capacity of collective-households in census 2011

collective_hh_capacity <- function(pop){
  
  df                      <- pop[pop$hh_position=='collective', c('HH_ID','age','HH_size')]
  median_coll_hh          <- ddply(df, .(HH_ID), summarize, median_age=median(age))
  median_coll_hh          <- merge(median_coll_hh, df[,c('HH_ID','HH_size')], all.x = FALSE, all.y = FALSE)
  median_coll_hh          <- median_coll_hh[!duplicated(median_coll_hh$HH_ID),]
  median_coll_hh$capacity <- NA
  median_coll_hh$capacity[is.na(median_coll_hh$capacity)]  <- median_coll_hh$HH_size[is.na(median_coll_hh$capacity)] + ceiling(0.15*median_coll_hh$HH_size[is.na(median_coll_hh$capacity)])
  median_coll_hh$capacity[median_coll_hh$HH_size<10]       <- median_coll_hh$HH_size[median_coll_hh$HH_size<10]
  
  return(median_coll_hh)
  
}


collective_age_size_dist <- function(pop){

  load("data/collective_dist.RData")
  # Age groups: 0-29, 30-59, 60+
  # HH size: c(0,10,50,100)
  
  collective_age_size        <- collective_dist
  names(collective_age_size) <- c('age_group','hh_size','prop')

  return(collective_age_size)
  
}


