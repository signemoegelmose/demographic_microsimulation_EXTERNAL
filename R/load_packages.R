
#### Packages ####

load_packages <- function()
{
  all_packages <- c('readxl','readr','tidyr','lifecontingencies','expss',
                    'lubridate','plyr','dplyr','timeDate', 'foreign', 'rowr',
                    'nnet', 'rapportools','pastecs','fmsb','reshape2', 'timeDate',
                    'foreach', 'rlist','maditr','fmsb','gtools','timeDate','plyr',
                    'lubridate','data.table','mgcv')
  
  # loop over the packages
  for(package_i in all_packages){
    
    # if not present => install
    if(!package_i %in% rownames(installed.packages())){
      install.packages(package_i)
    }
    
    # load package
    library(package_i, 
            character.only=TRUE, 
            quietly = TRUE, 
            warn.conflicts = FALSE,
            verbose = FALSE)
  } # end for-loop
  
} # end function


