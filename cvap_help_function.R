
cvap_total_updatter <- function(geo){
  if(geo=="state"){
    dta <- read.csv("cvap_raw_data/State.csv")
    dta_cvap <- dta %>% 
      select(GEONAME, LNTITLE, LNNUMBER, TOT_EST, TOT_MOE, CVAP_EST, CVAP_MOE) %>% 
      mutate(TOT_EST = case_when(
        TOT_MOE < .25*TOT_EST ~TOT_EST,
      TRUE ~NA_real_)) %>%
      mutate(CVAP_EST = case_when(
        CVAP_MOE < .25*CVAP_EST ~CVAP_EST,
        TRUE ~NA_real_)) %>% 
      mutate(prop = CVAP_EST / TOT_EST) %>% 
      mutate(group = case_when(
        LNNUMBER
      ))
      
      
  } else if(geo=="county"{
    dta <- read.csv("County.csv")
    ...
  }else if { geo=="district"){
    dta <- read.csv("CD.csv")
    ...
  }else{}
  return(cvap_cleanup)
  }