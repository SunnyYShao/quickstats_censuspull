pop_total_updatter <- function(){

  #first of set up the grabber function
  pop_grabber <- function(geo, year){
    
    pop <- c("B02001_005", "B02001_006", "B03002_003",
             "B02001_003", "B03002_012", "B02011_001", 
             "B02012_001")  
    
    #load data for variables
  table <- get_acs(variables = pop, geography = geo, year = year, summary_var = "B02001_001")
  
  table <- table %>% 
    mutate(group = case_when(
      variable=="B02001_005" ~"Asian Alone",
      variable=="B02001_006" ~"NHPI Alone",
      variable=="B03002_003" ~"Non-Hispanic White Alone",
      variable=="B02001_003" ~"African American Alone",
      variable=="B03002_012" ~"Latino Alone",
      variable=="B02011_001" ~"Asian Alone or in Combo",
      variable=="B02012_001" ~"NHPI Alone or in Combo")) %>% 
    select(NAME, group, variable, estimate, moe, summary_est, summary_moe) %>% 
    left_join(label) %>% select(label, everything()) # join table
  
  table[is.na(table)] <- 0
  
  tbl <- table %>% 
    mutate(estimate = case_when(
      moe <= .25*estimate ~estimate,
      TRUE ~NA_real_)) %>% 
    mutate(summary_est = case_when(
      summary_moe <= .25*summary_est ~summary_est,
      TRUE ~NA_real_)) %>% 
    mutate(prop = estimate / summary_est,
           topic_type = "pop",
           topic = "Population by Race",
           geo = geo) %>% 
    select(-moe, -variable, -summary_est, -summary_moe, -label)
  
  tbl_count <- tbl %>% select(-prop) %>% 
    mutate(estimate_type = "count")
    
  
  tbl_prop <- tbl %>% select(-estimate) %>% 
    mutate(estimate_type = "prop") %>% 
    rename(estimate = prop)
  
  tbl_final <- rbind(tbl_count, tbl_prop)
  return(tbl_final)
  }
  
  geo <- c("state", "county", "congressional district")
  year <- c(2016, 2016, 2016)
  argList <- list(geo, year)
  
  final <- pmap_dfr(argList,pop_grabber) # Running pov_grabber
}