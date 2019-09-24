detailed_total_updatter <- function(){
  
  #first of set up the grabber function
  detailed_grabber <- function(geo, year){
    
    #load data
    table1 <- get_acs(table = "B02015", geography = geo, year = year, summary_var = "B02015_001")
    table2 <- get_acs(table = "B02016", geography = geo, year = year, summary_var = "B02016_001")
    table3 <- get_acs(table = "B02018", geography = geo, year = year, summary_var = "B02018_001")
    table4 <- get_acs(table = "B02019", geography = geo, year = year, summary_var = "B02019_001")
    #for detailed group, assign race as group, and ethnic group as topic type
    table1 <- table1 %>% mutate(group ="Asian Alone")
    table2 <- table2 %>% mutate(group="NHPI Alone")
    table3 <- table3 %>% mutate(group="Asian Alone or in Combo")
    table4 <- table4 %>% mutate(group="NHPI Alone or in Combo")
    
    table <- rbind(table1, table2, table3, table4)
    
    table <- table %>% 
      select(NAME, variable, estimate, group, moe, summary_est, summary_moe) %>% 
      left_join(label) %>% select(label, everything()) # join table
    
    detailed_remove <- c("Two or More NHPI", "Other Pacific Islander, not specified (check box only)",
                         "Other Asian, specified", "Other Asian, not specified",
                         "Other Pacific Islander, not specified (check box only)",
                         "Other Pacific Islander, not specified", "Other Pacific Islander, not specified", "Two or more Asian",
                         "Micronesian", "Polynesian")
    
    
    tbl <- table %>% 
      filter(!label %in% c("Total", "Total Groups Tallied")) %>% 
      separate(label, into=c("label", "topic_type"), sep="!!") %>% 
      filter(!topic_type %in% detailed_remove) %>% 
      select(-label) %>% 
      mutate(topic_type = case_when(
        topic_type=="Chinese, except Taiwanese" ~"Chinese",
        TRUE ~topic_type)) %>% 
      mutate(estimate = case_when(
        moe <= .25*estimate ~estimate,
        TRUE ~NA_real_)) %>% 
      mutate(summary_est = case_when(
        summary_moe <= .25*summary_est ~summary_est,
        TRUE ~NA_real_)) %>% 
      mutate(prop = estimate / summary_est,
             topic = "Population by Detailed Groups",
             geo = geo) %>% 
      select(geo, NAME, group, topic, topic_type, estimate, prop)
        
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
  
  final <- pmap_dfr(argList,detailed_grabber) # Running pov_grabber
  return(final)
}