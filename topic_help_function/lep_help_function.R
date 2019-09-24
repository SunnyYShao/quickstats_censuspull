lep_total_updatter <- function(){
  
#first of set up the grabber function
  lep_grabber <- function(geo, year){
    
    #categories for lep
    lep <- c("Total!!Native!!Speak another language!!Speak English less than \"very well\"",
             "Total!!Foreign born!!Speak another language!!Speak English less than \"very well\"")
    
    other_lang <- c("Total!!Native!!Speak another language",
                    "Total!!Foreign born!!Speak another language")
    #load data for lep
    table1 <- get_acs(table = "B16005B", geography = geo, year = year)
    table2 <- get_acs(table = "B16005C", geography = geo, year = year)
    table3 <- get_acs(table = "B16005D", geography = geo, year = year)
    table4 <- get_acs(table = "B16005E", geography = geo, year = year)
    table5 <- get_acs(table = "B16005H", geography = geo, year = year)
    table6 <- get_acs(table = "B16005I", geography = geo, year = year)
    
    #generating group info
    table1 <- table1 %>% mutate(group="African American Alone")
    table2 <- table2 %>% mutate(group="AIAN Alone")
    table3 <- table3 %>% mutate(group="Asian Alone")
    table4 <- table4 %>% mutate(group="NHPI Alone")
    table5 <- table5 %>% mutate(group="Non-Hispanic White Alone")
    table6 <- table6 %>% mutate(group="Latino Alone")
    #merge into one
    table <- rbind(table1, table2, table3, table4, table5, table6)
    table <- table %>% 
      select(NAME, group, variable, estimate,moe) %>% 
      left_join(label) %>% select(label, everything()) # join table
    
    tbl_lep <- table %>%
      filter(label %in% lep) %>% 
      mutate(topic_type = "LEP") %>% 
      group_by(NAME, group, topic_type) %>% 
      mutate(estimate = sum(estimate),
             moe = sum(moe)) %>% 
      select(-label, -variable) %>% unique() %>% 
      ungroup() %>% 
      mutate(estimate = case_when(
        moe <= .25* estimate ~estimate,
        TRUE ~NA_real_))
    
    tbl_lang <- table %>%
      filter(label %in% other_lang) %>% 
      mutate(topic_type = "LEP") %>% 
      group_by(NAME, group, topic_type) %>% 
      mutate(summary_est = sum(estimate),
             summary_moe = sum(moe)) %>% 
      select(-label, -variable, -estimate, -moe) %>% unique() %>% 
      ungroup() %>% 
      mutate(summary_est = case_when(
        summary_moe <= .25* summary_est ~summary_est,
        TRUE ~NA_real_))
    
    tbl <- tbl_lep %>% left_join(tbl_lang) %>% 
      mutate(prop = estimate /summary_est) %>% 
      select(-moe, -summary_moe, -summary_est)
    
    tbl_count <- tbl %>% select(-prop) %>% 
      mutate(estimate_type = "count",
             topic = "Limited English Proficiency",
             geo = geo)
    
    tbl_prop <- tbl %>% select(-estimate) %>% 
      mutate(estimate_type = "prop",
             topic = "Limited English Proficiency",
             geo = geo) %>% 
      rename(estimate = prop)
    
    tbl_final <- rbind(tbl_count, tbl_prop)
    return(tbl_final)
  }
  #list of updates
  geo <- c("state", "county", "congressional district")
  year <- c(2016,2016,2016)
  argList <- list(geo, year)
  
  final <- pmap_dfr(argList, lep_grabber)
  return(final)
}
