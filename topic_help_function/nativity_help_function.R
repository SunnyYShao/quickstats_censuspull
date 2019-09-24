nativity_total_updatter <- function(){

#first of set up the grabber function
  nativity_grabber <- function(geo, year){
    #categories for fb
    fb <- c("Total!!Male!!Under 18 years!!Foreign born",
            "Total!!Male!!18 years and over!!Foreign born",
            "Total!!Female!!Under 18 years!!Foreign born",
            "Total!!Female!!18 years and over!!Foreign born")
    #load data for nativity
    table1 <- get_acs(table = "B05003B", geography = geo, year = year, summary_var = "B05003B_001")
    table2 <- get_acs(table = "B05003C", geography = geo, year = year, summary_var = "B05003C_001")
    table3 <- get_acs(table = "B05003D", geography = geo, year = year, summary_var = "B05003D_001")
    table4 <- get_acs(table = "B05003E", geography = geo, year = year, summary_var = "B05003E_001")
    table5 <- get_acs(table = "B05003H", geography = geo, year = year, summary_var = "B05003H_001")
    table6 <- get_acs(table = "B05003I", geography = geo, year = year, summary_var = "B05003I_001")
    
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
      select(NAME, group, variable, estimate,moe, summary_est, summary_moe) %>% 
      left_join(label) %>% select(label, everything()) # join table
    
    tbl <- table %>% 
      filter(label %in% fb) %>% 
      mutate(topic_type="Foreign-born") %>%
      group_by(NAME, group, topic_type) %>% 
      mutate(est_fb = sum(estimate),
             est_moe = sum(moe)) %>% 
      select(-label, -variable) %>% unique() %>% 
      ungroup() %>% 
      mutate(est_fb = case_when(
        est_moe <= .25*est_fb ~est_fb,
        TRUE ~NA_real_)) %>% 
      mutate(summary_est = case_when(
        summary_moe <= .25*summary_est ~summary_est,
        TRUE ~NA_real_)) %>% 
      mutate(prop = estimate / summary_est) %>% 
      select(-estimate, -moe, -summary_est, -summary_moe, -est_moe)
    
    tbl_count <- tbl %>% select(-prop) %>% 
      mutate(estimate_type = "count") %>% 
      rename(estimate = est_fb)
    
    tbl_prop <- tbl %>% select(-est_fb) %>% 
      mutate(estimate_type = "prop") %>% 
      rename(estimate = prop)
    
    tbl_final <- rbind(tbl_count, tbl_prop)
    
    tbl_final <- tbl_final %>% 
      mutate(topic = "nativity", geo = geo)
    
    return(tbl_final)
  }
  
  #list of updates
  geo <- c("state", "county", "congressional district")
  year <- c(2016,2016,2016)
  argList <- list(geo, year)
  
final <- pmap_dfr(argList, nativity_grabber)
return(final)
}

