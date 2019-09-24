ins_total_updatter <- function(){
  
  ins_grabber <- function(geo, year){
    
    #AAPI without Insurance
    blk_no_ins <- c("C27001B_004", "C27001B_007","C27001B_010")
    aian_no_ins <- c("C27001C_004", "C27001C_007","C27001C_010")
    aa_no_ins <- c("C27001D_004", "C27001D_007","C27001D_010")
    pi_no_ins <- c("C27001E_004", "C27001E_007","C27001E_010")
    wh_no_ins <- c("C27001H_004", "C27001H_007","C27001H_010")
    lax_no_ins <- c("C27001I_004", "C27001I_007","C27001I_010")
    
    
    
    #load data for insurance
    table1 <- get_acs(variables = blk_no_ins, geography = geo, year = year, summary_var = "C27001B_001")
    table2 <- get_acs(variables = aian_no_ins, geography = geo, year = year, summary_var = "C27001C_001")
    table3 <- get_acs(variables = aa_no_ins, geography = geo, year = year, summary_var = "C27001D_001")
    table4 <- get_acs(variables = pi_no_ins, geography = geo, year = year, summary_var = "C27001E_001")
    table5 <- get_acs(variables = wh_no_ins, geography = geo, year = year, summary_var = "C27001H_001")
    table6 <- get_acs(variables = lax_no_ins, geography = geo, year = year, summary_var = "C27001I_001")
    
    #generating group info
    table1 <- table1 %>% mutate(group="African American Alone")
    table2 <- table2 %>% mutate(group="AIAN Alone")
    table3 <- table3 %>% mutate(group="Asian Alone")
    table4 <- table4 %>% mutate(group="NHPI Alone")
    table5 <- table5 %>% mutate(group="Non-Hispanic White Alone")
    table6 <- table6 %>% mutate(group="Latino Alone")
    
    #merge into one
    table <- rbind(table1, table2, table3, table4, table5, table6)
    
    tbl <- table %>% 
      mutate(estimate = case_when(
        moe <= .25* estimate ~estimate,
        TRUE ~NA_real_)) %>% 
      mutate(summary_est = case_when(
        summary_moe <= .25*summary_est ~summary_est,
      TRUE ~NA_real_)) %>% 
      mutate( prop = estimate / summary_est,
              topic_type = "without health insurance") %>% 
      select(NAME, group, topic_type, estimate, prop)
    
    tbl_count <- tbl %>% select(-prop) %>% 
      mutate(estimate_type = "count",
             topic = "Health Insurance",
             geo = geo)
    
    tbl_prop <- tbl %>% select(-estimate) %>% 
      mutate(estimate_type = "prop",
             topic = "Health Insurance",
             geo = geo) %>% 
      rename(estimate = prop)
    
    tbl_final <- rbind(tbl_count, tbl_prop)
    return(tbl_final)
      
  }
  
  #list of updates
  geo <- c("state", "county", "congressional district")
  year <- c(2016,2016,2016)
  argList <- list(geo, year)
  
  final <- pmap_dfr(argList, ins_grabber)
  return(final)
}