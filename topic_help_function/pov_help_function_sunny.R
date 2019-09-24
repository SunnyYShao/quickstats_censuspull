pov_total_updatter <- function(){
  
  #first of set up the grabber function
 poverty_grabber <- function(geo, year){
   
   #load data for pov
   table1 <- get_acs(variables = "B17001B_002", geography = geo, year = year, summary_var = "B17001B_001")
   table2 <- get_acs(variables = "B17001C_002", geography = geo, year = year, summary_var = "B17001C_001")
   table3 <- get_acs(variables = "B17001D_002", geography = geo, year = year, summary_var = "B17001D_001")
   table4 <- get_acs(variables = "B17001E_002", geography = geo, year = year, summary_var = "B17001E_001")
   table5 <- get_acs(variables = "B17001H_002", geography = geo, year = year, summary_var = "B17001H_001")
   table6 <- get_acs(variables = "B17001I_002", geography = geo, year = year, summary_var = "B17001I_001")
   
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
     left_join(label) %>% select(label, everything())# join table
   
   tbl <- table %>% 
     mutate(topic_type="Below Poverty") %>%
     mutate(estimate = case_when(
       moe <= .25*estimate ~estimate,
       TRUE ~NA_real_)) %>% 
     mutate(summary_est = case_when(
       summary_moe <= .25*summary_est ~summary_est,
       TRUE ~NA_real_)) %>% 
     mutate(prop = estimate / summary_est) %>% 
     select(NAME, group, topic_type, estimate, prop)
   
   tbl_count <- tbl %>% select(-prop) %>% 
     mutate(estimate_type = "count")
   
   tbl_prop <- tbl %>% select(-estimate) %>% 
     mutate(estimate_type = "prop") %>% 
     rename(estimate = prop)
   
   tbl_final <- rbind(tbl_count, tbl_prop)
   
   tbl_final <- tbl_final %>% 
     mutate(topic = "poverty", geo = geo)
   
   return(tbl_final)
   
 }
 
 #list of updates
 geo <- c("state", "county", "congressional district")
 year <- c(2016,2016,2016)
 argList <- list(geo, year)
 
 final <- pmap_dfr(argList, poverty_grabber)
 return(final)
 
 
}