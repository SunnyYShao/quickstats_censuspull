edu_total_updater <- function(){
  
  geo <- c("state", "county", "congressional district")
  year <- c(2016, 2016, 2016)
  argList <- list(geo, year)
  
  edu_grabber <- function(geo, year){
########################
#categories for edu
edu_cat4 <- c("Total!!Male!!Less than high school diploma", 
              "Total!!Female!!Less than high school diploma", 
              "Total!!Male!!High school graduate (includes equivalency)",
              "Total!!Female!!High school graduate (includes equivalency)",
              "Total!!Male!!Some college or associate's degree",
              "Total!!Female!!Some college or associate's degree",
              "Total!!Male!!Bachelor's degree or higher",
              "Total!!Female!!Bachelor's degree or higher")

#load data for edu
table1 <- get_acs(table = "C15002B", geography = geo, year = year, summary_var = "C15002B_001")
table2 <- get_acs(table = "C15002C", geography = geo, year = year, summary_var = "C15002C_001")
table3 <- get_acs(table = "C15002D", geography = geo, year = year, summary_var = "C15002D_001")
table4 <- get_acs(table = "C15002E", geography = geo, year = year, summary_var = "C15002E_001")
table5 <- get_acs(table = "C15002H", geography = geo, year = year, summary_var = "C15002H_001")
table6 <- get_acs(table = "C15002I", geography = geo, year = year, summary_var = "C15002I_001")
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
  select(NAME,group, variable, estimate,moe, summary_est, summary_moe) %>% 
  left_join(label) %>% select(label, everything()) # join table

tbl <- table %>% 
  filter(label %in% edu_cat4) %>% 
  mutate(topic_type = case_when(
    label=="Total!!Male!!Less than high school diploma" ~"Less than HS",
    label=="Total!!Female!!Less than high school diploma" ~"Less than HS",
    label=="Total!!Male!!High school graduate (includes equivalency)" ~"HS or GED",
    label=="Total!!Female!!High school graduate (includes equivalency)" ~"HS or GED",
    label=="Total!!Male!!Some college or associate's degree" ~"Some College or AA",
    label=="Total!!Female!!Some college or associate's degree" ~"Some College or AA",
    label=="Total!!Male!!Bachelor's degree or higher" ~"BA or higher",
    label=="Total!!Female!!Bachelor's degree or higher" ~"BA or higher")) %>% 
  group_by(NAME, group, topic_type) %>% 
  mutate(estimate = sum(estimate),
         est_moe = sum(moe)) %>% 
  select(NAME, group, topic_type, 
         estimate, est_moe, summary_est, 
         summary_moe) %>% unique() %>% 
  ungroup() %>% 
  mutate(estimate = case_when(
    est_moe <= 0.25*estimate ~estimate,
    TRUE ~NA_real_)) %>% 
  mutate(summary_est = case_when(
    summary_moe <= 0.25*summary_est ~summary_est,
    TRUE ~NA_real_)) %>% 
  mutate(prop = estimate / summary_est) %>% 
  select(-est_moe, -summary_est, -summary_moe)

tbl_count <- tbl %>% select(-prop) %>% 
  mutate(estimate_type = "count")
tbl_prop <- tbl %>% select(-estimate) %>% 
  mutate(estimate_type = "prop") %>% 
  rename(estimate = prop)
tbl_final <- rbind(tbl_count,tbl_prop)
tbl_final <- tbl_final %>% 
  mutate(topic="edu", geo=geo)

return(tbl_final)
  }
  
  final <- pmap_dfr(argList,edu_grabber) # Running pov_grabber
}

