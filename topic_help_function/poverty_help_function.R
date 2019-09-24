
poverty_total_updater <- function(){
  
  # Definitions -------------------------------------------------------------

  pov_vars <- c("B17001D_002","B17001E_002","B17001B_002","B17001I_002","B17001H_002") 
  totals <- c("B17001D_001","B17001E_001","B17001B_001","B17001I_001","B17001H_001")
  groups <- c("Asian Alone", "NHPI ALone","Black Alone", "Hispanic Alone", "White Alone")
  year <- c("2016", "2016", "2016", "2016", "2016")
  argList <-  list(pov_vars,totals,groups, year)     


  pov_grabber <- function(pov_vars, totals, groups, year){
# Pulling State-Level Data ------------------------------------------------
    print(glue_col("Pulling poverty data for {bold {red {groups}}} at the State-level
                 using {bold {red {pov_vars}}} for the poverty total and {bold {red {totals}}}
               for the group total "))
      temp_dta <- get_acs(geography="state",variables = pov_vars,summary_var = totals, year=year)
      print(glue(""))
      final_state<- temp_dta %>%
        mutate(estimate = case_when(moe>(.25*estimate) ~ NA_real_,
                                    TRUE ~ estimate),
               summary_est = case_when(summary_moe>(.25*summary_est) ~ NA_real_,
                                       TRUE ~ summary_est)) %>% 
        select(NAME,estimate,summary_est) %>% 
        mutate(count = estimate,
               prop = estimate/summary_est) %>% 
        select(NAME,count, prop) %>% 
        gather(key = estimate_type,estimate,-NAME) %>% 
        mutate(group = groups,
               topic = "Poverty",
               topic_type= "Below Poverty",
               geo="state") %>% 
        select(geo,NAME,group,topic, topic_type,estimate_type, estimate)
    
# Pulling County-Level Data -----------------------------------------------
      print(glue_col("Pulling poverty data for {bold {red {groups}}} at the county-level
                 using {bold {red {pov_vars}}} for the poverty total and {bold {red {totals}}}
                 for the group total "))
      temp_dta <- get_acs(geography="county",variables = pov_vars,summary_var = totals, year=year)
      final_county<- temp_dta %>%
        mutate(estimate = case_when(moe>(.25*estimate) ~ NA_real_,
                                    TRUE ~ estimate),
               summary_est = case_when(moe>(.25*summary_est) ~ NA_real_,
                                       TRUE ~ summary_est)) %>% 
        select(NAME,estimate,summary_est) %>% 
        mutate(count = estimate,
               prop = estimate/summary_est) %>% 
        select(NAME,count, prop) %>% 
        gather(key = estimate_type,estimate,-NAME) %>% 
        mutate(group = groups,
               topic = "Poverty",
               topic_type= "Below Poverty",
               geo="county") %>% 
        select(geo,NAME,group,topic, topic_type,estimate_type, estimate)
    
# Pulling Congressional District Data -------------------------------------
      print(glue_col("Pulling poverty data for {bold {red {groups}}} at the District-level
                 using {bold {red {pov_vars}}} for the poverty total and {bold {red {totals}}}
                 for the group total ")) # So we know what is going 
      temp_dta <- get_acs(geography="congressional district",variables = pov_vars,summary_var = totals, year=year)
      final_district<- temp_dta %>%
        mutate(estimate = case_when(moe>(.25*estimate) ~ NA_real_,
                                    TRUE ~ estimate),
               summary_est = case_when(moe>(.25*summary_est) ~ NA_real_,
                                       TRUE ~ summary_est)) %>% 
        select(NAME,estimate,summary_est) %>% 
        mutate(count = estimate,
               prop = estimate/summary_est) %>% 
        select(NAME,count, prop) %>% 
        gather(key = estimate_type,estimate,-NAME) %>% 
        mutate(group = groups,
               topic = "Poverty",
               topic_type= "Below Poverty",
               geo="district") %>% 
        select(geo,NAME,group,topic, topic_type,estimate_type, estimate)
      
    
# Combining Geographies ---------------------------------------------------
    final <- rbind(final_state, final_county, final_district)
    return(final)
  }
final <- pmap_dfr(argList,pov_grabber) # Running pov_grabber
print("Final dataset contains poverty counts/proportions for all racial groups + all geographies,\n use geo to split dataframe by geo, and later NAME to get other geographies")
return(final)

}


