

final_merger <- function(update_topics){
# Load Helper Function for Poverty
source("topic_help_function/pov_help_function_sunny.R")
source("topic_help_function/detailed_help_function.R")
source("topic_help_function/pop_help_function.R")
source("topic_help_function/edu_help_function.R")
source("topic_help_function/lep_help_function.R")
source("topic_help_function/nativity_help_function.R")
source("topic_help_function/ins_help_function.R")
# Each helper function needs to output with following columns: geo, NAME, group, topic, topic_type, estimate_type, estimate
# geo values = [state, county, district]
# NAME= [raw name from tidycensus, we will disagg later after we split by geo]
# group = [Asian Alone, NHPI Alone, Black Alone, Hispanic Alone, White Alone]
# topic = [poverty, education, etc.]
# topic_type = [Below Poverty, BA or Higher, etc.]
# estimate_type = [count, prop]
# estimate = [the actual value]
  

# Conditionally update each topic -----------------------------------------
# NEED SOME CODE HERE TO RUN HELPER FUNCTION IF IT"S INCLUDED IN "update_topics"
# Right now I coded the poverty function to just automatically update everything (all geographies + all racial groups, but we might want to alter this)

if(update_topics %in% "detailed"){
  print("Looks like you want to update detailed groups!")
  
  temp_df2 <- detailed_total_updatter()
  
}else if(update_topics %in% "education"){
  print("Looks like you want to update education!")

  temp_df3 <- edu_total_updater()
  
}else if(update_topics %in% "lep"){
  print("Looks like you want to update LEP!")

  temp_df4 <- lep_total_updatter()
  
}else if(update_topics %in% "nativity"){
  print("Looks like you want to update nativity!")

  temp_df5 <- nativity_total_updatter()

}else if(update_topics %in% "population"){
  print("Looks like you want to update population!")

  temp_df6 <- pop_total_updatter()
  
}else if(update_topics %in% "poverty"){
  print("Looks like you want to update poverty!")
  temp_df1 <- pov_total_updatter()

}else if(update_topics %in% "insurance"){
  temp_df7 <- ins_total_updatter()
}else {}
  
  
# Combine them ------------------------------------------------------------
### SOME CODE HERE
combined_df <- rbind(if(exists("temp_df1")) temp_df1, if(exists("temp_df2")) temp_df2,
                     if(exists("temp_df3")) temp_df3, if(exists("temp_df4")) temp_df4,
                     if(exists("temp_df5")) temp_df5, if(exists("temp_df6")) temp_df6,
                     if(exists("temp_df7")) temp_df7)


rm(list=ls()[! ls() %in% c("combined_df")]) #Delete and clear space
gc()
# combined_df will have all the topics +geos? 

# Disaggregate by geography -----------------------------------------------

# This should take the combined_df and subset it by geography

# State-Level data --------------------------------------------------------
print("Subsetting data for State-level File \n Saving in cleaned_data folder")
combined_df %>% 
  filter(geo =="state") %>% 
  select(-geo) %>% 
  rename(State=NAME) %>% 
  write_rds("raw_cleanup/dta_state.rds")


# County ------------------------------------------------------------------
print("Subsetting data for County-level File. \n Saving in cleaned_data folder")
combined_df %>% 
  filter(geo =="county") %>% 
  select(-geo) %>% 
  separate(NAME, into = c("County","State"),sep=", ") %>% 
  write_rds("raw_cleanup/dta_county.rds")


# Congressional district --------------------------------------------------
print("Subsetting data for District-level File \n Saving in cleaned_data folder")
combined_df %>% 
  filter(geo =="district") %>% 
  select(-geo) %>% 
  separate(NAME, into = c("District","State"),sep=", ") %>% 
  mutate(District = str_remove(District,"Congressional ")) %>% 
  mutate(District = str_remove(District, " \\(115th Congress\\)")) %>% 
  write_rds("raw_cleanup/dta_district.rds")
}

