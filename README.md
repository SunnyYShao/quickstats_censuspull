# quickstats_raw_data
Quickstats Raw Data - AAPI Data

1. create help functions for each topic: (input includes "year", "geography")
2. create cleanup script that call each topics at one geo level and merge into one df
3. write out csv file that display all topics in one geo level in a long format
4. columns should include: state, county/district, group, topic, topic_type, estimate_type, estimate
5. name each csv file as "state.csv", "county.csv", ect
6. store all csv files under raw_cleanup folder

## Margin of Error Rule

- Use `tidycensus` to pull down margin of error for each count, if the MOE for a count is greater than 25% of the actual estimate, we need to drop it because it is unreliable.

## Tables for each topic

- Education: C15002D/E
- Detailed pop: Alone B02015, B02016
- Detailed pop: Combo B02018, B02019
- pop Alone: - var_list <- c("B02001_005", "B02001_006", "B03002_003",
                    "B02001_003", "B03002_012")
- pop Combo: - var_list <- c("B02011_001", "B02012_001", "B03002_003",
                    "B02001_003", "B03002_012")  
- nativity: B05003D/E
- LEP: B16005D/E
- poverty: B17001D/E


# File notes:

1. `TEST_full_update.r`: This is just a test script, all it does right now is load the `final_merger` function and then runs it
2. `final_merger.r`: I think this could be the skelton of how the final merge process will work.