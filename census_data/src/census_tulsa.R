#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019
# =========================================
# HRW-us-tulsa/census_data/src/census_tulsa.R
library(pacman)
p_load(tidyverse, readr, here, tidycensus, sf, leaflet, htmlwidgets)
options(scipen=999)
options(tigris_use_cache = TRUE)

here <- here::here

source(here("other_descriptives/src/functions_shared.R"))

##Get api key from tidycensus: 
#save in .Renviron
#Use 2017 5-Year American Community Survey for all demographic/
#economic data.

#list of handcode-frozen files
frozenfiles <- c(
   zips = "census_data/frozen/zip_ACS.csv",
   zips_econ = "census_data/frozen/zip_ACS_economic.csv",
   north_tulsa_labels = "census_data/frozen/north_tulsa_labels.csv",
   tulsa_zips = "census_data/frozen/tulsa_zips.csv",
   life_expectancy = "census_data/frozen/life_expectancy.csv"
)
   
read_frozen <- function(f) read_csv(here(frozenfiles[f]))

#start a list of values for export
census_literal_values <- list()

#output files. 
outputfiles <- list(
   census_literal_values = "census_data/output/census_literal_values.rds",
   tulsa_city_census_wide = "census_data/output/city_census_wide.rds",
   tulsa_city_census_long = "census_data/output/city_census_long.rds",
   census_tracts = "census_data/output/census_tracts.rds",
   tulsa_zips = "census_data/output/zips.rds",
   map_life_expectancy = "census_data/output/life_expectancy_map.html",
   map_black_perc_pop = "census_data/output/black_pop_perc_map.html",
   map_unemployment = "census_data/output/unemployment_map.html",
   map_poverty = "census_data/output/poverty_map.html",
   census_variables = "census_data/frozen/census_variables.csv"
   ) %>% map(here)

######### CENSUS DATA  ###############
#Two methods for city wide rates, numbers
census_variables <- load_variables(2017, "acs5", cache = TRUE)
write_csv(census_variables, outputfiles$census_variables)

my_vars <- c(
   tot_pop = "C02003_001",
   one_race_pop = "C02003_002",
   white_pop = "C02003_003",
   black_pop = "C02003_004",
   native_am_pop = "C02003_005",
   asian_pop = "C02003_006",
   other1 = "C02003_007",
   other2 = "C02003_008",
   two_more_total = "C02003_009",
   two_more_black_white = "C02003_013",
   two_more_white_native = "C02003_014",
   two_more_white_asian = "C02003_015",
   two_more_black_native = "C02003_016"
)

#tulsa city
city_census <- get_acs(geography = "place", 
                       variables = my_vars, 
                       state = "OK") %>%
   filter(GEOID == 4075000)

city_census <- city_census %>%
   select(-moe) %>%
   spread(variable, estimate) 

city_census <- city_census %>%
   mutate(two_more_unknown = two_more_total - 
             (two_more_black_white + two_more_black_native +
                 two_more_white_native + two_more_white_asian),
          tot_pop_less_unknown = tot_pop - two_more_unknown, 
          other_pop = other1 + other2) %>%
   select(-other1, -other2) 

#method one - use single race only in proportions
city_wide_m1 <- city_census %>%
   mutate(perc_asian = asian_pop/one_race_pop, perc_other = other_pop/one_race_pop,
          perc_native = native_am_pop/one_race_pop, perc_black = black_pop/one_race_pop,
          perc_white = white_pop/one_race_pop,
          non_white = 1 - perc_white) %>%
   mutate(method = "Method 1 - multi-race pop removed")

m2 <- city_wide_m1 %>%
   gather(Race, Percent, 17:21) %>%
   select(Race, Tulsa_City_Percent = Percent) %>%
   mutate(Tulsa_City_Percent = round(Tulsa_City_Percent, 3)) %>%
   mutate(Race = case_when(
      Race == "perc_asian" ~ "asian_pop",
      Race == "perc_black" ~ "black_pop",
      Race == "perc_native" ~ "native_am_pop",
      Race == "perc_other" ~ "other_pop",
      Race == "perc_white" ~ "white_pop"
   ))

m3 <-  city_wide_m1 %>%
   gather(Race, number, asian_pop, black_pop, native_am_pop,
          other_pop, white_pop) %>%
   select(Race, number) %>%
   mutate(method = "Method 1 - multi-race pop removed")

city_long_m1 <- left_join(m2, m3)
rm(m2, m3)

#method2 with two or more races recoded as single race.
city_wide_m2 <- city_census %>%
   mutate(black_pop = black_pop + two_more_black_white + two_more_black_native,
          native_am_pop = native_am_pop + two_more_white_native,
          asian_pop = asian_pop + two_more_white_asian) %>%
   mutate(perc_asian = asian_pop/tot_pop_less_unknown, perc_other = other_pop/tot_pop_less_unknown,
          perc_native = native_am_pop/tot_pop_less_unknown, perc_black = black_pop/tot_pop_less_unknown,
          perc_white = white_pop/tot_pop_less_unknown,
          non_white = 1 - perc_white) %>%
   mutate(method = "Method 2 - multi-race recoded as single race")

m2 <- city_wide_m2  %>%
   gather(Race, Percent, 17:21) %>%
   select(Race, Tulsa_City_Percent = Percent) %>%
   mutate(Tulsa_City_Percent = round(Tulsa_City_Percent, 3)) %>%
   mutate(Race = case_when(
      Race == "perc_asian" ~ "asian_pop",
      Race == "perc_black" ~ "black_pop",
      Race == "perc_native" ~ "native_am_pop",
      Race == "perc_other" ~ "other_pop",
      Race == "perc_white" ~ "white_pop"
   ))

m3 <-  city_wide_m2  %>%
   gather(Race, number, asian_pop, black_pop, native_am_pop,
          other_pop, white_pop) %>%
   select(Race, number) %>%
   mutate(method = "Method 2 - multi-race recoded as single race")

city_long_m2 <- left_join(m2, m3)

tulsa_city_census_long <- bind_rows(city_long_m1, city_long_m2)
tulsa_city_census_wide <- bind_rows(city_wide_m1, city_wide_m2 )
rm(m2, m3, city_long_m2, city_long_m1, city_wide_m2, city_wide_m1)

tulsa_city_census_long <- tulsa_city_census_long %>%
   mutate(race2 = case_when(
      Race == "asian_pop" ~ "Asian",
      Race == "black_pop" ~ "Black",
      Race == "native_am_pop" ~ "Native American",
      Race == "other_pop" ~ "Other",
      Race == "white_pop" ~ "White",
      Race == "H" ~ "Latino/Hispanic",
      is.na(Race) ~ NA_character_,
      TRUE        ~ "ERROR"
   ))

#####CENSUS TRACT#####
#add economic variables
my_vars <- c(my_vars,
   gini = "B19083_001",
   median_hh_income = "B19013_001",
   median_black_income = "B19013B_001",
   median_white_income = "B19013A_001",
   labor_force = "B23025_002",
   unemployed_over16 = "B23025_005",
   poverty_status = "B17020_002",
   poverty_pop = "B17020_001",
   white_poverty_pop = "B17020A_001",
   white_poverty_status = "B17020A_002",
   black_female_labor_force = "C23002B_017",
   black_female_unemployed = "C23002B_021",
   black_male_labor_force = "C23002B_004",
   black_male_unemployed = "C23002B_008",
   white_female_labor_force = "C23002A_017",
   white_female_unemployed = "C23002A_021",
   white_male_labor_force = "C23002A_004",
   white_male_unemployed = "C23002A_008",
   black_poverty_pop = "B17020B_001",
   black_poverty_status = "B17020B_002",
   white_edu_total = "C15002A_001",
   white_less_highschool_male = "C15002A_003",
   white_less_highschool_female = "C15002A_008",
   white_college_male = "C15002A_006",
   white_college_female = "C15002A_011",
   black_edu_total = "C15002B_001",
   black_less_highschool_male = "C15002B_003",
   black_less_highschool_female = "C15002B_008",
   black_college_male = "C15002B_006",
   black_college_female = "C15002B_011"
)

tracts <- get_acs(geography = "tract", 
                variables = my_vars,
                state = "OK", county = "Tulsa",
                geometry = T)

tracts <- tracts %>%
   select(-moe) %>%
   spread(variable, estimate)

tracts <- tracts %>%
   mutate(two_more_unknown = two_more_total - 
             (two_more_black_white + two_more_black_native +
                 two_more_white_native + two_more_white_asian),
          tot_pop_less_unknown = tot_pop - two_more_unknown, 
          other_pop = other1 + other2) %>%
   select(-other1, -other2) 

tracts <- tracts %>%
   mutate(black_labor_force = black_male_labor_force + 
             black_female_labor_force,
          white_labor_force = white_male_labor_force + 
             white_female_labor_force,
          black_unemployed = black_male_unemployed + black_female_unemployed,
          white_unemployed = white_male_unemployed + white_female_unemployed,
          black_unemployment_rate = black_unemployed/black_labor_force,
          white_unemployment_rate = white_unemployed/white_labor_force,
          unemployment_rate = unemployed_over16/labor_force,
          white_poverty_rate = white_poverty_status/white_poverty_pop,
          black_poverty_rate = black_poverty_status/black_poverty_pop,
          poverty_rate = poverty_status/poverty_pop) 

tracts <- tracts %>%
   mutate(white_perc_less_highschool = (white_less_highschool_female + 
                                           white_less_highschool_male)/white_edu_total,
          white_perc_collegedegree = (white_college_male + white_college_female)/white_edu_total,
          black_perc_less_highschool = (black_less_highschool_female + 
                                           black_less_highschool_male)/black_edu_total,
          black_perc_collegedegree = (black_college_male + black_college_female)/black_edu_total)

#method one - use single race only in proportions
tract_m1 <- tracts %>%
   mutate(perc_asian = asian_pop/one_race_pop, perc_other = other_pop/one_race_pop,
          perc_native = native_am_pop/one_race_pop, perc_black = black_pop/one_race_pop,
          perc_white = white_pop/one_race_pop,
          non_white = 1 - perc_white) %>%
   mutate(method = "Method 1 - multi-race pop removed")

#method 2
tract_m2 <- tracts %>%
   mutate(black_pop = black_pop + two_more_black_white + two_more_black_native,
          native_am_pop = native_am_pop + two_more_white_native,
          asian_pop = asian_pop + two_more_white_asian) %>%
   mutate(perc_asian = asian_pop/tot_pop_less_unknown, perc_other = other_pop/tot_pop_less_unknown,
          perc_native = native_am_pop/tot_pop_less_unknown, perc_black = black_pop/tot_pop_less_unknown,
          perc_white = white_pop/tot_pop_less_unknown,
          non_white = 1 - perc_white) %>%
   mutate(method = "Method 2 - multi-race recoded as single race")

tracts <- rbind(tract_m1, tract_m2)
rm(tract_m1, tract_m2)

#Bring in handcoding of North Tulsa tracts and "keep" e.g. tracts
#within city limit
n_tulsa_labels <- read_frozen("north_tulsa_labels")

n_tulsa_labels$GEOID <- as.character(n_tulsa_labels$GEOID)
tracts <- left_join(tracts, n_tulsa_labels)

tracts <- tracts %>%
   filter(keep == 1)
rm(n_tulsa_labels)

tracts <- tracts %>%
   mutate(tracts = gsub("Census Tract ", "", NAME),
          tracts = gsub(", Tulsa County, Oklahoma", "", tracts),
          tracts = as.double(tracts))

#bring in life expectancy
life <- read_frozen("life_expectancy") %>%
   select(GEOID = (`Tract ID`),
          life_expectancy = `e(0)`) %>%
   mutate(GEOID = as.character(GEOID))

tracts <- left_join(tracts, life)

####ZIP CODES####
#zip codes can't use tidycensus b/c problem with zip to ZCTA.
#Downloaded from census bureau
#zip files in frozen
zips <- read_frozen("zips")
econ_zip <- read_frozen("zips_econ")

zips <- zips %>%
   select(zip = GEO.id2, tot_pop = HC01_VC43, one_race_pop = HC01_VC44, asian_pop = HC01_VC56, 
          other_pop = HC01_VC69, native_am_pop = HC01_VC51, 
          black_pop = HC01_VC50, white_pop = HC01_VC49, two_more_total = HC01_VC45,
          two_more_black_white = "HC01_VC71", 
          two_more_white_native = "HC01_VC72",
          two_more_white_asian = "HC01_VC73",
          two_more_black_native = "HC01_VC74") %>%
   mutate(two_more_unknown = two_more_total - 
             (two_more_black_white + two_more_black_native +
                 two_more_white_native + two_more_white_asian),
          tot_pop_less_unknown = tot_pop - two_more_unknown)

#method 1
zips_m1 <- zips %>%
   filter(one_race_pop > 0) %>%
   mutate(perc_asian = asian_pop/one_race_pop, perc_other = other_pop/one_race_pop,
          perc_native = native_am_pop/one_race_pop, perc_black = black_pop/one_race_pop,
          perc_white = white_pop/one_race_pop,
          non_white = 1 - perc_white) %>%
   mutate(method = "Method 1 - multi-race pop removed")

#method 2
zips_m2 <- zips %>%
   mutate(black_pop = black_pop + two_more_black_white + two_more_black_native,
          native_am_pop = native_am_pop + two_more_white_native,
          asian_pop = asian_pop + two_more_white_asian) %>%
   mutate(perc_asian = asian_pop/tot_pop_less_unknown, perc_other = other_pop/tot_pop_less_unknown,
          perc_native = native_am_pop/tot_pop_less_unknown, perc_black = black_pop/tot_pop_less_unknown,
          perc_white = white_pop/tot_pop_less_unknown,
          non_white = 1 - perc_white) %>%
   mutate(method = "Method 2 - multi-race recoded as single race")

zips <- bind_rows(zips_m1, zips_m2)
rm(zips_m1, zips_m2)

econ_zip <- econ_zip %>%
   select(zip = GEO.id2, unemploy_rate = HC03_VC12, med_income = HC01_VC85,
          perc_below_pov = HC03_VC161)

zips <- left_join(zips, econ_zip)
rm(econ_zip)

zips <- zips %>%
   mutate(unemploy_rate = as.numeric(unemploy_rate),
          med_income = as.numeric(med_income),
          perc_below_pov = as.numeric(perc_below_pov),
          non_white = 1 - perc_white)

#filter tulsa city zips
tulsa_zips <- read_frozen("tulsa_zips")

zips <- semi_join(zips, tulsa_zips)
rm(tulsa_zips)

#basic maps #filter for method 2
tracts2 <- tracts 
tracts <- tracts %>%
   filter(method == "Method 2 - multi-race recoded as single race")

#life expectancy
pal <- colorNumeric(palette = "viridis", 
                    domain = tracts$life_expectancy,
                    reverse = T)

map_life <- tracts %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ life_expectancy,
             title = "Life expectancy",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(life_expectancy,
                               " years life expectancy", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(life_expectancy)) 
map_life
saveWidgetFix(map_life,
              file = outputfiles$map_life_expectancy, selfcontained=T)
rm(map_life)


#black percent of tract
pal <- colorNumeric(palette = "viridis", 
                    domain = tracts$perc_black)

map_perc_black <- tracts %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ perc_black,
             title = "Percent of Census Tract that is Black",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(round(perc_black, 2) * 100,
                               " percent Black", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(perc_black)) 
#map_perc_black
saveWidgetFix(map_perc_black,
 file = outputfiles$map_black_perc_pop, selfcontained=T)
rm(map_perc_black)

#unemployment rate
pal <- colorNumeric(palette = "viridis", 
                    domain = tracts$unemployment_rate)

map_unemployed <- tracts %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ unemployment_rate,
             title = "Unemployment Rate",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(round(unemployment_rate, 2) * 100, "%",
                               " unemployment rate", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(unemployment_rate)) 
map_unemployed
saveWidgetFix(map_unemployed,
   file = outputfiles$map_unemployment, selfcontained=T)
rm(map_unemployed)

#map poverty rate
pal <- colorNumeric(palette = "viridis", 
                    domain = tracts$poverty_rate)

map_poverty <- tracts %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ poverty_rate,
             title = "poverty Rate",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(round(poverty_rate, 2) * 100, "%",
                               " poverty rate", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(poverty_rate)) 
map_poverty
saveWidgetFix(map_poverty,
           file = outputfiles$map_poverty, selfcontained=T)
rm(map_poverty)

#Aggregate in N. Tulsa
north <- tracts %>%
   filter(north_tulsa == 1) %>%
   summarise(north_black_labor = sum(black_labor_force),
             north_black_unemployed = sum(black_unemployed),
             north_black_pov_pop = sum(black_poverty_pop),
             north_black_poverty = sum(black_poverty_status),
             north_white_labor = sum(white_labor_force),
             north_white_unemployed = sum(white_unemployed),
             north_white_pov_pop = sum(white_poverty_pop),
             north_white_poverty = sum(white_poverty_status),
             black_pop = sum(black_pop),
             white_pop = sum(white_pop)) %>%
   mutate(north_black_unemployment_rate = 
             north_black_unemployed/north_black_labor,
          north_black_poverty_rate = 
             north_black_poverty/north_black_pov_pop,
          north_white_unemployment_rate = 
             north_white_unemployed/north_white_labor,
          north_white_poverty_rate = 
             north_white_poverty/north_white_pov_pop)

#save literal values for North Tulsa
census_literal_values$north_black_unemployment_rate <- 
   as.numeric(round(north$north_black_unemployment_rate, 2)*100)
census_literal_values$north_white_unemployment_rate <- 
   as.numeric(round(north$north_white_unemployment_rate, 2)*100)
census_literal_values$north_black_poverty_rate <-
   as.numeric(round(north$north_black_poverty_rate, 2)*100)
census_literal_values$north_white_poverty_rate <-
   as.numeric(round(north$north_white_poverty_rate, 2)*100)



#output
write_rds(census_literal_values, outputfiles$census_literal_values)
write_rds(tulsa_city_census_wide, outputfiles$tulsa_city_census_wide)
write_rds(tulsa_city_census_long, outputfiles$tulsa_city_census_long)
write_rds(tracts, outputfiles$census_tract)
write_rds(zips, outputfiles$tulsa_zips)
