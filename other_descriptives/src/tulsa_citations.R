#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019
# =========================================
# HRW-us-tulsa/descriptives/src/tulsa_citations.R

library(pacman)
p_load(tidyverse, lubridate, readr, scales, stringr, here, assertr,
       sf, leaflet, htmlwidgets, ggplot2, zoo, Hmisc, extrafont)
options(scipen=999)

here <- here::here
font_import() # import all your fonts
fonts() #get a list of fonts
fonttable()
fonttable()[90:94,]
loadfonts()

source(here("other_descriptives/src/functions_shared.R"))

#load data
citations <- read_rds(here("matching/output/citations.rds"))
census_tract <- read_rds(here("census_data/output/census_tracts.rds"))
census_long <- read_rds(here("census_data/output/city_census_long.rds"))

#start a list of values for export
citations_literal_values <- list()

#list of output files
outputfiles <- list(
   citations_literal_values = "other_descriptives/output/citations_literal_values.rds",
   table_proportion = "other_descriptives/output/citations_table_proportion.rds",
   table_multiple_locations = "other_descriptives/output/table_multiple_locations.rds",
   citation_rate_map_data = "other_descriptives/output/citation_rate_map_data.rds",
   rate_map = "other_descriptives/output/map_rate_citations.html",
   DL_only_map_data = "other_descriptives/output/DL_only_map_data.rds",
   DL_only_map = "other_descriptives/output/map_DLonly_rate.html",
   interactive = "other_descriptives/output/interactive_citations.csv")
outputfiles <- map(outputfiles, here)

#date range
citations_literal_values$start_date <- min(citations$VIOLATIONDATE)
citations_literal_values$end_date <- max(citations$VIOLATIONDATE)

#total_number
citations_literal_values$total_citations <- n_distinct(citations$CASENO)
citations_literal_values$total_citation_events <- n_distinct(citations$citation_id)

#move latino/hispanic to white, as we did elsewhere
citations <- citations %>%
   mutate(race2 = ifelse(race == "Latino/Hispanic", "White", race))

#citywide census
tulsa_census2 <- census_long %>%
   filter(method == "Method 2 - multi-race recoded as single race")

#rate citywide, no race
total <- sum(tulsa_census2$number)

rate <- citations %>%
   summarise(citations = n(),
             individual_events = n_distinct(citation_id)) %>%
   mutate(rate = (citations/total * 2) * 1000)

citations_literal_values$citywide_total_citation_rate_per_1K <- round(as.numeric(rate[1,3]), 0)
rm(total, rate)

#Race citywide
rate <- citations %>%
   group_by(race2) %>%
   summarise(citations = n(),
             individual_events = n_distinct(citation_id)) %>%
   mutate(perc = citations/sum(citations))

rate <- left_join(rate, tulsa_census2)

rate <- rate %>%
   mutate(rate_per_capita = (citations/number*2) * 1000)

#overall difference in per capita rate, citywide
rate <- rate %>%
   mutate(ratio = round(rate_per_capita/rate_per_capita[race2 == "White"], 1))

citations_literal_values$citywide_ratio_per_capita <- as.numeric(rate[2, 10])
citations_literal_values$citywide_black_citation_rate <- round(as.numeric(rate[2, 9]), 0)
citations_literal_values$citywide_black_perc_of_citations <- 100 * 
   round(as.numeric(rate[2, 4]), 2)
citations_literal_values$citywide_white_perc_of_citations <- 100 * 
   round(as.numeric(rate[5, 4]), 2)
rm(rate)

#total percentage of citations for different things
rate <- citations %>%
   group_by(citation) %>%
   summarise(citations = n()) %>%
   mutate(perc = citations/sum(citations)) %>%
   arrange(desc(perc))

citations_literal_values$perc_total_speeding <- 
   as.numeric(round(rate[1,3], 2)) * 100
citations_literal_values$perc_total_DL <- 
   as.numeric(round(rate[2,3], 2)) * 100
citations_literal_values$perc_total_moving <- 
   as.numeric(round(rate[3,3], 2)) * 100
citations_literal_values$perc_total_licenseplate <- 
   as.numeric(round(rate[4,3], 2)) * 100
citations_literal_values$perc_total_seatbelts <- 
   as.numeric(round(rate[5,3], 2)) * 100

#citywide race rates by type of citation
rate <- citations %>%
   filter(citation == "Driver's license/liability insurance related" |
              citation == "Speeding" ) %>%
   group_by(race2, citation) %>%
   summarise(citations = n(),
             individual_events = n_distinct(citation_id)) 

rate <- left_join(rate, tulsa_census2)

rate <- rate %>%
   mutate(rate_per_capita = (citations/number*2) * 1000)

#overall difference in per capita rate, citywide
rate <- rate %>%
   ungroup() %>%
   group_by(citation) %>%
   mutate(ratio = round(rate_per_capita/rate_per_capita[race2 == "White"], 1)) %>%
   arrange(citation)
citations_literal_values$citywide_license_ratio <- as.numeric(rate[2, 10])

rate <- citations %>%
   filter(citation == "Driver's license/liability insurance related" |
             citation == "Speeding" ) %>%
   group_by(race2, citation) %>%
   summarise(citations = n_distinct(citation_id)) 

rate <- left_join(rate, tulsa_census2)

rate <- rate %>%
   mutate(rate_per_capita = (citations/number*2) * 1000)

#overall difference in per capita rate, citywide
rate <- rate %>%
   ungroup() %>%
   group_by(citation) %>%
   mutate(ratio = round(rate_per_capita/rate_per_capita[race2 == "White"], 1)) %>%
   arrange(citation)
rm(rate)

#Table
table <- citations %>%
   filter(citation == "Driver's license/liability insurance related" |
             citation == "Speeding" ) %>%
   group_by(citation, race2) %>%
   summarise(citations = n()) %>%
   mutate(perc = citations/sum(citations)) %>%
   filter(race2 == "White" | race2 == "Black") 

join <- tulsa_census2 %>%
   filter(race2 == "White" | race2 == "Black") %>%
   select(race2, perc = Tulsa_City_Percent) %>%
   mutate(citation = "Tulsa population")

table <- bind_rows(table, join) %>%
   mutate(perc = paste((round(perc, 2) * 100), "%", sep = "")) %>%
   select(-citations) %>%
   spread(race2, perc) %>%
   rename(Group = citation) %>%
   arrange(desc(Group))
write_rds(table, outputfiles$table_proportion)

rm(join, table)

##are people getting license/insurance also getting other tickets?
#mark any citation id that got one
citations <- citations %>%
   mutate(license = ifelse(citation == "Driver's license/liability insurance related", 
                           1, 0))

test <- citations %>%
   ungroup() %>%
   filter(license == 1)

citations <- citations %>%
   mutate(has_license_citation = ifelse(citation_id %in% test$citation_id,
                                        1, 0))
rm(test)

test <- citations %>%
   filter(license == 0 & has_license_citation == 1)

citations <- citations %>%
   mutate(has_non_license = ifelse(citation_id %in% test$citation_id,
                                   1, 0))

test <- citations %>%
   filter(has_license_citation == 1 &
             has_non_license == 0)

citations <- citations %>%
   mutate(only_license = ifelse(citation_id %in% test$citation_id,
                                1, 0))
rm(test)

#proportion only license
rate <- citations %>%
   filter(has_license_citation == 1) %>%
   group_by(only_license) %>%
   summarise(count = n_distinct(citation_id)) %>%
   mutate(perc = count/sum(count))
rm(rate)

#rates and ratio
race_rate <- citations %>%
  #filter(has_license_citation == 1) %>%
   group_by(has_license_citation, only_license, race2) %>%
   summarise(count = n_distinct(citation_id)) %>%
   mutate(perc = count/sum(count))

race_rate <- left_join(race_rate, tulsa_census2) 
race_rate <- race_rate %>%
   filter(!is.na(Race)) %>%
   group_by(has_license_citation, only_license) %>%
   mutate(rate_per_capita = (count/number*2) * 1000,
          ratio = round(rate_per_capita/rate_per_capita[race2 == "White"], 2))


#race ratios for having a license citation
race_rate <- citations %>%
   filter(has_license_citation == 1) %>%
   group_by(only_license, race2) %>%
   summarise(count = n_distinct(citation_id)) %>%
   mutate(perc = count/sum(count))

race_rate <- left_join(race_rate, tulsa_census2) %>%
   mutate(rate_per_capita = (count/number*2) * 1000,
          ratio = rate_per_capita/rate_per_capita[race2 == "White"])

citations_literal_values$citywide_license_only_ratio <- round(as.numeric(race_rate[7, 10]), 2)

#Tracts
#filter out citations that weren't geocoded to city tracts
citations_full <- citations

census_tract <- census_tract %>%
   filter(method == "Method 2 - multi-race recoded as single race") 
   
join <- as.data.frame(census_tract) %>%
   select(GEOID, keep)

citations <- left_join(citations, join) %>%
   filter(keep == 1)
rm(join)

tract <- citations %>%
   group_by(GEOID) %>%
   summarise(count = n()) %>%
   filter(!is.na(count))

tract <- left_join(census_tract, tract) %>%
   mutate(rate_total = (count/tot_pop *2) * 1000)

citation_tract_rate <- st_sf(tract)

#write_data for markdown
write_rds(citation_tract_rate, outputfiles$citation_rate_map_data)

#map of traffic stop rates
pal <- colorNumeric(palette = "viridis", domain = citation_tract_rate$rate_total)

map_rate <- citation_tract_rate %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ rate_total,
             title = "Citations per 1,000 people living in census tract.\n2014 - 2017",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(comma_format()(rate_total), " citations per 1,000 people", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(rate_total)) 
map_rate
saveWidgetFix(map_rate, file = outputfiles$rate_map, selfcontained=T)


#write tract rate of citations for interactive
interactive <- tract %>%
   select(GEOID, citation_rate = rate_total)
write_csv(interactive, outputfiles$interactive)
rm(interactive)

#map of DL-only citations total rate per capita.
tract <- citations %>%
   filter(only_license == 1) %>%
   group_by(GEOID) %>%
   summarise(count = n()) %>%
   filter(!is.na(count))

tract <- left_join(census_tract, tract) %>%
   mutate(rate_total = (count/tot_pop *2) * 1000)

tract <- st_sf(tract)

pal <- colorNumeric(palette = "viridis", domain = tract$rate_total)

map_rate <- tract %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ rate_total,
             title = "Driver's license or insurance only citations per 1,000 census tract residents.\n2014 - 2017",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(comma_format()(rate_total), " citations per 1,000 residents", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(rate_total)) 
map_rate
saveWidgetFix(map_rate, file = outputfiles$DL_only_map, selfcontained=T)

#north v. south
north <- tract %>%
   group_by(north_tulsa) %>%
   summarise(total_citations = sum(count),
             total_population = sum(tot_pop)) %>%
      mutate(rate = total_citations/(2 * total_population) * 1000)
rm(north)

#proportion of citations for different things
#top ten officers, top three locations, overall, north tulsa
#Top three locations
top3 <- citations %>%
   filter(top_3_location == 1)

top3_tracts <- semi_join(citations, top3) %>%
   select(GEOID) 
top3_tracts <- semi_join(tract, top3_tracts)

table <- semi_join(citations, top3) %>%
   group_by(citation) %>%
   summarise(count = n()) %>%
   mutate(perc_3locations = count/sum(count)) %>%
   arrange(desc(count)) %>%
   select(-count)

#top ten officers
officers <- citations %>%
   group_by(OFFICERNO) %>%
   summarise(count = n()) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(count)) %>%
   mutate(cumperc = cumsum(perc)) %>%
   slice(1:10)

officer2 <- semi_join(citations, officers)

top10 <- officer2 %>%
   group_by(citation) %>%
   summarise(count = n()) %>%
   mutate(perc_10officers = count/sum(count)) %>%
   arrange(desc(count)) %>%
   select(-count)

table <- full_join(table, top10)
rm(top10, officer2, officers)

#total
total <- citations %>%
   group_by(citation) %>%
   summarise(count = n()) %>%
   mutate(perc_total = count/sum(count)) %>%
   arrange(desc(count)) %>%
   select(-count)

table <- full_join(table, total)
rm(total)

#north Tulsa
join <- as.data.frame(census_tract) %>%
   select(GEOID, north_tulsa)
citations <- left_join(citations, join)

total <- citations %>%
   filter(north_tulsa == 1) %>%
   group_by(citation) %>%
   summarise(count = n()) %>%
   mutate(perc_north = count/sum(count)) %>%
   arrange(desc(count)) %>%
   select(-count)

table <- full_join(table, total)

#non-North Tulsa
total <- citations %>%
   filter(is.na(north_tulsa)) %>%
   group_by(citation) %>%
   summarise(count = n()) %>%
   mutate(perc_non_north = count/sum(count)) %>%
   arrange(desc(count)) %>%
   select(-count)

table <- full_join(table, total)

table <- table %>%
   mutate_if(is.numeric, round, 2) %>%
   mutate_if(is.numeric, ~.x*100) %>%
   mutate_if(is.numeric, paste,"%", sep = "") %>%
   slice(1:7) %>%
   select(Citation = citation,
          `Tulsa total` = perc_total,
          `North Tulsa` = perc_north,
          `Non-North Tulsa` = perc_non_north,
          `Top 3 locations` = perc_3locations,
          `Top 10 officers` = perc_10officers
          )

rm(total, top3)

#save table
write_rds(table, outputfiles$table_multiple_locations)
rm(table)

#save literal values
write_rds(citations_literal_values, outputfiles$citations_literal_values)
