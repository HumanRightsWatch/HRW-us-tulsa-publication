#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019
# =========================================
# HRW-us-tulsa/matching/src/matching_script.R


library(pacman)
p_load(tidyverse, lubridate, scales, stringr, here, fuzzyjoin,
       stringdist, geosphere)
options(scipen=999)


########### input and output files ##############
# input files:
inputfiles <- tribble(
   ~name, ~filename,
   "arrests", "processing/output/arrests.rds",
   "citations", "processing/output/citations.rds",
   "calls_stops", "processing/output/calls_stops.rds",
   "use_force", "processing/output/use_force.rds"
)

# files that have undergone some manual transformations or hand coding
frozenfiles <- c(
   force_match_scored = "matching/frozen/force_matches_scored.csv"
   )

#start a list of values for export
literal_values <- list()

#output file list
outputfiles <- list(
   matching_literal_values = "matching/output/matching_values.rds",
   stops =  "matching/output/stops.rds",
   arrests =  "matching/output/arrests.rds",
   citations =  "matching/output/citations.rds",
   stop_arrest_match_ids =  "matching/extra/stop_arrest_match_ids.rds",
   full_arrest_stop_matches =  "matching/extra/full_arrest_stop_matches.rds",
   stop_citation_match_ids =  "matching/extra/stop_citation_match_ids.rds",
   full_citation_stop_matches =  "matching/extra/full_citation_stop_matches.rds",
   force_match =  "matching/output/force_arrests_matches.rds",
   use_force = "matching/output/use_force.rds"
 ) %>% 
   map(here)

read_input <- function(inputfile) {
   inputfiles %>%
      filter(name == inputfile) %>%
      pluck("filename") %>%
      here %>%
      read_rds
}

read_frozen <- function(f) read_csv(here(frozenfiles[f]))

#read in data
arrests <- read_input("arrests") %>%
   mutate(long = as.numeric(long),
          lat = as.numeric(lat))
citations <- read_input("citations") %>%
   mutate(long = as.numeric(long),
          lat = as.numeric(lat))
calls_stops <- read_input("calls_stops") %>%
   mutate(long = as.numeric(long),
          lat = as.numeric(lat))
use_force <- read_input("use_force")

#just traffic or pedestrian stops
stops <- calls_stops %>%
   filter(reason == "Traffic Stop" | reason == "Pedestrian Check")

min(stops$Response_Date)
max(stops$Response_Date)
min(arrests$ARREST_DATE)
max(arrests$ARREST_DATE)


###MATCHING ARRESTS AND STOPS###
#Join the arrests data base with the stops database based on the rounded 
#date, so only comparing events that occurred on the same day.
#Limitation: we may miss edge cases if stop is coded before midnight
#and arrest is coded after midnight
arrests_join <- arrests %>%
   select(arrest_id, lat, long, date = ARREST_DATE, arc_address) %>%
   filter(!is.na(lat) & !is.na(long)) %>%
   mutate(rounded_date = round_date(date, "day"))

stops_join <- stops %>%
   select(stop_id, lat, long, date = Response_Date, arc_address) %>%
   filter(!is.na(lat) & !is.na(long)) %>%
   mutate(rounded_date = round_date(date, "day"))

#time join
stops_arrests_day_join <- inner_join(arrests_join, stops_join, 
                                 by = "rounded_date") %>%
   distinct()

#filter by time - only arrests that occurred within an hour of stop.
stops_arrests_day_join <- stops_arrests_day_join %>%
   mutate(time_difference = difftime(date.y, date.x, units = "mins")) %>%
   filter(time_difference < 0 & time_difference >= -60)

#filter by distance - only arrests within 500 ft of stop.
stops_arrests_matches <- stops_arrests_day_join %>%
   rowwise() %>% 
   mutate(dist = distHaversine(c(long.x, lat.x), c(long.y, lat.y))) %>%
   filter(dist < 152.4)
n_distinct(stops_arrests_matches$arrest_id)

#90% with one arrest per stop, 96% with one stop per arrest.
#pull out one stop per arrest
t1 <- stops_arrests_matches %>%
   group_by(arrest_id) %>%
   summarise(stops_per = n_distinct(stop_id)) %>%
   filter(stops_per == 1)

#single stop per and multi stop per arrest
one_stop_match <- semi_join(stops_arrests_matches, t1, by = "arrest_id")
multi_stops <- anti_join(stops_arrests_matches, t1, by = "arrest_id")

#deal with multi stops per arrest first. We can only have one stop for
#an arrest. order by arrest ID, distance.
#select closest, if same distance, select soonest, 
#if no soonest, select first.
multi_stops <- multi_stops %>%
   arrange(arrest_id, dist) %>%
   group_by(arrest_id) %>%
   filter(dist == min(dist)) %>%
   filter(time_difference == min(time_difference)) %>%
   slice(1)

full_arrest_stop_matches <- bind_rows(one_stop_match, multi_stops)
rm(multi_stops, one_stop_match)

#now we have one stop for each arrest. 
#we can have multiple arrests per stop
#but want to filter out some where the street names are so different,
#they likely aren't the same event.
##If under 100 meters (300 ft), we are saying its a match (often corners of two streets - stop and arrest
#have different streets.)
#if over 100 meters, the stringdist between the street names must be under .2.
#.2 is arbitrary, but we are making the cutoff based on visual comparison of street names
full_arrest_stop_matches <- full_arrest_stop_matches %>%
   mutate(streetx = word(arc_address.x, 1, sep = fixed(',')),
          streety = word(arc_address.y, 1, sep = fixed(','))) %>%
   mutate(string_dist = stringdist(streetx, streety, method = 'jw', p = .1))

full_arrest_stop_matches <- full_arrest_stop_matches %>%
   filter(dist < 100 | (dist > 100 & string_dist <= .2))


#want to mark in stops and in arrests, those stops and arrests that 
#have a match and the corresponding IDs
stop_arrest_match_ids <- full_arrest_stop_matches %>%
   group_by(stop_id, arrest_id) %>%
   summarise(count = n()) %>%
   group_by(stop_id) %>%
   mutate(id = 1:n()) %>%
   ungroup() %>%
   select(-count) 

#join to stops database 
stops_arrests <- stop_arrest_match_ids %>%
   group_by(stop_id) %>%
   summarise(num_arrests = n_distinct(id)) %>%
   mutate(arrest_yn = 1)
stops <- left_join(stops, stops_arrests, by = "stop_id")
rm(stops_arrests)

#join to arrests database
stops_arrests <- stop_arrest_match_ids %>%
   mutate(stop_yn = 1) %>%
   select(-id)

arrests <- left_join(arrests, stops_arrests)
rm(stops_arrests)

###MATCHING CITATIONS AND STOPS###
min(citations$VIOLATIONDATE)
max(citations$VIOLATIONDATE)
min(stops$Response_Date)
max(stops$Response_Date)

#remove the few citations from 2018.
citations <- citations %>%
   filter(year(VIOLATIONDATE) < 2018)

#two years of citations
#arrests join, with seconds variable for time join
citations_join <- citations %>%
   select(citation_id, lat, long, date = VIOLATIONDATE, arc_address) %>%
   filter(!is.na(lat) & !is.na(long)) %>%
   mutate(rounded_date = round_date(date, "day"))

#time join
stops_citations_day_join <- inner_join(citations_join, stops_join, 
                                     by = "rounded_date") %>%
   distinct()

#filter by time - only arrests that occurred within an hour of stop.
stops_citations_day_join <- stops_citations_day_join %>%
   mutate(time_difference = difftime(date.y, date.x, units = "mins")) %>%
   filter(time_difference < 0 & time_difference >= -60)

#filter by distance - only arrests within 500 ft of stop.
stops_citations_matches <- stops_citations_day_join %>%
   rowwise() %>% 
   mutate(dist = distHaversine(c(long.x, lat.x), c(long.y, lat.y))) %>%
   filter(dist < 152.4)

#pull out one stop per citation
t1 <- stops_citations_matches %>%
   group_by(citation_id) %>%
   summarise(stops_per = n_distinct(stop_id)) %>%
   filter(stops_per == 1)

#single stop per and multi stop per citation
one_stop_match <- semi_join(stops_citations_matches, t1, by = "citation_id")
multi_stops <- anti_join(stops_citations_matches, t1, by = "citation_id")

#deal with multi stops per citation first. We can only have one stop for
#a citation. order by citation ID, distance.
#select closest, if same distance, select soonest, 
#if no soonest, select first.
multi_stops <- multi_stops %>%
   arrange(citation_id, dist) %>%
   group_by(citation_id) %>%
   filter(dist == min(dist)) %>%
   filter(time_difference == min(time_difference)) %>%
   slice(1)

full_citation_stop_matches <- bind_rows(one_stop_match, multi_stops)
rm(multi_stops, one_stop_match)

#now we have one stop for each citation. 
#we can have multiple citation per stop
#but want to filter out some where the street names are so different,
#they likely aren't the same event.
##If under 100 meters (300 ft), we are saying its a match (often corners of two streets - stop and arrest
#have different streets.)
#if over 100 meters, the stringdist between the street names must be under .2.
#.2 is arbitrary, but we are making the cutoff based on visual comparison of street names
full_citation_stop_matches <- full_citation_stop_matches %>%
   mutate(streetx = word(arc_address.x, 1, sep = fixed(',')),
          streety = word(arc_address.y, 1, sep = fixed(','))) %>%
   mutate(string_dist = stringdist(streetx, streety, method = 'jw', p = .1)) %>%
   filter(dist < 100 | (dist > 100 & string_dist <= .2))

#want to mark in stops and in citations, those stops and arrests that 
#have a match and the corresponding IDs
stop_citation_match_ids <- full_citation_stop_matches %>%
   group_by(stop_id, citation_id) %>%
   summarise(count = n()) %>%
   group_by(stop_id) %>%
   mutate(id = 1:n()) %>%
   ungroup() %>%
   select(-count) 

#join to stops database 
stops_citations <- stop_citation_match_ids %>%
   group_by(stop_id) %>%
   summarise(num_citations = n_distinct(id)) %>%
   mutate(citations_yn = 1)

stops <- left_join(stops, stops_citations, by = "stop_id")

#join to citations database
stops_citations <- stop_citation_match_ids %>%
   mutate(stop_yn = 1) %>%
   select(-id)

citations <- left_join(citations, stops_citations)
rm(stops_citations_day_join, stops_citations)


###MATCH ARRESTS TO USE OF FORCE####
#pull timestamp from use of force
force <- use_force %>%
   filter(`Cit arrest` == "Yes") %>%
   select(date_time, Key, `Address (sans street #)`) %>%
   mutate(time = as.numeric(seconds(ymd_hms(date_time))))

arrests_join$time <- as.numeric(seconds(ymd_hms(arrests_join$date)))
   
force_match <- difference_inner_join(arrests_join, force, by = c("time"), max_dist = 3600)

force_match <- force_match %>%
   mutate(time_diff = difftime(date, date_time, units = "mins")) %>%
   mutate(time_diff2 = as.numeric(time_diff)) %>%
   filter(time_diff2 > 0) 

force_match <- force_match %>%
   mutate(street_arrest = word(arc_address, 1, sep = fixed(',')),
          street_arrest = sub("^\\S+\\s+", '', street_arrest),
          street_force = `Address (sans street #)`)
          
force_match <- force_match %>%
   mutate(street_force = gsub("\\Road", "Rd", street_force),
   street_force = gsub("\\Avenue", "Ave", street_force),
   street_force = gsub("\\Boulevard", "Blvd", street_force),
   street_force = gsub("\\Street", "St", street_force),
   street_force = gsub("\\Drive", "Dr", street_force),
   street_force = gsub("\\Place", "Pl", street_force),
   street_force = gsub("\\Tulsa", "", street_force),
   street_force = gsub("\\d{5}", "", street_force),
   street_force = gsub("\\.", "", street_force))
             
force_match <- force_match %>%
   mutate(string_dist = stringdist(street_arrest, street_force, method = 'jw', p = .1))

#per force id, closest match in string_dist - handlabelled
#whether streets were actual match
force_match <- read_frozen("force_match_scored")

force_match <- force_match %>%
   filter(match == 1)

stops <- stops %>%
   ungroup() %>%
   distinct()

#stops - either a citation or arrest match
stops <- stops %>%
   mutate(either_match = ifelse(citations_yn == 1 | arrest_yn == 1,
          1, 0))

#Now provide proportions of stops with matches
prop_stops <- stops %>%
   group_by(either_match) %>%
   summarise(count = n_distinct(stop_id)) %>%
   mutate(percentage = count/sum(count))

literal_values$num_total_matches <- as.numeric(prop_stops[1,2])
literal_values$percentage_total_matches <- 
   round(as.numeric(prop_stops[1,3]), 3)

prop_stops <- stops %>%
   group_by(arrest_yn) %>%
   summarise(count = n_distinct(stop_id)) %>%
   mutate(percentage = count/sum(count))

literal_values$stop_arrest_match_count <- as.numeric(prop_stops[1,2])
literal_values$percentage_stops_match_arrest <- 
   round(as.numeric(prop_stops[1,3]), 3)

prop_stops <- stops %>%
   group_by(citations_yn) %>%
   summarise(count = n_distinct(stop_id)) %>%
   mutate(percentage = count/sum(count))

literal_values$stop_citation_match_count <- as.numeric(prop_stops[1,2])
literal_values$percentage_stops_match_citation <- 
   round(as.numeric(prop_stops[1,3]), 3)
rm(prop_stops)

#percentage of arrests with a match in stops
prop_arrests <- arrests %>%
   group_by(stop_yn) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(percentage = count/sum(count))

literal_values$percentage_arrests_match_stop <- 
   round(as.numeric(prop_arrests[1,3]), 3)
rm(prop_arrests)

#percentage of citations with a match in stops
prop_citations <- citations %>%
   group_by(stop_yn) %>%
   summarise(count = n_distinct(citation_id)) %>%
   mutate(percentage = count/sum(count))

literal_values$percentage_citations_match_stop <- 
   round(as.numeric(prop_citations[1,3]), 3)
rm(prop_citations)

#remove addresses and lat/long from files
stops <- stops %>%
   select(-address, -address_clean, -address_clean2, -arc_address,
          -lat, -long)

force_match <- force_match %>%
   select(-lat, -long, -arc_address)

citations <- citations %>%
   select(-VIOLATION_LOCATION, -arc_address, -lat, -long,
          -address_clean, -address_clean2)

arrests <- arrests %>%
   select(-arc_address, -lat, -long,
          -address_clean, -address_clean2, -ARREST_LOCATION)


#copy jail/use of force from processing and save in matching output for public repo
file.copy(here("processing/output/jail.rds"), here("matching/output"), 
          overwrite = TRUE)

file.copy(here("processing/output/deadly_force.rds"), here("matching/output"), 
          overwrite = TRUE)


#write to output
write_rds(stops, outputfiles$stops)
write_rds(arrests, outputfiles$arrests)
write_rds(citations, outputfiles$citations)
write_rds(stop_arrest_match_ids, outputfiles$stop_arrest_match_ids)
write_rds(full_arrest_stop_matches, outputfiles$full_arrest_stop_matches)
write_rds(stop_citation_match_ids, outputfiles$stop_citation_match_ids)
write_rds(full_citation_stop_matches, outputfiles$full_citation_stop_matches)
write_rds(force_match, outputfiles$force_match)
write_rds(literal_values, outputfiles$matching_literal_values)
write_rds(use_force, outputfiles$use_force)

