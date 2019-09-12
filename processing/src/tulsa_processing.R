#
# Authors:     BR
# Maintainers: BR, TS
# Copyright:   2019
# =========================================
# HRW-us-tulsa/processing/src/tulsa_processing.R

library(pacman)
p_load(tidyverse, lubridate, readr, stringr, here, assertr, tidycensus, geosphere, uuid)
options(scipen=999)

here <- here::here

########### input and output files ##############
# input files:
inputfiles <- tribble(
    ~name, ~filename,
    "arrests", "import/output/arrests.rds",
    "citations", "import/output/citations.rds",
    "calls_stops", "import/output/calls_stops.rds",
    "use_force", "import/output/use_force.rds",
    "complaints", "import/output/complaints.rds",
    "jail", "import/frozen/jail.rds",
    "offenses", "import/output/offenses.rds"
)


# files that have undergone some manual transformations or hand coding
frozenfiles <- c(
   offenses_recoded     = "processing/frozen/offenses_recoded.csv",
   citations_coded      = "processing/frozen/citations_coded.csv",
   citations_coded2     = "processing/frozen/citations_coded2.csv",
   officers             = "processing/frozen/Officers.csv",
   types_cleaned        = "processing/frozen/force_types_cleaned.csv",
   types_cleaned2       = "processing/frozen/force_types_cleaned2.csv",
   service_coded        = "processing/frozen/force_reason_recoded.csv",
   addresses_to_join    = "processing/frozen/addresses_to_join2.csv",
   crimes               = "processing/frozen/jail_crimes_recoded.csv",
   jail_offenses        = "processing/frozen/jail_offenses_recoded.csv"
)

#output files. 
outputfiles <- list(
   complaints = "processing/output/complaints.rds",
   use_force = "processing/output/use_force.rds",
   calls_stops = "processing/output/calls_stops.rds",
   citations = "processing/output/citations.rds",
   arrests = "processing/output/arrests.rds",
   jail = "processing/output/jail.rds",
   address_to_geocode = "processing/output/address_to_geocode.csv",
   deadly_force = "processing/output/deadly_force.rds"
) %>% map(here)

# code dictionary for UCC codes
uccfiles <- list(
   "import/input/ucr_tabula.csv",
   "processing/frozen/extra_ucc_recoded.csv"
)

#functions to read
read_input <- function(inputfile) {
   inputfiles %>%
      filter(name == inputfile) %>%
      pluck("filename") %>%
      here %>%
      read_rds
}

read_frozen <- function(f) read_csv(here(frozenfiles[f]))

# ucc code -> offense description
ucc <- uccfiles %>% map(here) %>% map_dfr(read_csv)

######### ARREST DATA  ###############
#create unique ID based on non-offense columns
arrests <- read_input("arrests") %>%
   mutate(arrest_id = as.numeric(as.factor(paste(ARREST_DATE, ARREST_LOCATION, AGE, 
                                                 RACE, SEX, ETHNICITY, sep="_"))))

#let's filter out the 2 2018 months, to make it clean
arrests <- arrests %>%
   filter(ARREST_DATE < ymd("2018-01-01"))

#bring out address for compiling of addresses
address <- arrests %>%
   select(ARREST_LOCATION) %>%
   rename(address = ARREST_LOCATION) %>%
   distinct()

# race/gender descriptions
arrests <- arrests %>%
   mutate(race = case_when(
      RACE == "A" ~ "Asian",
      RACE == "B" ~ "Black",
      RACE == "I" ~ "Native American",
      RACE == "U" ~ "Unknown",
      RACE == "W" ~ "White",
      RACE == "H" ~ "Latino/Hispanic",
      is.na(RACE) ~ NA_character_,
      TRUE        ~ "ERROR"
   ),
   gender = case_when(
      SEX == "F" ~ "Female",
      SEX == "M" ~ "Male",
      SEX == "U" ~ "Unknown",
      is.na(SEX) ~ NA_character_,
      TRUE       ~ "ERROR"
   )) %>%
   select(-SEX, -RACE) %>%
   verify(is.na(race) | race != "ERROR") %>%
   verify(is.na(gender) | gender != "ERROR")

#offenses
# raise an error if there are unrecognized UCC codes
arrests <- arrests %>%
   left_join(ucc, by = "UCC_CODE") %>%
   assert(not_na, offense) 
rm(ucc)

#create binary whether the arrest had a warrant charge or not.
arrests <- arrests %>%
   mutate(warrant_offense = offense %in% c("County Warrant/CRF RR CRM JVJ",
                                           "City Warrant/Municipal Court",
                                           "Other Warrants/State/Federal")) %>%
   group_by(arrest_id) %>%
   mutate(warrant_arrest = max(as.integer(warrant_offense))) %>%
   ungroup

#create binary if arrest only had a warrant offense
t <- arrests %>%
   filter((warrant_arrest == 1 & warrant_offense == F) |
         (warrant_arrest == 0))
arrests <- arrests %>%
   mutate(only_warrant_offense = ifelse(arrest_id %in% t$arrest_id,
                                         0, 1))
rm(t)

#offense types
offenses_recoded <- read_frozen("offenses_recoded")
arrests <- left_join(arrests, offenses_recoded, by = "offense") %>%
   assert(not_na, offense_recode) 
rm(offenses_recoded)

#mark most serious offense
arrests <- arrests %>%
   mutate(row = row_number())

t1 <- arrests %>%
   select(arrest_id, row, rank) %>%
   group_by(arrest_id) %>%
   filter(rank == min(rank)) %>%
   slice(c(1)) %>%
   mutate(most_serious_offense = 1)

arrests <- left_join(arrests, t1)
rm(t1)

#recode drug offenses
arrests <- arrests %>%
   mutate(offense_type = ifelse(offense_type == "Drug offense" &
                                   drug == "Sale",
                                "Drug sale",
                                ifelse(offense_type == "Drug offense" &
                                          drug != "Sale",
                                       "Drug possession/use",
                                       offense_type)))

#month of arrest
arrests <- arrests %>%
   mutate(month = floor_date(ARREST_DATE, unit = "months"))

######### CITATIONS DATA  ###############
citations <- read_input("citations")

#did not code every citation, but the vast bulk
citations_coded <- read_frozen("citations_coded") %>%
   select(-count, -perc, -cumperc)

citations <- left_join(citations, citations_coded) 
rm(citations_coded)

#bring in second round of citations recodes
cites_codes <- read_frozen("citations_coded2")
citations <- left_join(citations, cites_codes, by = c("CHARGETITLE", "CHARGESECTION"))

citations <- citations %>%
   mutate(citation = ifelse(!is.na(citation.x), citation.x, citation.y)) %>%
   mutate(citation = ifelse(is.na(citation), "Other violation", citation)) %>%
   select(-citation.x, -citation.y)

#better race/gender
citations <- citations %>%
   mutate(race = case_when(
      is.na(RACE) ~ NA_character_,
      RACE == "A" ~ "Asian",
      RACE == "B" ~ "Black",
      RACE == "I" ~ "Native American",
      RACE == "U" ~ "Unknown",
      RACE == "W" ~ "White",
      RACE == "H" ~ "Latino/Hispanic",
      TRUE        ~ "ERROR"
   ),
   gender = case_when(
      is.na(SEX) ~ NA_character_,
      SEX == "F" ~ "Female",
      SEX == "M" ~ "Male",
      SEX == "U" ~ "Unknown",
      TRUE       ~ "ERROR"
   )) %>%
   select(-SEX, -RACE) %>%
   verify(is.na(race) | race != "ERROR") %>%
   verify(is.na(gender) | gender != "ERROR")

citations <- citations %>%
   mutate(race = ifelse(is.na(race), "Unknown", race))

#better ID for citations based on time/location/gender/race, then remove paste id because
#of location data.
citations <- citations %>%
   mutate(citation_id_test = paste(VIOLATIONDATE, VIOLATION_LOCATION, gender, race, sep = "_")) %>%
   mutate(citation_id = group_indices(., citation_id_test)) %>%
   select(-citation_id_test)

#pull in race of officers
officers <- read_frozen("officers") %>%
   select(-count, -perc, -cumperc)
citations <- left_join(citations, officers)
rm(officers)

#addresses to add to list
address_bind <- citations %>%
   select(VIOLATION_LOCATION) %>%
   rename(address = VIOLATION_LOCATION)
address <- bind_rows(address, address_bind)
rm(address_bind)

######### COMPLAINTS DATA  ###############
complaints <- read_input("complaints")
complaints$Date <- as.Date(complaints$Date, format = "%d-%b-%y")

######### USE OF FORCE DATA  ###############
use_force <- read_input("use_force")

#deal with date/time
use_force <- use_force %>%
   mutate(Date = gsub(",", "/01", Date),
          Date = gsub("2012", "12", Date),
          Date = gsub("2013", "13", Date),
          Date = gsub("2014", "14", Date),
          Date = gsub("2015", "15", Date),
          Date = gsub("2016", "16", Date),
          Date = gsub("2017", "17", Date),
          Date = as.Date(Date, format = "%m/%d/%y"),
          date_time = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))

#pull zip code out
use_force <- use_force %>%
   mutate(zip = str_extract(`Address (sans street #)`, "\\d{5}"))

# types table - additional re-coding of types done
types_force <- use_force %>%
   select(Key, `Force Type`) %>%
   separate_rows(`Force Type`, sep = ",")

types_force <- types_force %>%
   mutate(`Force Type` = str_squish(`Force Type`))

#Bring in handcoding of types 
types_cleaned <- read_frozen("types_cleaned") %>%
   select(-count,-perc) 

types_force <- left_join(types_force, types_cleaned) %>%
   assert(not_na, force_comma)

types_key <- types_force %>%
   select(Key, force_comma) %>%
   separate_rows(force_comma, sep = ",")
rm(types_cleaned)

#Read in additional re-coding of force types
types_cleaned2 <- read_frozen("types_cleaned2") %>%
   select(-count) %>%
   distinct()

types_key <- types_key %>%
   rename(force_type = force_comma)

types_key <- left_join(types_key, types_cleaned2)

rm(types_cleaned2, types_force)

#joining to use_force
types_key <- types_key %>%
   select(-force_type)

use_force <- left_join(use_force, types_key)

#fix injury
use_force <- use_force %>%
   mutate(cit_injury = ifelse(grepl('Yes', `Cit injury`), 1, 0),
          off_injury = ifelse(grepl('Yes', `Off injury`), 1, 0))

#race recode
use_force <- use_force %>%
   mutate(race = ifelse(grepl('Native', `Cit demo`), "Native American",
                        ifelse(grepl('Hispanic', `Cit demo`), "Latino/Hispanic",
                               ifelse(grepl('White', `Cit demo`), "White",
                                      ifelse(grepl('Black', `Cit demo`), "Black",
                                             "Unknown")))))

#group strikes together
use_force <- use_force %>%
   mutate(force_type_cleaned2 = 
             ifelse(grepl('Strike', force_type_cleaned), "Strike", force_type_cleaned))

#read in hand-recoded service rendered
service_recoded <- read_frozen("service_coded")

use_force <- left_join(use_force, service_recoded)
rm(service_recoded)

#remove citizen name
use_force <- use_force %>%
   select(-`Cit name`)

######### CALLS/STOPS DATA  ###############
calls_stops <- read_input("calls_stops")

#recode stop reason
calls_stops <- calls_stops %>%
   mutate(reason = ifelse(Problem == "*Traffic Stop", "Traffic Stop", Problem),
          reason = ifelse(Problem == "*Pedestrian Check", "Pedestrian Check",
                           reason),
          reason = ifelse(Problem == "*Out At", "Out At",
                           reason),
          reason = ifelse(Problem == "*Off Duty Job", "Off Duty Job",
                           reason))

#columns for length of stop 
calls_stops <- calls_stops %>%
   mutate(hours = difftime(Time_CallClosed, Response_Date, units = c("hours")),
          mins = difftime(Time_CallClosed, Response_Date, units = c("mins")))

#rename id
calls_stops <- calls_stops %>%
   rename(stop_id = Master_Incident_Number)

#save for linking zip after we have all of the addresses.
zip_codes <- calls_stops %>%
   mutate(address_clean = str_to_title(Address)) %>%
   group_by(address_clean, Postal_Code) %>%
   summarise(count = n()) %>%
   arrange(desc(count)) %>%
   select(-count) %>%
   filter(!is.na(Postal_Code)) %>%
   distinct()

#add calls addresses to the address list
address_bind <- calls_stops %>%
   select(address = Address)

address <- bind_rows(address, address_bind)
rm(address_bind)

#str to title on others, link and remove leading zeros
address <- address %>%
   mutate(address_clean = gsub("(?<![0-9])0+", "", address, perl = TRUE)) %>%
   mutate(address_clean = str_to_title(address_clean)) 

address <- left_join(address, zip_codes, by = "address_clean")
rm(zip_codes)

address <- address %>%
   mutate(city = "Tulsa", state = "OK") %>%
   rename(zip_code = Postal_Code) %>%
   distinct()  %>%
   mutate(row_id = row_number())

address <- address %>%
   mutate(address_clean2 = gsub("Hwy51", "State Highway 51", address_clean),
          address_clean2 = gsub("Hw51", "State Highway 51", address_clean2),
          address_clean2 = gsub(" Ba ", " State Highway 51", address_clean2),
          address_clean2 = gsub("Hw11", "State Highway 11", address_clean2),
          address_clean2 = gsub("Hwy11", "State Highway 11", address_clean2),
          address_clean2 = gsub("Hy11", "State Highway 11", address_clean2),
          address_clean2 = gsub("Sidl", "South Inner Dispersal Loop Expressway", address_clean2),
          address_clean2 = gsub("Nidl", "North Inner Dispersal Loop Expressway", address_clean2),
          address_clean2 = gsub("Widl", "West Inner Dispersal Loop Expressway", address_clean2),
          address_clean2 = gsub("Eidl", "East Inner Dispersal Loop Expressway", address_clean2),
          address_clean2 = gsub("Hiway75", "US-75", address_clean2),
          address_clean2 = gsub("Hwy75", "US-75", address_clean2),
          address_clean2 = gsub("Hwy 75", "US-75", address_clean2), 
          address_clean2 = gsub("Hw75", "US-75", address_clean2),
          address_clean2 = gsub("Hwy412", "US-412", address_clean2),
          address_clean2 = gsub("Hwy 412 ", "US-412", address_clean2),
          address_clean2 = gsub("Hw412 ", "US-412", address_clean2),
          address_clean2 = gsub("Hw266", "OK-266", address_clean2),
          address_clean2 = gsub("E I244", "I-244 E", address_clean2),
          address_clean2 = gsub("I244", "I-244", address_clean2),
          address_clean2 = gsub("Hwyi244", "I-244", address_clean2),
          address_clean2 = gsub("Hw244", "I-244", address_clean2),
          address_clean2 = gsub("Hwy244", "I-244", address_clean2),
          address_clean2 = gsub("E 244 I", "I-244 E", address_clean2),
          address_clean2 = gsub("E 244", "I-244 E", address_clean2),
          address_clean2 = gsub("E I44", "I-44 E", address_clean2),
          address_clean2 = gsub("E  I44", "I-44 E", address_clean2),
          address_clean2 = gsub("W 44 I", "I-44 W", address_clean2),
          address_clean2 = gsub("Hwy44", "I-44", address_clean2),
          address_clean2 = gsub("Hwy I44", "I-44", address_clean2),
          address_clean2 = gsub("Hwy144", "I-44", address_clean2),
          address_clean2 = gsub("Hwyi44", "I-44", address_clean2),
          address_clean2 = gsub("44 I", "I-44", address_clean2),
          address_clean2 = gsub("I44 Hw", "I-44", address_clean2),
          address_clean2 = gsub("I44", "I-44", address_clean2),
          address_clean2 = gsub("S Hwy 169", "US-169 S", address_clean2),
          address_clean2 = gsub("N Hwy 169", "US-169 N", address_clean2),
          address_clean2 = gsub("Hw169", "US-169", address_clean2),
          address_clean2 = gsub("Hy169", "US-169", address_clean2),
          address_clean2 = gsub("Hwy169", "US-169", address_clean2),
          address_clean2 = gsub("Hw64", "US-64", address_clean2),
          address_clean2 = gsub("Hwy64", "US-64", address_clean2),
          address_clean2 = gsub("Hy6451", "US-64", address_clean2),
          address_clean2 = gsub("Hwy6451", "US-64", address_clean2),
          address_clean2 = gsub("Hwy 64-51", "US-64", address_clean2),
          address_clean2 = gsub("Stlouis", "St Louis", address_clean2),
          address_clean2 = gsub("Sanitlouis", "St Louis", address_clean2),
          address_clean2 = gsub("Stlewis", "St Louis", address_clean2),
          address_clean2 = gsub("Santafe", "Santa Fe", address_clean2),
          address_clean2 = gsub("Gilcreasemuseum", "Gilcrease Museum", address_clean2),
          address_clean2 = gsub("Gilcreasemus", "Gilcrease Museum", address_clean2),
          address_clean2 = gsub("Gilcreasemes", "Gilcrease Museum", address_clean2),
          address_clean2 = gsub("Gilcreasemue", "Gilcrease Museum", address_clean2),
          address_clean2 = gsub("Civiccenter", "Civic Center", address_clean2),
          address_clean2 = gsub("Civiccneter", "Civic Center", address_clean2),
          address_clean2 = gsub("Matthewbrady", "Mathew Brady", address_clean2),
          address_clean2 = gsub("Expy", "", address_clean2),
          address_clean2 = gsub(" Ea ", " E ", address_clean2),
          address_clean2 = gsub("Ex", "", address_clean2),
          address_clean2 = gsub("Sb", "", address_clean2),
          address_clean2 = gsub("sb", "", address_clean2),
          address_clean2 = gsub("S B", "", address_clean2),
          address_clean2 = gsub("Nb", "", address_clean2),
          address_clean2 = gsub("nb ", "", address_clean2),
          address_clean2 = gsub("Eb", "", address_clean2),
          address_clean2 = gsub("eb", "", address_clean2),
          address_clean2 = gsub("wb", "", address_clean2),
          address_clean2 = gsub("Wb", "", address_clean2),
          address_clean2 = gsub("exp", "", address_clean2),
          address_clean2 = gsub("ex", "", address_clean2),
          address_clean2 = gsub("Ex", "", address_clean2),
          address_clean2 = gsub("Countryclub", "Country Club", address_clean2),
          address_clean2 = gsub("Martinluther", "Martin Luther King Jr", address_clean2),
          address_clean2 = gsub("Mlkjr Vl", "Martin Luther King Jr Bl", address_clean2),
          address_clean2 = gsub("Martinlutherking Bm", "Martin Luther King Jr Bl", address_clean2),
          address_clean2 = gsub("Brokenarrow", "Broken Arrow", address_clean2),
          address_clean2 = gsub("Portroad", "Port Rd", address_clean2),
          address_clean2 = gsub("Lltisdale", "LL Tisdale", address_clean2),
          address_clean2 = trimws(address_clean2),
          address_clean2 = str_squish(address_clean2))

#create another id just for the clean addresses
address_clean_id <- address %>%
   select(address_clean2) %>%
   distinct() %>%
   mutate(id2 = row_number() + 300000)

address <- left_join(address, address_clean_id)
rm(address_clean_id)

#id variable for first address clean
address_clean_id <- address %>%
   select(address_clean) %>%
   distinct() %>%
   mutate(id3 = row_number() + 600000)

address <- left_join(address, address_clean_id)

#output address  for Geo-coding
address_out <- address %>%
   select(raw_original_address = address,
          clean_address_to_geocode = address_clean2,
          id2, id3) %>%
   distinct()
write_csv(address_out, outputfiles$address_to_geocode)
rm(address, address_clean_id)

#bring in geocoded addresses
addresses_to_join <- read_frozen("addresses_to_join") %>%
   mutate(GEOID = as.character(GEOID))

#join to datasets with "address"
arrests <- left_join(arrests, addresses_to_join, by = c("ARREST_LOCATION" = "address"))
calls_stops <- calls_stops %>%
   rename(address = Address)
calls_stops <- left_join(calls_stops, addresses_to_join)
citations <- left_join(citations, addresses_to_join, by = c("VIOLATION_LOCATION" = "address"))

#creating variables that mark several specific addresses for future analysis.
#doing this in processing because we want to remove addresses from public use data.

#garnett hotel
arrests <- arrests %>%
   mutate(garnett = ifelse(grepl('S Garnett', arc_address) &
                              grepl('1011', arc_address), 1, 0)) 

#near garnett hotel (within .25 mi)
near_garnett <- arrests %>%
   rowwise() %>%
   mutate(dist = distHaversine(c(-95.85113, 36.14913), c(long, lat))) %>%
   filter(dist < 402.4)

arrests <- arrests %>%
   mutate(near_garnett = ifelse(arrests$arrest_id %in% near_garnett$arrest_id, 1, 0))
rm(near_garnett)

#downtown jail
arrests <- arrests %>%
   mutate(jail = ifelse(arc_address == "300 N Denver Ave", 1, 0))

#walmart
arrests <- arrests %>%
   mutate(walmart = ifelse(arc_address == "207 S Memorial Dr", 1, 0))

#Top 3 locations for citations
top3 <- citations %>%
   filter(!is.na(arc_address)) %>%
   group_by(arc_address) %>%
   summarise(count = n_distinct(citation_id)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(count)) %>%
   slice(1:3)

citations <- citations %>%
   mutate(top_3_location = ifelse(citations$arc_address %in% top3$arc_address, 1, 0))
rm(top3)

#make a geocoded y/n variable for stops
calls_stops <- calls_stops %>%
   mutate(geocoded = ifelse(is.na(lat), 0, 1))

######### JAIL DATA  ###############
jail <- read_input("jail")

jail <- jail %>%
   mutate(row_id = row_number())

#read in hand re-coded crimes
crimes <- read_frozen("crimes")

crimes <- crimes %>%
   mutate(rank = case_when(
      charge_category == "Violent/Potentially violent offense" ~	1,
      charge_category == "Weapon" ~ 2,
      charge_category == "Theft/Property offense" ~	4,
      charge_category == "Non-violent sex offense" ~	3,
      charge_category == "Public order offense" ~	6,
      charge_category == "Drug sale" ~	5,
      charge_category == "Drug possession" ~	7,
      charge_category == "Warrant" ~	8,
      charge_category == "Status offense" ~	9,
      charge_category == "Traffic offense" ~	10,
      charge_category == "Hold" ~	11,)) %>%
   select(-count, -perc, -cumperc)

jail <- left_join(jail, crimes) %>%
   assert(not_na, charge_recode2)


#more recoding
jail_offenses <- read_frozen("jail_offenses")

jail_offenses <- jail_offenses %>%
   mutate(charge_recode4 = ifelse(is.na(charge_recode4),
                                  charge_recode2,
                                  charge_recode4)) %>%
   select(-charge_category)

jail <- left_join(jail, jail_offenses, by = c("charge_recode2")) 

t1 <- jail %>%
   group_by(BookingId) %>%
   arrange(rank) %>%
   slice(c(1)) %>%
   mutate(most_serious_charge = 1) %>%
   select(row_id, most_serious_charge)

jail <- left_join(jail, t1)

rm(t1, crimes)

#fixing a piece of bad processing
jail <- jail %>%
   group_by(BookingId) %>%
   mutate(charges = n_distinct(ChargeSequence))

jail <- jail %>%
   mutate(num_charges = ifelse(charges >= 5 & charges <= 10, "5-10",
                               ifelse(charges >= 10, ">10", as.character(charges))))

#deadly force
deadly_force <- read_csv(here("import/frozen/deadly_force.csv"))

#save 
saveRDS(complaints, outputfiles$complaints)
saveRDS(use_force, outputfiles$use_force)
saveRDS(calls_stops, outputfiles$calls_stops)
saveRDS(citations, outputfiles$citations)
saveRDS(arrests, outputfiles$arrests)
saveRDS(jail, outputfiles$jail)
saveRDS(deadly_force, outputfiles$deadly_force)
