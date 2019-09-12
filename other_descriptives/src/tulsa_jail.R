#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019
# =========================================
# HRW-us-tulsa/other_descriptives/src/tulsa_jail.R

library(pacman)
p_load(tidyverse, lubridate, readr, scales, stringr, here, assertr,
       sf, leaflet, htmlwidgets, ggplot2, zoo, Hmisc, extrafont)
options(scipen=999)

here <- here::here
font_import() # import all your fonts
fonttable()
fonttable()[90:94,]
loadfonts()

source(here("other_descriptives/src/functions_shared.R"))

#read data
df <- read_rds(here("matching/output/jail.rds"))


#start a list of values for export
jail_literal_values <- list()

#list of output files
outputfiles <- list(
   jail_literal_values = "other_descriptives/output/jail_literal_values.rds"
   )
outputfiles <- map(outputfiles, here)

#dates
jail_literal_values$start_date <- min(df$BookingDate)
jail_literal_values$end_date <- max(df$BookingDate)

#number of bookings and people
jail_literal_values$number_bookings <- n_distinct(df$BookingId)
jail_literal_values$number_people <- n_distinct(df$InmateId)

##people released to ICE
ICE <- df %>%
   filter(ReleaseReason == "873 - ICE Federal Agency")
jail_literal_values$total_ICE_detainees <- n_distinct(ICE$InmateId)

#with ICE holds
most_serious <- ICE %>%
   filter(most_serious_charge == 1) %>%
   group_by(charge_recode4) %>%
   summarise(count = n_distinct(BookingId)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(count))

jail_literal_values$perc_ICE_holds <- as.numeric(round(
   (most_serious[1,3] + most_serious[2,3]), 2) * 100)

jail_literal_values$perc_nonICE_holds <- 100 - jail_literal_values$perc_ICE_holds

#without holds
most_serious <- ICE %>%
   filter(most_serious_charge == 1) %>%
   filter(charge_recode3 != "Hold For Other Agency" &
             charge_recode3 != "Hold/Ice") %>%
   group_by(charge_recode3) %>%
   summarise(count = n_distinct(BookingId)) %>%
   arrange(desc(count)) %>%
   mutate(perc = count/sum(count))

jail_literal_values$num_nonICE_hold <- sum(most_serious$count)

jail_literal_values$num_DUI <- as.numeric(most_serious[1,2])
jail_literal_values$num_DL <- as.numeric(most_serious[2,2])
jail_literal_values$num_battery <- as.numeric(most_serious[3,2])
jail_literal_values$num_intox <- as.numeric(most_serious[4,2] + 
                                               most_serious[9,2] )
jail_literal_values$num_firearm <- as.numeric(most_serious[5,2])
jail_literal_values$num_insura <- as.numeric(most_serious[6,2])
jail_literal_values$num_moving <- as.numeric(most_serious[7,2])
jail_literal_values$num_drug_sale <- as.numeric(most_serious[8,2])

#types
most_serious <- ICE %>%
   filter(most_serious_charge == 1) %>%
   filter(charge_recode3 != "Hold For Other Agency" &
             charge_recode3 != "Hold/Ice") %>%
   group_by(charge_category) %>%
   summarise(count = n_distinct(BookingId)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(count))

jail_literal_values$public_order_perc <- as.numeric(round(
   most_serious[1,3], 2) * 100)
jail_literal_values$violent_perc <- as.numeric(round(
   most_serious[2,3], 2) * 100)

#write out
write_rds(jail_literal_values, outputfiles$jail_literal_values)
