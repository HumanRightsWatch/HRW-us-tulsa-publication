#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019
# =========================================
# HRW-us-tulsa/other_descriptives/src/tulsa_use_force_other_descriptives.R

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
use_force <- read_rds(here("processing/output/use_force.rds"))
tulsa_city_census_long <- read_rds(here("census_data/output/city_census_long.rds"))
zip_census <- read_rds(here("census_data/output/zips.rds"))
arrests <- read_rds(here("matching/output/arrests.rds"))
arrest_rates <- read_csv(here("other_descriptives/output/arrests_methodology__andforce_table.csv"))
deadly <- read_csv(here("import/frozen/deadly_force.csv"))
force_matches <- read_rds(here("matching/output/force_arrests_matches.rds"))

#start a list of values for export
literal_values <- list()

#list of output files
outputfiles <- list(
   force_literal_values = "other_descriptives/output/force_values.rds",
   table_force_rates = "other_descriptives/output/table_force_rates.rds",
   correlation_force_non_white_plot = "other_descriptives/output/correlation_plot_force_race.pdf",
   trends_force_plot = "other_descriptives/output/trends_force_plot.pdf",
   trends_for_force_plot = "other_descriptives/output/trends_for_force_plot.rds",
   zip_force = "other_descriptives/output/zip_force.rds"
)
outputfiles <- map(outputfiles, here)

#dates of Use of Force
literal_values$min_force_date <- min(use_force$Date)
literal_values$max_force_date <- max(use_force$Date)

#use of force incidents/actions
literal_values$total_num_force_incidents <- n_distinct(use_force$Key)

#unique use of force actions
literal_values$total_num_force_actions <- nrow(use_force)

#num per month
use_force <- use_force %>%
   mutate(month = floor_date(Date, unit = "months"))

month <- use_force %>%
   group_by(month) %>%
   summarise(count = n_distinct(Key))
literal_values$median_force_per_month <- median(month$count)
rm(month)


#race and force
#per capita
#race

#because latino/hispanic is included in white, we must adjust here as well
use_force <- use_force %>%
   mutate(race2 = ifelse(race == "Latino/Hispanic", "White", race))

race <- use_force %>%
   group_by(race2) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count)) %>%
   rename(race = race2)

#city wide rates
tulsa_census2 <- tulsa_city_census_long %>%
   filter(method == "Method 2 - multi-race recoded as single race") %>%
   rename(race = race2)

tulsa_rates <- left_join(race, tulsa_census2)

tulsa_rates <- tulsa_rates %>%
   mutate(annual_rate_per_1K_capita = round((count/(6*number))
                              * 1000, 2)) %>%
   select(race, annual_rate_per_1K_capita)

#per arrest
race <- left_join(race, arrest_rates)

race <- race %>%
   mutate(rate_per_1K_arrests = round((count/(num_arrests)) * 1000, 2),
          arrests_per_force_incident = round((num_arrests/count), 0)) 

black_white_ratios <- left_join(race, tulsa_rates) %>%
   mutate(ratio_per_capita = annual_rate_per_1K_capita/
             annual_rate_per_1K_capita[race == "White"],
          ratio_per_arrest = rate_per_1K_arrests/
             rate_per_1K_arrests[race == "White"],
          diff_per_arrest = rate_per_1K_arrests[race == "Black"] -
             rate_per_1K_arrests[race == "White"],
          num_arrest_per_1_increased_force = round(1000/diff_per_arrest, 0))

literal_values$percent_of_force_black <- round(as.numeric(black_white_ratios[1,3]), 2) * 100
literal_values$percent_of_force_white <- round(as.numeric(black_white_ratios[4,3]), 2) * 100
literal_values$annual_rate_per_1K_capita_white <- as.numeric(black_white_ratios[4,12])
literal_values$annual_rate_per_1K_capita_black <- as.numeric(black_white_ratios[1,12])
literal_values$rate_per_1K_arrests_white <- 
   as.numeric(black_white_ratios[4,10])
literal_values$rate_per_1K_arrests_black <-
   as.numeric(black_white_ratios[1,10])
literal_values$ratio_per_capita <- round(as.numeric(black_white_ratios[1,13]), 1)
literal_values$ratio_per_arrest <- round(as.numeric(black_white_ratios[1,14]), 1)
literal_values$percent_diff_rate_arrest <- 
   round(((literal_values$rate_per_1K_arrests_black - 
             literal_values$rate_per_1K_arrests_white)/
            literal_values$rate_per_1K_arrests_white), 2) * 100
literal_values$num_arrests_per_force_white <- as.numeric(black_white_ratios[4,11])
literal_values$num_arrests_per_force_black <- as.numeric(black_white_ratios[1,11])
 
#save table for markdown 
table_force_race <- black_white_ratios %>%
   mutate(perc = paste(100* round(perc, 2), "%", sep = ""),
          Tulsa_City_Percent = paste(100* round(Tulsa_City_Percent, 2), "%", sep = "")) %>%
   select(Race = race, `Use of force incidents` = count,
          `Percent of use of force incidents` = perc,
          `Percent of Tulsa population` = Tulsa_City_Percent,
         `Rate of force per 1,000 arrests` = rate_per_1K_arrests,
         `Rate of force per 1,000 residents per year` = annual_rate_per_1K_capita,
         `Number of arrests per use of force incident` = arrests_per_force_incident)

write_rds(table_force_race, outputfiles$table_force_rates)

rm(black_white_ratios, race, tulsa_rates, table_force_race)

#Analysis of location of use of force incidents
#proportion of incidents with zip data
zip <- use_force %>%
   filter(year(Date) > 2015) %>%
   group_by(zip) %>%
   summarise(count = n_distinct(Key)) %>%
   ungroup() %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(count))
literal_values$perc_with_zip <- as.numeric(round(100 * (1 - zip[1,3]), 0))
rm(zip)

use_force$zip <- as.integer(use_force$zip)

t1 <- use_force %>%
   filter(!is.na(zip)) %>%
   filter(year(Date) > 2015) %>%
   group_by(zip) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count))

zip <- left_join(t1, zip_census)

t1 <- zip %>%
   group_by(zip) %>%
   summarise(pop = sum(one_race_pop)) %>%
   mutate(perc_of_total_pop = pop/sum(pop, na.rm = T)) %>%
   select(-pop)

zip <- left_join(zip, t1)

#rate is per 2 years (2016 and 17)
zip <- zip %>%
   mutate(perc_non_white = 1 - perc_white,
          rate_use_force = count/(one_race_pop*2) * 1000) %>%
   #filter non-Tulsa zip
   filter(zip != 74001)

#r value - correlation of non-white pop and per capita rate of force use
literal_values$cor_force_rate_non_white_pop <- 
   round(cor.test(zip$rate_use_force, zip$perc_non_white)$estimate, 3)

#plot correlation
plot <- ggplot(zip, aes(x = perc_non_white, y = rate_use_force)) +
   geom_point(alpha = 0.9, shape = 20, size = 4) + 
   theme_agile() +
   theme(legend.position = "none") +
   geom_smooth(method = 'lm') +
   scale_x_continuous(label = percent) +
   expand_limits(x = 0) +
   labs(x = "Proportion of zip code population that is non-white",
        y = "Use of force incidents per 1,000 people (2016-2017)",
        title = "Non-firearm Use of Force and the Non-white Population",
        subtitle = paste("Moderate to strong correlation (r = ",  literal_values$cor_force_rate_non_white_pop,
                         ")", sep = ""),
        caption = "Source: Human Rights Watch analysis of Tulsa Police Department data.")
ggsave(plot, file = outputfiles$correlation_force_non_white_plot, 
       units="in", width = 8, height = 6, dpi = 600)
embed_fonts(outputfiles$correlation_force_non_white_plot)

rm(t1)

#type of force used as percent of actions
perc_type <- use_force %>%
   group_by(force_type_cleaned) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(count))

literal_values$control_hold_perc = round(as.numeric(perc_type[2,3]), 2) * 100
literal_values$pepper_perc = round(as.numeric(perc_type[3,3]), 2) * 100
literal_values$k9_perc = round(as.numeric(perc_type[4,3]), 2) * 100
literal_values$k9_number = as.numeric(perc_type[4,2])
literal_values$taser_perc = round(as.numeric(perc_type[1,3]), 2) * 100
literal_values$taser_number = as.numeric(perc_type[1,2])
rm(perc_type)

#examine types of force by race
#rates of race by type - each type of force used is only counted once per incident.
race_type <- use_force %>%
   group_by(race = race2, force_type_cleaned2) %>%
   summarise(count = n_distinct(Key)) 

race_type <- left_join(race_type, tulsa_census2)

race_type <- race_type %>%
   mutate(type_rate_per_capita =  count/(6 * number) * 1000) %>%
   arrange(force_type_cleaned2, desc(type_rate_per_capita)) %>%
   filter(force_type_cleaned2 == "K-9" |
             force_type_cleaned2 == "ECD (e.g. Taser)" |
             force_type_cleaned2 == "OC (pepper) Spray") %>%
   select(race, count, force_type_cleaned2, type_rate_per_capita) 

arrest_join <- arrest_rates %>%
   select(race, num_arrests)

#per arrest
race_per_arrests <- left_join(race_type, arrest_join) %>%
   select(race, force_type_cleaned2, count, num_arrests, type_rate_per_capita) %>%
   mutate(type_rate_per_1K_arrests = round((count/(num_arrests)) * 1000, 2)) %>%
   group_by(force_type_cleaned2) %>%
   mutate(ratio_per_arrest = (type_rate_per_1K_arrests/
                                 type_rate_per_1K_arrests[race == "White"]),
          ratio_per_capita = type_rate_per_capita/
             type_rate_per_capita[race == "White"])

literal_values$ratio_per_capita_k9 <- round(as.numeric(race_per_arrests[5,8]), 1)
literal_values$ratio_per_arrest_k9 <- round(as.numeric(race_per_arrests[5,7]), 1)
literal_values$ratio_per_capita_pepper <- round(as.numeric(race_per_arrests[9,8]), 1)
literal_values$ratio_per_arrest_pepper <- round(as.numeric(race_per_arrests[9,7]), 1)
literal_values$ratio_per_capita_taser <- round(as.numeric(race_per_arrests[1,8]), 1)
literal_values$ratio_per_arrest_taser <- round(as.numeric(race_per_arrests[1,7]), 1)
literal_values$rate_taser_white_per_capita <- round(as.numeric(race_per_arrests[2,5]), 2)
literal_values$rate_taser_black_per_capita <- round(as.numeric(race_per_arrests[1,5]), 2)

rm(arrest_join, race_per_arrests, race_type)   


#injuries
#percent of incidents with an injury
inj <- use_force %>%
   group_by(cit_injury) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count))

literal_values$perc_with_injury = round(as.numeric(inj[2,3]), 2) * 100
rm(inj)

#percent with officer injury
inj <- use_force %>%
   group_by(off_injury) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count)) 

literal_values$perc_with_officer_injury = round(as.numeric(inj[2,3]), 2) * 100
rm(inj)

#type of force most often associated with an injury
inj <- use_force %>%
   filter(cit_injury == 1) %>%
   group_by(force_type_cleaned) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(count))

#percentage of each type where a citizen was injured
inj <- use_force %>%
   group_by(force_type_cleaned, cit_injury) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(cit_injury == 1) %>%
   arrange(desc(count))
rm(inj)



#what were police doing when the incident occurred?
service <- use_force %>%
   group_by(service_recoded) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(perc))

literal_values$service_domestic = round(as.numeric(service[1,3]), 2) * 100
literal_values$service_disturbance = round(as.numeric(service[2,3]), 2) * 100
literal_values$service_burglary = round(as.numeric(service[3,3]), 2) * 100
literal_values$service_traffic = round(as.numeric(service[4,3]), 2) * 100
literal_values$service_warrant = round(as.numeric(service[5,3]), 2) * 100
literal_values$service_ped = round(as.numeric(service[6,3]), 2) * 100
rm(service)

#reason
force_reason <- use_force %>%
   group_by(`UOF reason`) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(perc))

literal_values$reason_resist = round(as.numeric(force_reason[1,3]), 2) * 100
literal_values$reason_elude = round(as.numeric(force_reason[2,3]), 2) * 100
literal_values$reason_assault = round(as.numeric(force_reason[6,3]), 2) * 100
rm(force_reason)

#arrest made
arrested <- use_force %>%
   group_by(`Cit arrest`) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(grepl("Yes", `Cit arrest`))
literal_values$percent_arrested <- round(as.numeric(sum(arrested$perc)), 2)*100
rm(arrested)

#match arrests
match <- force_matches %>%
   select(Key, match)
use_force <- left_join(use_force, match)

#% matched
match <- use_force %>%
   group_by(match) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count))
literal_values$number_matched <- as.numeric(match[1,2])
rm(match)

arrests_force <- semi_join(arrests, force_matches, by = "arrest_id")

#let's look only at most serious charges.
serious_offenses <- arrests_force %>%
   filter(most_serious_offense == 1) %>%
   group_by(offense_recode) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(perc))

literal_values$arrest_disorderly = round(as.numeric(serious_offenses[1,3]), 2) * 100
literal_values$arrest_assault = round(as.numeric(serious_offenses[2,3]), 2) * 100
literal_values$arrest_intox = round(as.numeric(serious_offenses[3,3]), 2) * 100
literal_values$arrest_weapon = round(as.numeric(serious_offenses[4,3]), 2) * 100
literal_values$arrest_other_assault = round(as.numeric(serious_offenses[5,3]), 2) * 100
rm(serious_offenses)

#Rates of force per arrest for subset of matched offenses - by race/arrest type
offenses <- arrests_force %>%
   filter(most_serious_offense == 1) %>%
   group_by(offense_type, race) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = count/sum(count))

offenses_total_arrests <- arrests %>%
   filter(most_serious_offense == 1) %>%
   group_by(offense_type, race) %>%
   summarise(count_arrests = n_distinct(arrest_id)) 

offenses <- left_join(offenses, offenses_total_arrests)

offenses <- offenses %>%
   mutate(per_arrest = count/count_arrests) %>%
   select(offense_type, race, count, per_arrest) %>%
   filter(race == "Black" | race == "White") %>%
   mutate(ratio_per_arrest = (per_arrest/
                                 per_arrest[race == "White"]))

rm(offenses)

#within policy
policy  <- use_force %>%
   group_by(`Within policy`) %>%
   summarise(count = n_distinct(Key)) %>%
   mutate(perc = count/sum(count))
rm(policy)


#deadly force
#processing
deadly <- deadly %>%
   filter(!is.na(Key))
deadly$Date <- as.Date(deadly$Date, format = "%m/%d/%y")
deadly$date_time <- as.POSIXct(paste(deadly$Date, 
                                     deadly$Time), 
                               format="%Y-%m-%d %H:%M:%S", tz="UCT")

deadly_full <- deadly
deadly <- deadly %>%
   filter(year(Date) > 2013)

#number of incidents
literal_values$num_deadly_force <- as.numeric(nrow(deadly_full))

#how many arrested, etc.
arrested <- deadly_full %>%
   group_by(`Citizen arrested`) %>%
   summarise(count =n_distinct(Key)) %>%
   filter(grepl("Yes", `Citizen arrested`))
literal_values$deadly_force_num_arrested <- as.numeric(sum(arrested$count))
rm(arrested)

#service rendered
service <- deadly_full %>%
   group_by(`Service rendered`) %>%
   summarise(count =n_distinct(Key)) %>%
   mutate(count/sum(count)) %>%
   arrange(desc(count))

literal_values$deadly_disturbance <- as.numeric(service[1,2])
literal_values$deadly_traffic <- as.numeric(service[2,2])
literal_values$deadly_robbery <- as.numeric(service[3,2])
literal_values$deadly_warrants <- as.numeric(service[5,2])
literal_values$deadly_autotheft <- as.numeric(service[6,2])
literal_values$deadly_pedstop <- as.numeric(service[8,2])

rm(service)

#race
race <- deadly_full %>%
   group_by(`Cit demo`) %>%
   summarise(count =n_distinct(Key)) %>%
   mutate(perc = count/sum(count))

black_perc <- race %>%
   filter(grepl("Black", `Cit demo`))

literal_values$black_perc_of_deadly <- round(as.numeric(sum(black_perc$perc)), 2) * 100
rm(black_perc)

#plot trends in use of tasers and dogs
trends <- use_force %>%
   filter(force_type_cleaned2 == "ECD (e.g. Taser)" | 
             force_type_cleaned2 == "K-9") %>%
   mutate(year = year(Date)) %>%
   group_by(year, force_type_cleaned2) %>%
   summarise(count = n_distinct(Key))

arrests_per_year <- arrests %>%
   mutate(year = year(ARREST_DATE)) %>%
   group_by(year) %>%
   summarise(number_arrests = n_distinct(arrest_id))

trends <- left_join(trends, arrests_per_year) %>%
   mutate(force_per_arrest = count/number_arrests * 1000) %>%
   arrange(force_type_cleaned2, year)

trends1 <- trends %>%
   select(year, force_type_cleaned2, num = count) %>%
   mutate(cat = "Number of Incidents")
trends2 <- trends %>%
   select(year, force_type_cleaned2, num = force_per_arrest) %>%
   mutate(cat = "Force used per 1,000 arrests") 
trends_joined <- bind_rows(trends1, trends2)

#write data for markdown
write_rds(trends_joined, outputfiles$trends_for_force_plot)

plot <- ggplot(trends_joined, aes(x = year, y = num)) +
   geom_line() + geom_point() +
   theme_agile() +
   facet_wrap(cat ~ force_type_cleaned2, scales = "free") +
   expand_limits(y = 0) +
   scale_y_continuous(name = "",  limits = c(),
                      breaks = pretty_breaks()) +
   #theme(axis.text.y=element_blank()) +
   theme(legend.title=element_blank()) +
   theme(strip.text.y = element_text(size = 8)) +
   #theme(axis.text.x = element_text(angle=45, vjust = .5, hjust = .5)) +
   theme(legend.position = "bottom") +
   labs(x = "",
        title = "Increased Taser and K-9 Use", 
        subtitle = "",
        caption = "Source: Human Rights Watch analysis of Tulsa Police Department data.")
ggsave(plot, file = outputfiles$trends_force_plot, 
       units="in", width = 8, height = 6, dpi = 600)
embed_fonts(outputfiles$trends_force_plot)



#Save literal values
saveRDS(zip, outputfiles$zip_force)
saveRDS(literal_values, outputfiles$force_literal_values)