#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019
# =========================================
# HRW-us-tulsa/other_descriptives/src/tulsa_stops.R

library(pacman)
p_load(tidyverse, lubridate, readr, stringr, here, assertr,
       sf, leaflet, htmlwidgets, ggplot2, zoo, Hmisc, extrafont, scales)
options(scipen=999)

here <- here::here
font_import() # import all your fonts
fonts() #get a list of fonts
fonttable()
fonttable()[90:94,]
loadfonts()

source(here("other_descriptives/src/functions_shared.R"))

#load data
stops <- read_rds(here("matching/output/stops.rds"))
census_tract <- read_rds(here("census_data/output/census_tracts.rds"))
tulsa_census2 <- read_rds(here("census_data/output/city_census_long.rds"))

#start a list of values for export
stops_literal_values <- list()

#list of output files
outputfiles <- list(
   stops_literal_values = "other_descriptives/output/traffic_stops_values.rds",
   map_traffic_stop_rates = "other_descriptives/output/map_traffic_stops_rate.html",
   selected_tracts = "other_descriptives/output/selected_tracts_stops.rds",
   quartiles = "other_descriptives/output/quartiles.rds",
   stops_tract = "other_descriptives/output/stops_tract.rds",
   correlation_race_plot = "other_descriptives/output/correlation_race_plot.pdf",
   correlation_poverty_plot = "other_descriptives/output/correlation_poverty_plot.pdf",
   stop_length_tract_map = "other_descriptives/output/map_length.html",
   interactive = "other_descriptives/output/interactive_stops.csv",
   stops_data_for_maps = "other_descriptives/output/stops_maps_data.rds"
 )
outputfiles <- map(outputfiles, here)

#dates of the stops database
stops_literal_values$stops_minyear <- year(min(stops$Response_Date))
stops_literal_values$stops_maxyear <- year(max(stops$Response_Date))

#of traffic and ped stops
tot <- stops %>%
   group_by(reason) %>%
   summarise(count = n_distinct(stop_id))

stops_literal_values$total_ped_stops <- as.numeric(tot[1,2])
stops_literal_values$total_traffic_stops <- as.numeric(tot[2,2])
rm(tot)

#percentage of stops that were geocoded
geocode_perc <- stops %>%
   group_by(geocoded) %>%
   summarise(count = n_distinct(stop_id)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(geocoded == 1)
stops_literal_values$percent_stops_geocoded <- 100 * round((geocode_perc$perc), 2)
rm(geocode_perc)

#dataframe of total/rate of traffic stops by tract.
traffic <- stops %>%
   filter(reason == "Traffic Stop")

tract <- traffic %>%
   group_by(GEOID) %>%
   summarise(count = n_distinct(stop_id))

#join and filter only city tracts
census_tract <- census_tract %>%
   filter(method == "Method 2 - multi-race recoded as single race") 

tract <- left_join(census_tract, tract) %>%
   filter(keep == 1)

tract <- tract %>%
   mutate(per_capita_rate_per_1K = round(count/(4 * tot_pop) * 1000, 0)) %>%
   arrange(desc(per_capita_rate_per_1K)) %>%
   select(GEOID, NAME, per_capita_rate_per_1K, perc_black, perc_white,
          tot_pop, median_hh_income, count, everything())

#Leaflet map of traffic stop rates per tract population
pal <- colorNumeric(palette = "viridis", domain = tract$per_capita_rate_per_1K)

map_stops <- tract %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ per_capita_rate_per_1K,
             title = "Rate of traffic stops per 1,000 people living in census tract.\n2014 - 2017",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(comma_format()(per_capita_rate_per_1K), "stops per 1K residents", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(per_capita_rate_per_1K)) 
map_stops
saveWidgetFix(map_stops, file = outputfiles$map_traffic_stop_rates,
              selfcontained=T)

#example tracts at different ends of rate distribution
stops_literal_values$tract62_rate <- as.numeric(
   tract$per_capita_rate_per_1K[tract$GEOID == 40143006200])
stops_literal_values$tract62_perc_black <- as.numeric(round(
   tract$perc_black[tract$GEOID == 40143006200], 2) * 100)
stops_literal_values$tract62_pop <- as.numeric(
   tract$tot_pop[tract$GEOID == 40143006200])
stops_literal_values$tract62_income <- as.numeric(
   tract$median_hh_income[tract$GEOID == 40143006200])

stops_literal_values$tract7637_rate <- as.numeric(
   tract$per_capita_rate_per_1K[tract$GEOID == 40143007637])
stops_literal_values$tract7637_perc_white <- as.numeric(round(
   tract$perc_white[tract$GEOID == 40143007637], 2) * 100)
stops_literal_values$tract7637_pop <- as.numeric(
   tract$tot_pop[tract$GEOID == 40143007637])
stops_literal_values$tract7637_income <- as.numeric(
   tract$median_hh_income[tract$GEOID == 40143007637])

stops_literal_values$tract7638_rate <- as.numeric(
   tract$per_capita_rate_per_1K[tract$GEOID == 40143007638])
stops_literal_values$tract7638_perc_white <- as.numeric(round(
   tract$perc_white[tract$GEOID == 40143007638], 2) * 100)
stops_literal_values$tract7638_pop <- as.numeric(
   tract$tot_pop[tract$GEOID == 40143007638])
stops_literal_values$tract7638_income <- as.numeric(
   tract$median_hh_income[tract$GEOID == 40143007638])

#A whiter, poorer tract
stops_literal_values$tract30_rate <- as.numeric(
   tract$per_capita_rate_per_1K[tract$GEOID == 40143003000])
stops_literal_values$tract30_perc_white <- as.numeric(round(
   tract$perc_white[tract$GEOID == 40143003000], 2) * 100)
stops_literal_values$tract30_pop <- as.numeric(
   tract$tot_pop[tract$GEOID == 40143003000])
stops_literal_values$tract30_income <- as.numeric(
   tract$median_hh_income[tract$GEOID == 40143003000])


#top and bottom tracts, perc non-white, $median income
top <- tract %>%
   arrange(desc(per_capita_rate_per_1K)) %>%
   slice(1:10) 
bottom <- tract %>%
   arrange((per_capita_rate_per_1K)) %>%
   slice(1:5)

selected_tracts <- as.data.frame(bind_rows(top, bottom))  %>%
   ungroup() %>%
   mutate(poverty_rate = paste((round(poverty_rate, 2) *100), "%", sep = ""),
          perc_black = paste((round(perc_black, 2) * 100), "%", sep = ""),
          perc_white = paste((round(perc_white, 2) * 100), "%", sep = "")) %>%
   select(Tract = NAME,
          `Annual rate per 1K residents` = per_capita_rate_per_1K,
          `Percent Black` = perc_black, 
          `Percent White` = perc_white, 
          `Median household income` = median_hh_income, 
          `Poverty rate` = poverty_rate) %>%
   mutate(Tract = gsub("\\,.*","", Tract))
rm(top, bottom)

write_rds(selected_tracts, outputfiles$selected_tracts)
rm(selected_tracts)

#group tracts in quartiles by stop rate and non-white residents
tract <- tract %>%
   mutate(non_white_quartile = ntile(non_white, 4),
          stop_rate_quartile = ntile(per_capita_rate_per_1K, 4))

pop_in_quartile <- as.data.frame(tract) %>%
   select(-geometry) %>%
   group_by(stop_rate_quartile) %>%
   summarise(total_pop = sum(tot_pop),
             total_white = sum(white_pop),
             total_black = sum(black_pop),
             avg_non_white = mean(non_white),
             avg_median_income = mean(median_hh_income),
             avg_below_pov = mean(poverty_rate)) %>%
   mutate(avg_median_income = round(avg_median_income, 0),
      avg_below_pov = paste(round(avg_below_pov, 2) * 100,
                                "%", sep = ""),
      percent_of_pop = paste(round(total_pop/sum(total_pop), 2)
                                 * 100, "%", sep = ""),
          percent_of_black_pop = paste(round(total_black/sum(total_black), 2)
                                       * 100, "%", sep = ""),    
          perc_black = paste(round(total_black/total_pop, 2)
                             * 100, "%", sep = ""),
      per_non_white = paste(round((total_pop - total_white)/total_pop, 2)
                            * 100, "%", sep = ""),
          perc_non_white = paste(round(1 - (total_white/total_pop), 2)
                                 * 100, "%", sep = "")) %>%
   mutate(`Census Tract Traffic Stop Rate Quartiles` = case_when(
      stop_rate_quartile == 1 ~ "Lowest rate of traffic stops",
      stop_rate_quartile == 2 ~ "Second quartile",
      stop_rate_quartile == 3 ~ "Third quartile",
      stop_rate_quartile == 4 ~ "Highest rate of traffic stops"
   )) %>%
   select(`Census Tract Traffic Stop Rate Quartiles`,
          `Total Population` = total_pop,
          `Percent of Total Tulsa Population` = percent_of_pop,
          `Percent of Tulsa's Black Population` = percent_of_black_pop,
          `Percent of Quartile Population that is Black` = perc_black,
          `Percent of Quartile Population that is non-White` = perc_non_white,
          `Average Tract  Median Income` = avg_median_income,
          `Average Tract Percentage Below Poverty Rate` = avg_below_pov)
 
write_rds(pop_in_quartile, outputfiles$quartiles)


#correlations
stops_literal_values$cor_stoprate_nonwhite <- round(cor.test(tract$per_capita_rate_per_1K,
                                                       tract$non_white)$estimate, 3)
stops_literal_values$cor_stoprate_perc_below_pov <- round(cor.test(tract$per_capita_rate_per_1K,
                                                              tract$poverty_rate)$estimate, 3)
stops_literal_values$cor_stoprate_med_income <- round(cor.test(tract$per_capita_rate_per_1K,
                                                         tract$median_hh_income)$estimate, 3)
stops_literal_values$cor_stoprate_med_black_income <- round(cor.test(tract$per_capita_rate_per_1K,
                                                                     tract$median_black_income)$estimate, 3)
stops_literal_values$cor_stoprate_med_white_income <- round(cor.test(tract$per_capita_rate_per_1K,
                                                                     tract$median_white_income)$estimate, 3)

#write data out for markdown
write_rds(tract, outputfiles$stops_tract)

#scatter plot of census tracts - traffic stop rate v. proportion of pop that
#is non-white
plot <- ggplot(tract, aes(x = non_white, y = per_capita_rate_per_1K)) +
   geom_point() + 
   geom_smooth(method = 'lm') +
   theme_agile() +
   theme(legend.position = "bottom") +
   scale_x_continuous(labels = percent) +
   labs(x = "Percentage of population that is non-white",
        y = "Traffic stops per 1,000 people",
        title = "Census Tracts by Traffic Stop Rate and Non-White Population",
        subtitle = paste("Moderate correlation (r = ", round(stops_literal_values$cor_stoprate_nonwhite, 3), ")"),
        caption = "Source: Human Rights Watch analysis of Tulsa Police Department data.")
ggsave(plot, file = outputfiles$correlation_race_plot, 
       units="in", width = 8, height = 6, dpi = 600)
embed_fonts(outputfiles$correlation_race_plot)

#scatter plot 
plot <- ggplot(tract, aes(x = poverty_rate, y = per_capita_rate_per_1K)) +
   geom_point() + 
   geom_smooth(method = 'lm') +
   theme_agile() +
   theme(legend.position = "bottom") +
   scale_x_continuous(labels = percent) +
   labs(x = "Percentage of population living below poverty line",
        y = "Traffic stops per 1,000 people",
        title = "Census Tracts by Traffic Stop Rate and Poverty Rate",
        subtitle = paste("Moderate correlation (r = ", round(stops_literal_values$cor_stoprate_perc_below_pov, 3), ")"),
        caption = "Source: Human Rights Watch analysis of Tulsa Police Department data.")
ggsave(plot, file = outputfiles$correlation_poverty_plot, 
       units="in", width = 8, height = 6, dpi = 600)
embed_fonts(outputfiles$correlation_poverty_plot)

#length of time of traffic stops
length <- traffic %>%
   filter(hours < 24 & mins > 0)

stops_literal_values$median_traffic_stop_length <- round(median(length$mins), 1)
stops_literal_values$mean_traffic_stop_length <- round(mean(length$mins), 1)

#time by census tract
tract_length <- length %>%
   group_by(GEOID) %>%
   summarise(number = n_distinct(stop_id),
             median = as.numeric(median(mins)),
             avg = as.numeric(mean(mins, na.rm = T))) %>%
   filter(number > 100) %>%
   as_tibble() %>%
   ungroup()

tract_length <- left_join(tract, tract_length, by = c("GEOID"))


pal <- colorNumeric(palette = "viridis", 
                    domain = tract_length$avg)

map_rate <- tract_length %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ avg,
             title = "Mean length of traffic stop census tract.\n2014 - 2017",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(comma_format()(avg), " minutes", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(avg)) 
map_rate
saveWidgetFix(map_rate, outputfiles$stop_length_tract_map, selfcontained=T)

stops_literal_values$cor_lengthstop_nonwhite <- 
   round(cor.test(tract_length$avg, tract_length$non_white)$estimate, 3)
stops_literal_values$cor_lengthstop_poverty <-
   round(cor.test(tract_length$avg, tract_length$poverty_rate)$estimate, 3)

stops_literal_values$tract5_nonwhite <- as.numeric(
   round(tract$non_white[tract$GEOID == 40143000500], 2))
stops_literal_values$tract5_poverty <- as.numeric(
   round(tract$poverty_rate[tract$GEOID == 40143000500], 2))
stops_literal_values$tract5_length <- as.numeric(
   round(tract_length$avg[tract$GEOID == 40143000500], 0))

stops_literal_values$tract79_nonwhite <- as.numeric(
   round(tract$non_white[tract$GEOID == 40143007900], 2))
stops_literal_values$tract79_poverty <- as.numeric(
   round(tract$poverty_rate[tract$GEOID == 40143007900], 2))
stops_literal_values$tract79_length <- as.numeric(
   round(tract_length$avg[tract$GEOID == 40143007900], 0))

stops_literal_values$tract7619_nonwhite <- as.numeric(
   round(tract$non_white[tract$GEOID == 40143007619], 2))
stops_literal_values$tract7619_poverty <- as.numeric(
   round(tract$poverty_rate[tract$GEOID == 40143007619], 2))
stops_literal_values$tract7619_length <- as.numeric(
   round(tract_length$avg[tract$GEOID == 40143007619], 0))


#for interactive
for_interactive <- tract_length %>%
   select(GEOID, avg_mins_timelength_of_traffic_stops = avg, -geometry)

for_interactive <- as.data.frame(for_interactive) %>%
   select(-geometry)

for_int2 <- tract %>%
   select(GEOID, number_of_traffic_stops = count,
          per_capita_traffic_stop_rate_per_1K = per_capita_rate_per_1K)

for_int2 <- as.data.frame(for_int2) %>%
   select(-geometry)

for_interactive <- left_join(for_interactive, for_int2)
write_csv(for_interactive, outputfiles$interactive)

#write data for maps
stops_data_for_maps <- tract_length %>%
   select(GEOID, per_capita_rate_per_1K, avg)
saveRDS(stops_data_for_maps, outputfiles$stops_data_for_maps)

#Save literal values
saveRDS(stops_literal_values, outputfiles$stops_literal_values)
