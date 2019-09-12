#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019
# =========================================
# HRW-us-tulsa/arrests_descriptives/src/tulsa_arrests.R

library(pacman)
p_load(tidyverse, lubridate, readr, scales, stringr, here, assertr,
       sf, leaflet, htmlwidgets, ggplot2, zoo, Hmisc, extrafont, geosphere)
options(scipen=999)

here <- here::here
font_import() # import all your fonts
fonts() #get a list of fonts
fonttable()
fonttable()[90:94,]
loadfonts()

source(here("other_descriptives/src/functions_shared.R"))

#load data
arrests <- read_rds(here("matching/output/arrests.rds"))
census_tract <- read_rds(here("census_data/output/census_tracts.rds"))
tulsa_city_census_long <- read_rds(here("census_data/output/city_census_long.rds"))

#start a list of values for export
literal_values <- list()

#list of output files
outputfiles <- list(
   arrests_literal_values = "other_descriptives/output/arrests_values.rds",
   monthly_arrests_plot = "other_descriptives/output/monthly_arrests_plot.pdf",
   arrests_methodology_and_force_table = "other_descriptives/output/arrests_methodology__andforce_table.csv",
   race_rates_citywide = "other_descriptives/output/race_rates_citywide.csv",
   total_arrests_tract_map = "other_descriptives/output/map_total_arrests_map.html",
   arrest_rate_tract_map = "other_descriptives/output/tract_arrest_rate_map.html",
   arrest_offenses = "other_descriptives/output/arrest_offenses.csv",
   warrant_only_map_data = "other_descriptives/output/warrant_only_map_data.rds",
   warrant_only_arrest_map = "other_descriptives/output/warrant_only_arrest_map.html",
   correlation_arrests_population_plot = "other_descriptives/output/correlation_arrest_pop.pdf",
   correlation_arrest_rate_poverty_plot = "other_descriptives/output/correlation_arrest_poverty_plot.pdf",
   correlation_arrest_rate_nonwhite_plot = "other_descriptives/output/correlation_arrest_nonwhite_plot.pdf",
   correlation_arrest_rate_medincome_plot = "other_descriptives/output/correlation_arrest_medincome_plot.pdf",
   interactive_arrests = "other_descriptives/output/interactive_arrests.csv",
   correlation_race_specific_arrest_income_plot = 
      "other_descriptives/output/correlation_race_specific_arrest_income_plot.pdf",
   data_race_specific_correlation_plot =
      "other_descriptives/output/data_race_specific_correlation_plot.rds",
   tract_race = "other_descriptives/output/tract_race.rds"
)
outputfiles <- map(outputfiles, here)

#dates
literal_values$min_arrest_date <- min(arrests$ARREST_DATE)
literal_values$max_arrest_date <- max(arrests$ARREST_DATE)
literal_values$total_arrests <- n_distinct(arrests$arrest_id)

#yearly arrests
year_arrests <- arrests %>%
   group_by(year(ARREST_DATE)) %>%
   summarise(count = n_distinct(arrest_id))
literal_values$mean_annual_arrests <- round(mean(year_arrests$count), 0)
rm(year_arrests)

#monthly arrests 
monthly_arrests <- arrests %>%
   group_by(month) %>%
   summarise(count = n_distinct(arrest_id))

literal_values$mean_monthly_arrests <- round(mean(monthly_arrests$count), 0)
literal_values$median_monthly_arrests <- median(monthly_arrests$count)

plot <- ggplot(monthly_arrests, aes(x = month, y = count)) +
   geom_bar(stat = "identity") +
   theme_agile() +
   expand_limits(y = 0) +
   scale_y_continuous(name = "Monthly arrests", labels = comma, limits = c()) +
   theme(legend.title=element_blank()) +
   theme(strip.text.y = element_text(size = 8)) +
   #theme(axis.text.x = element_text(angle=45, vjust = .5, hjust = .5)) +
   theme(legend.position = "bottom") +
   labs(x = "",
        title = "Tulsa Police Department Arrests", 
        subtitle = "Monthly Arrests (2012 - 2017)",
        caption = "Source: Human Rights Watch analysis of Tulsa Police Department data.")
ggsave(plot, file = outputfiles$monthly_arrests_plot, 
       units="in", width = 8, height = 6, dpi = 600)
embed_fonts(outputfiles$monthly_arrests_plot)
rm(monthly_arrests)

#So far, providing total arrests. For remainder of analysis, data
#will only include arrests geolocated within a tract limit,
#so we have appropriate
#arrests/population data so that appropriate rates can be calculated.
arrests_complete <- arrests

arrests <- arrests[arrests$GEOID %in% census_tract$GEOID, ]

#citywide arrest rate.
total_arrests2 <- n_distinct(arrests$arrest_id)

#all analysis will use method 2 in the census data 
tulsa_census2 <- tulsa_city_census_long %>%
   filter(method == "Method 2 - multi-race recoded as single race")

city_arrests <- tulsa_census2 %>%  
   summarise(total_pop = sum(number)) %>%
   mutate(annual_arrest_rate_per1K = total_arrests2/(6*total_pop)
           * 1000)
rm(total_arrests2, city_arrests)

#Total arrest rates by race
race_rates <- arrests %>%
   group_by(race) %>%
   summarise(num_arrests = n_distinct(arrest_id)) %>%
   mutate(percent_arrests = num_arrests/sum(num_arrests)) %>%
   filter(!is.na(race))

race_rates <- left_join(race_rates, tulsa_census2, by = c( "race" = "race2"))

race_rates <- race_rates %>%
   mutate(arrest_rate = round((num_arrests/(6*number))
          * 1000, 2)) %>%
   select(-Race, - method) %>%
   mutate(offense_type = "Total arrests")

#Race numbers and proportions of arrests for the methodology table and for use of force
write_csv(race_rates, outputfiles$arrests_methodology_and_force_table)

#citywide race rates by offense type
offense_type_race_rates <- arrests %>%
   filter(most_serious_offense == 1) %>%
   group_by(race, offense_type) %>%
   summarise(num_arrests = n_distinct(arrest_id)) 

offense_type_race_rates <- left_join(offense_type_race_rates, tulsa_census2,
                                     by = c( "race" = "race2"))

offense_type_race_rates <- offense_type_race_rates %>%
   mutate(arrest_rate = round((num_arrests/(6*number))
                              * 1000, 2)) %>%
   select(race, offense_type, num_arrests, arrest_rate)

race_rates <- bind_rows(race_rates, offense_type_race_rates) %>%
   select(race, offense_type, num_arrests, arrest_rate, percent_arrests, Tulsa_City_Percent)
write_csv(race_rates, outputfiles$race_rates_citywide)

offense_type_race_rates <- offense_type_race_rates %>%
   filter(race == "Black" | race == "White") %>%
   group_by(offense_type) %>%
   mutate(ratio = arrest_rate/arrest_rate[race == "White"])
rm(offense_type_race_rates)

#identify literal values for markdown
literal_values$white_citywide_arrest_rate <- as.numeric(race_rates[5,4])
literal_values$black_citywide_arrest_rate <- as.numeric(race_rates[2,4])
literal_values$black_arrest_percent <- as.numeric(race_rates[2,5])
literal_values$white_arrest_percent <- as.numeric(race_rates[5,5])

# Ratios of black to white rates
black_white_ratios <- race_rates %>%
   select(race, arrest_rate, offense_type) %>%
   filter(race == "White" | race == "Black") %>%
   spread(race, arrest_rate) %>%
   rename(black_rate = Black, white_rate = White) %>%
   mutate(black_to_white_ratio = round(black_rate/white_rate, 1))

#more literal values
literal_values$black_to_white_ratio_citywide <- 
   as.numeric(black_white_ratios[7,4])
literal_values$black_to_white_drug_possession <-
   as.numeric(black_white_ratios[1,4])
literal_values$black_to_white_drug_sale <-
   as.numeric(black_white_ratios[2,4])
literal_values$pub_order_black_white_ratio <-   
   as.numeric(black_white_ratios[5,4])
literal_values$theft_black_white_ratio <- 
   as.numeric(black_white_ratios[6,4])
literal_values$violent_black_white_ratio <- 
   as.numeric(black_white_ratios[8,4])
literal_values$warrant_black_white_ratio <- 
   as.numeric(black_white_ratios[9,4])
literal_values$weapons_black_white_ratio <- 
   as.numeric(black_white_ratios[10,4])

#arrest rates for all drug offenses
drug <- arrests %>%
   filter(most_serious_offense == 1) %>%
   filter(offense_type == "Drug sale" |
             offense_type == "Drug possession/use") %>%
   filter(race == "White" | race == "Black") %>%
   group_by( race) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = round(count/sum(count), 3)) 

drug <- left_join(drug, tulsa_census2, by = c( "race" = "race2"))   

drug <- drug %>%
   mutate(rate = count/(number*6) * 1000) 

literal_values$black_to_white_drugs_total <-
   as.numeric(round(drug[1,8]/drug[2,8], 1))
   
#Black and white arrest rates for specific drug offenses
drug <- arrests %>%
   filter(most_serious_offense == 1) %>%
   filter(offense_type == "Drug sale" |
             offense_type == "Drug possession/use") %>%
   filter(race == "White" | race == "Black") %>%
   group_by(drug, offense_recode, race) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = round(count/sum(count), 3)) 

drug <- left_join(drug, tulsa_census2, by = c( "race" = "race2"))   

drug <- drug %>%
   mutate(rate = count/(number*6) * 1000) 

#literal
literal_values$black_to_white_mj_sale <-
   as.numeric(round(drug[10,10]/drug[11,10], 1))
literal_values$black_to_white_mj_possession <-
   as.numeric(round(drug[2,10]/drug[3,10], 1))


#most serious offense, offense type total percentage of arrests, no race
most_serious <- arrests %>%
   filter(most_serious_offense == 1) %>%
   group_by(offense_recode) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = round(count/sum(count), 3)) %>%
   arrange(desc(count)) %>%
   mutate(cumperc = cumsum(perc))

#top crimes
top <- most_serious %>%
   slice(1:50)

#race disparities by most serious charge
by_offense <- arrests %>%
   filter(offense_recode %in% top$offense_recode) %>%
   filter(most_serious_offense == 1) %>%
   filter(race == "Black" | race == "White") %>%
   group_by(offense_type, offense_recode, race) %>%
   summarise(count = n_distinct(arrest_id)) 

by_offense <- left_join(by_offense, tulsa_census2,
                        by = c( "race" = "race2")) 

by_offense <- by_offense %>%
   mutate(rate = count/(number*6) * 1000) %>%
   group_by(offense_recode) %>%
   mutate(ratio = rate/rate[race == "White"])

#store some literal values for markdown
#perc of arrests that are warrants
warrants <- most_serious %>%
   filter(grepl('Warrant', offense_recode))

literal_values$warrant_total_perc_of_arrests <- 
   100 * sum(warrants$perc)
literal_values$municipal_warrant_perc_of_arrests <- 
   100 * sum(warrants[2,3])
literal_values$county_warrant_perc_of_arrests <- 
   100 * sum(warrants[1,3])

#of warrant arrests, how many was warrant only charge?
warrants <- arrests %>%
   filter(warrant_arrest == 1) %>%
   group_by(only_warrant_offense) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = count/sum(count))

rm(warrants)

#proportion of arrests that are police initiated (HRW coding)
police_intiated <- arrests %>%
   filter(most_serious_offense == 1) %>%
   group_by(initiation) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = count/sum(count))

literal_values$proactive_arrest_percent <- 
   round(police_intiated[2,3], 3) * 100
rm(police_intiated)

police_intiated <- arrests %>%
   filter(offense_recode %in% top$offense_recode) %>%
   filter(most_serious_offense == 1) %>%
   filter(race == "Black" | race == "White") %>%
   group_by(initiation, offense_recode, race) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = count/sum(count))

police_intiated <- left_join(police_intiated, tulsa_census2, by = c( "race" = "race2"))   

police_intiated <- police_intiated %>%
   mutate(rate = count/(number*6) * 1000) %>%
   mutate(ratio = rate/rate[race == "White"]) %>%
   filter(race == "Black") %>%
   arrange(initiation, desc(ratio))

join <- police_intiated %>%
   select(offense_recode, black_to_white_ratio = ratio, initiation)

top <- left_join(top, join)
write_csv(top, outputfiles$arrest_offenses)

#examples of re-active offenses and disparities
reactive <- arrests  %>%
   filter(initiation == "Call/victim initiated" 
          & most_serious_offense == 1) %>%
   group_by(offense_recode, race) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = round(count/sum(count), 3)) 
               
reactive <- left_join(reactive, tulsa_census2, by = c( "race" = "race2"))   
               
reactive <- reactive %>%
    mutate(rate = count/(number*6) * 1000) %>%
   filter(count > 200) %>%
   mutate(ratio = rate/rate[race == "White"])
rm(reactive)

#Garnett
garnett <- arrests %>%
   filter(garnett == 1)

literal_values$garnett_total_arrests <- n_distinct(garnett$arrest_id)

#types of charges
garnett_charges <- garnett %>%
   group_by(offense_type) %>%
   summarise(offenses = n_distinct(arrest_id)) %>%
   mutate(perc = round(offenses/sum(offenses), 3)) %>%
   arrange(desc(perc))

garnett_charges <- garnett %>%
   group_by(offense_recode) %>%
   summarise(offenses = n_distinct(arrest_id)) %>%
   mutate(perc = round(offenses/sum(offenses), 3)) %>%
   arrange(desc(perc))


#within .25 miles
near_garnett <- arrests %>%
   filter(near_garnett == 1)

near_garnett_offenses <- near_garnett %>%
   filter(most_serious_offense == 1) %>%
   group_by(offense_type) %>%
   summarise(offenses = n_distinct(arrest_id)) %>%
   mutate(perc = round(offenses/sum(offenses), 3)) %>%
   arrange(desc(perc))

#how many people arrested there were charged with a warrant?
warrant <- garnett %>%
   group_by(warrant_arrest) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = count/sum(count))

literal_values$garnett_perc_warrants <- as.numeric(round
                                                   (warrant[2,3], 2)* 100)
   
#were people charged only with warrants?
warrant <- garnett %>%
   filter(warrant_arrest == 1) %>%
   group_by(only_warrant_offense) %>%
   summarise(count = n_distinct(arrest_id)) %>%
   mutate(perc = count/sum(count))

literal_values$garnett_perc_only_warrants <- as.numeric(round
                                                   (warrant[2,3], 2)* 100)
rm(warrant, garnett_charges, garnett, near_garnett_offenses, near_garnett)

#census tract level analysis
#race per census tract

#tract - remove downtown jail arrests
arrests_tract <- arrests %>%
   filter(jail == 0) %>%
   filter(!is.na(GEOID)) %>%
   group_by(GEOID, race) %>%
   summarise( count = n_distinct(arrest_id)) %>%
   spread(race, count) %>%
   ungroup() %>%
   mutate(total_arrests = rowSums(select(., Asian:White), na.rm = T))

#only want method 2
census_join <- census_tract %>%
   filter(method == "Method 2 - multi-race recoded as single race")

tract_race <- left_join(census_join, arrests_tract)

#rate of arrest (per pop) per tract
tract_race <- tract_race %>%
   mutate(black_pop = ifelse(black_pop == 0, 1, black_pop), 
          total_arrest_rate = (total_arrests/(6*tot_pop)) * 1000,
          black_arrest_rate = (Black/(6*black_pop)) * 1000,
          white_arrest_rate = (White/(6*white_pop)) * 1000,
          non_white_arrest_rate = ((total_arrests - White)/
                               (6* tot_pop - 6 *white_pop)) * 1000)

tract_race <- tract_race %>%
   mutate(black_to_white = round(black_arrest_rate/white_arrest_rate, 2))

#filter out downtown and the walmart tracts (unique outlier tracts)
tract_race2 <- tract_race %>%
   filter(GEOID != "40143002500" & 
             GEOID != "40143008300") 

#overall correlations
literal_values$cor_arrest_rate_non_white_pop <- 
   as.numeric(round(cor.test(tract_race2$total_arrest_rate, 
                             tract_race2$non_white)$estimate, 3))
literal_values$cor_arrest_rate_poverty <- 
   as.numeric(round(cor.test(tract_race2$total_arrest_rate, 
                             tract_race2$poverty_rate)$estimate, 3))
literal_values$cor_arrest_rate_income <- 
   as.numeric(round(cor.test(tract_race2$total_arrest_rate, 
                             tract_race2$median_hh_income)$estimate, 3))
literal_values$cor_arrest_rate_unemployment <- 
   as.numeric(round(cor.test(tract_race2$total_arrest_rate, 
                             tract_race2$unemployment_rate)$estimate, 3))

#plot the correlation
#percent non-white
plot <- ggplot(tract_race2, 
               aes(x = non_white, y = total_arrest_rate)) +
   geom_point() + 
   geom_smooth(method = 'lm') +
   theme_agile() +
   theme(legend.position = "bottom") +
   scale_x_continuous(labels = percent) +
   labs(x = "Percentage of population that is non-white",
        y = "Arrests per 1,000 people",
        title = "Census Tracts by Arrest Rate and Percentage of Tract that is Non-white",
        subtitle = paste("Moderate correlation (r = ", 
                         literal_values$cor_arrest_rate_non_white_pop,
                         ")", sep = ""),
        caption = "Source: Human Rights Watch analysis of Tulsa Police Department data.")
ggsave(plot, file = outputfiles$correlation_arrest_rate_nonwhite_plot, 
       units="in", width = 8, height = 6, dpi = 600)
embed_fonts(outputfiles$correlation_arrest_rate_nonwhite_plot)

#percent below poverty
plot <- ggplot(tract_race2, 
               aes(x = poverty_rate, y = total_arrest_rate)) +
   geom_point() + 
   geom_smooth(method = 'lm') +
   theme_agile() +
   theme(legend.position = "bottom") +
   scale_x_continuous(labels = percent) +
   labs(x = "Percentage of population living below poverty line",
        y = "Arrests per 1,000 people",
        title = "Census Tracts by Arrest Rate and Poverty Rate",
        subtitle = paste("Moderate correlation (r = ", 
                         literal_values$cor_arrest_rate_perc_poverty,
                         ")", sep = ""),
        caption = "Source: Human Rights Watch analysis of Tulsa Police Department data.")
ggsave(plot, file = outputfiles$correlation_arrest_rate_poverty_plot, 
       units="in", width = 8, height = 6, dpi = 600)
embed_fonts(outputfiles$correlation_arrest_rate_poverty_plot)

#median income
plot <- ggplot(tract_race2, 
               aes(x = median_hh_income, y = total_arrest_rate)) +
   geom_point() + 
   geom_smooth(method = 'lm') +
   theme_agile() +
   theme(legend.position = "bottom") +
   scale_x_continuous(labels = dollar) +
   labs(x = "Median Income",
        y = "Arrests per 1,000 people",
        title = "Census Tracts by Arrest Rate and Median Income",
        subtitle = paste("Moderate correlation (r = ", 
                         literal_values$cor_arrest_rate_income,
                         ")", sep = ""),
        caption = "Source: Human Rights Watch analysis of Tulsa Police Department data.")
ggsave(plot, file = outputfiles$correlation_arrest_rate_medincome_plot, 
       units="in", width = 8, height = 6, dpi = 600)
embed_fonts(outputfiles$correlation_arrest_rate_medincome_plot)

#filter only tracts where at least 1% of the pop is black or white people
tract_race3 <- tract_race2 %>%
   filter(perc_black > .01 & perc_white > .01)

b_model <- lm(tract_race3$black_arrest_rate ~ tract_race3$median_black_income)
w_model <- lm(tract_race3$white_arrest_rate ~ tract_race3$median_white_income)
summary(b_model) 
summary(w_model)

literal_values$black_model_intercept <- summary(b_model)$coefficients[1,1]
literal_values$white_model_intercept <- summary(w_model)$coefficients[1,1]
literal_values$white_model_rsquared <- summary(w_model)$r.squared
literal_values$black_model_rsquared <- summary(b_model)$r.squared
literal_values$black_model_coeff <- summary(b_model)$coefficients[2,1] * 1000
literal_values$white_model_coeff <- summary(w_model)$coefficients[2,1] * 1000

names(summary(w_model))

group1 <- tract_race3 %>%
   st_set_geometry(NULL) %>%
   ungroup() %>%
   select(GEOID, black_arrest_rate, white_arrest_rate) %>%
   gather(cat, arrest_rate, 2:3) %>%
   mutate(cat = ifelse(cat == "black_arrest_rate",
                       "Black", "White"))

group2 <- tract_race3 %>%
   st_set_geometry(NULL) %>%
   ungroup() %>%
   select(GEOID, median_black_income, median_white_income) %>%
   gather(cat, median_income, 2:3) %>%
   mutate(cat = ifelse(cat == "median_black_income",
                       "Black", "White")) 

data_race_specific_correlation_plot <- left_join(group1, group2)
rm(group2, group1)

corrs <- data_race_specific_correlation_plot %>% 
   group_by(cat) %>% 
   summarise(cor = cor(arrest_rate, median_income, use="complete.obs"))

literal_values$cor_arrests_income_white <- as.numeric(corrs[2,2])
literal_values$cor_arrests_income_black <- as.numeric(corrs[1,2])

#send plot data out for markdown
write_rds(data_race_specific_correlation_plot, outputfiles$data_race_specific_correlation_plot)

plot <- ggplot(data_race_specific_correlation_plot, 
               aes(x = median_income,
                   y = arrest_rate)) +
   geom_point() + 
   facet_grid( ~ cat) +
   geom_smooth(method = 'lm') +
   theme_agile() +
   theme(legend.position = "bottom") +
   scale_x_continuous(labels = dollar) +
   scale_y_continuous(labels = comma) +
   expand_limits(x = 0) +
   theme(panel.spacing = unit(2, "lines")) +
   theme(plot.margin = unit(c(.5,1,0,.5), "cm")) +
   labs(x = "Median income",
        y = "Arrest rate",
        title = "Census Tracts by Race-specific Arrest Rate and Median Income",
        subtitle = paste("Black - no correlation (r = ", 
                         round(literal_values$cor_arrests_income_black, 3),"),", 
                         " White - moderate correlation (r = ",
                         round(literal_values$cor_arrests_income_white, 3), ")", sep = ""),
        caption = "Source: Human Rights Watch analysis of Tulsa Police Department data.")
ggsave(plot, file = outputfiles$correlation_race_specific_arrest_income_plot, 
       units="in", width = 8, height = 6, dpi = 600)
embed_fonts(outputfiles$correlation_race_specific_arrest_income_plot)

#map sheer numbers of arrests with arrests at downtown jail removed
tract_race <- tract_race %>%
   filter(!is.na(total_arrests))

bins <-c(0, 100, 500, 1000, 1500, 2000, 2500, 5300)
pal <- colorBin(palette = "viridis", pretty = T, bins = bins,
                    domain = tract_race$total_arrests)

map_total <- tract_race %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ total_arrests,
             title = "Number of arrests in census tract.\n2012 - 2017",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(comma_format()(total_arrests),
                               " total arrests", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(total_arrests)) 
map_total
saveWidgetFix(map_total, outputfiles$total_arrests_tract_map, selfcontained=T)

#total rate of arrest map
pal <- colorNumeric(palette = "viridis", 
                    domain = tract_race$total_arrest_rate)

bins <-c(0, 10, 25,50,75, 100, 200, 300)
 pal <- colorBin(palette = "viridis", pretty = T, bins = bins,
                 domain = tract_race$total_arrest_rate)

map_total <- tract_race %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ total_arrest_rate,
             title = "Arrest rate per 1,000 people\n2012 - 2017",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(comma_format()(total_arrest_rate),
                               " arrests per 1,000 people", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(total_arrest_rate)) 
map_total
saveWidgetFix(map_total, outputfiles$arrest_rate_tract_map, selfcontained=T)

#write tract race for markdown maps
write_rds(tract_race, outputfiles$tract_race)

#warrant only map
warrant_only <- arrests %>%
   filter(only_warrant_offense == 1) %>%
   group_by(GEOID) %>%
   summarise(count = n_distinct(arrest_id))
sum(warrant_only$count)

warrant_only <- left_join(warrant_only, tract_race2)

warrant_only <- warrant_only %>%
   mutate(warrant_only_rate = count/(tot_pop*6) * 1000) 

cor.test(warrant_only$warrant_only_rate, 
         warrant_only$median_hh_income)

warrant_only <- st_sf(warrant_only)

#write out for markdown
write_rds(warrant_only, outputfiles$warrant_only_map_data)

pal <- colorNumeric(palette = "viridis", 
                    domain = warrant_only$warrant_only_rate)

map_total <- warrant_only %>%
   st_transform(crs = "+init=epsg:4326") %>%
   leaflet(width = "100%", height = 800) %>%
   addProviderTiles(provider = "CartoDB.Positron") %>%
   addLegend("bottomright", 
             pal = pal, 
             values = ~ warrant_only_rate,
             title = "Warrant-Only Arrest Rate, \n2012 - 2017",
             opacity = 1) %>%
   addPolygons(popup = ~ paste(round(warrant_only_rate, 1),
                               " warrant-only arrests per 1,000 residents", sep = ""),
               stroke = FALSE,
               smoothFactor = 0,
               fillOpacity = 0.7,
               color = ~ pal(warrant_only_rate)) 
map_total
saveWidgetFix(map_total, 
              file = outputfiles$warrant_only_arrest_map, selfcontained=T)


#output table with tracts to join for interactive map.
#tracts : arrest rate, white arrest rate, black arrest rate.
#by offense type, total, white, Black. With outlier tracts.
warrant_only <- arrests %>%
   filter(only_warrant_offense == 1) %>%
   group_by(GEOID) %>%
   summarise(count = n_distinct(arrest_id))
sum(warrant_only$count)

warrant_only <- left_join(warrant_only, tract_race)

warrant_only <- warrant_only %>%
   mutate(warrant_only_rate = count/(tot_pop*6) * 1000) 



interactive_arrests <- as.data.frame(warrant_only) %>%
   select(GEOID, total_arrests, total_arrest_rate,
          black_to_white_arrest_rate_ratio = black_to_white,
          warrant_only_arrest_rate = warrant_only_rate)

write_csv(interactive_arrests, outputfiles$interactive_arrests)

#Save literal values
saveRDS(literal_values, outputfiles$arrests_literal_values)
