#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019, HRW, GPL v2 or later
# ============================================
# HRW-us-tulsa/other_descriptives/Makefile
#
HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

.PHONY: all clean

stops_output := \
	$(HERE)/other_descriptives/output/traffic_stops_values.rds \
	$(HERE)/other_descriptives/output/map_traffic_stops_rate.html \
	$(HERE)/other_descriptives/output/selected_tracts_stops.rds \
	$(HERE)/other_descriptives/output/quartiles.csv \
	$(HERE)/other_descriptives/output/stops_tract.rds \
	$(HERE)/other_descriptives/output/correlation_race_plot.pdf \
	$(HERE)/other_descriptives/output/correlation_poverty_plot.pdf \
	$(HERE)/other_descriptives/output/map_length.html \
	$(HERE)/other_descriptives/output/interactive_stops.csv \
	$(HERE)/other_descriptives/output/stops_maps_data.rds

citations_output := \
	$(HERE)/other_descriptives/output/citations_literal_values.rds \
	$(HERE)/other_descriptives/output/citations_table_proportion.rds \
	$(HERE)/other_descriptives/output/table_multiple_locations.rds \
	$(HERE)/other_descriptives/output/citation_rate_map_data.rds \
	$(HERE)/other_descriptives/output/map_rate_citations.html \
	$(HERE)/other_descriptives/output/DL_only_map_data.rds \
	$(HERE)/other_descriptives/output/map_DLonly_rate.html \
	$(HERE)/other_descriptives/output/interactive_citations.csv

force_output := \
	$(HERE)/other_descriptives/output/force_values.rds \
	$(HERE)/other_descriptives/output/table_force_rates.rds \
	$(HERE)/other_descriptives/output/correlation_plot_force_race.pdf \
	$(HERE)/other_descriptives/output/trends_force_plot.pdf \
	$(HERE)/other_descriptives/output/trends_for_force_plot.rds \
	$(HERE)/other_descriptives/output/zip_force.rds

jail_output := \
	$(HERE)/other_descriptives/output/jail_literal_values.rds


descriptives_input := \
		$(HERE)/matching/output/stops.rds \
		$(HERE)/matching/output/citations.rds \
		$(HERE)/matching/output/force_arrests_matches.rds \
		$(HERE)/matching/output/arrests.rds \
		$(HERE)/matching/output/use_force.rds \
		$(HERE)/matching/output/jail.rds \
		$(HERE)/matching/output/deadly_force.rds \
		$(HERE)/census_data/output/census_tracts.rds \
		$(HERE)/census_data/output/zips.rds \
		$(HERE)/census_data/output/city_census_long.rds \
		$(HERE)/census_data/output/city_census_wide.rds \
		$(HERE)/other_descriptives/output/arrests_methodology__andforce_table.csv 
		

all: $(stops_output) \
	$(jail_output) \
	$(citations_output) \
	$(force_output)


clean:
	rm output/*


$(stops_output): \
		src/tulsa_stops.R \
		$(descriptives_input)
	Rscript --vanilla $<


$(citations_output): \
		src/tulsa_citations.R \
		$(descriptives_input)
	Rscript --vanilla $<

$(force_output): \
		src/tulsa_use_force_descriptives.R \
		$(descriptives_input)
	Rscript --vanilla $<


$(jail_output): \
		src/tulsa_jail.R \
		$(descriptives_input)
	Rscript --vanilla $<


# done
