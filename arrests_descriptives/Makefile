#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019, HRW, GPL v2 or later
# ============================================
# HRW-us-tulsa/arrests_descriptives/Makefile
#
HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

.PHONY: all clean

arrests_descriptives_output := \
	$(HERE)/other_descriptives/output/arrests_values.rds \
	$(HERE)/other_descriptives/output/monthly_arrests_plot.pdf \
	$(HERE)/other_descriptives/output/arrests_methodology__andforce_table.csv \
	$(HERE)/other_descriptives/output/race_rates_citywide.csv \
	$(HERE)/other_descriptives/output/map_total_arrests_map.html \
	$(HERE)/other_descriptives/output/tract_arrest_rate_map.html \
	$(HERE)/other_descriptives/output/warrant_only_map_data.rds \
	$(HERE)/other_descriptives/output/arrest_offenses.csv \
	$(HERE)/other_descriptives/output/warrant_only_arrest_map.html \
	$(HERE)/other_descriptives/output/correlation_arrest_pop.pdf \
	$(HERE)/other_descriptives/output/correlation_arrest_poverty_plot.pdf \
	$(HERE)/other_descriptives/output/correlation_arrest_nonwhite_plot.pdf \
	$(HERE)/other_descriptives/output/correlation_arrest_medincome_plot.pdf \
	$(HERE)/other_descriptives/output/interactive_arrests.csv \
	$(HERE)/other_descriptives/output/correlation_race_specific_arrest_income_plot.pdf \
	$(HERE)/other_descriptives/output/data_race_specific_correlation_plot.rds \
	$(HERE)/other_descriptives/output/tract_race.rds

arrests_descriptives_input := \
		$(HERE)/matching/output/arrests.rds \
		$(HERE)/census_data/output/census_tracts.rds \
		$(HERE)/census_data/output/city_census_long.rds


all: $(arrests_descriptives_output)


clean:
	rm output/*


$(arrests_descriptives_output): \
		src/tulsa_arrests.R \
		$(arrests_descriptives_input)
	Rscript --vanilla $<

# done
