#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019, HRW, GPL v2 or later
# ============================================
# HRW-us-tulsa/census_data/Makefile
#
HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

.PHONY: all clean

census_data_output := \
	$(HERE)/census_data/output/city_census_wide.rds \
	$(HERE)/census_data/output/city_census_long.rds \
	$(HERE)/census_data/output/census_tracts.rds \
	$(HERE)/census_data/output/zips.rds \
	$(HERE)/census_data/output/census_literal_values.rds \
	$(HERE)/census_data/output/life_expectancy_map.html \
	$(HERE)/census_data/output/black_pop_perc_map.html \
	$(HERE)/census_data/output/unemployment_map.html \
	$(HERE)/census_data/output/poverty_map.html \
	$(HERE)/census_data/frozen/census_variables.csv

census_data_input := \
		$(HERE)/census_data/frozen/zip_ACS.csv \
		$(HERE)/census_data/frozen/zip_ACS_economic.csv \
		$(HERE)/census_data/frozen/north_tulsa_labels.csv \
		$(HERE)/census_data/frozen/tulsa_zips.csv \
		$(HERE)/census_data/frozen/life_expectancy.csv


all: $(census_data_output)


clean:
	rm output/*


$(census_data_output): \
		src/census_tulsa.R \
		$(census_data_input)
	Rscript $<

# done
