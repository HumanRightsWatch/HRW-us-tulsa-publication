#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019, HRW, GPL v2 or later
# ============================================
# HRW-us-tulsa/processing/Makefile
#
HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

.PHONY: all clean

processing_output := \
	$(HERE)/processing/output/complaints.rds \
	$(HERE)/processing/output/use_force.rds \
	$(HERE)/processing/output/calls_stops.rds \
	$(HERE)/processing/output/citations.rds \
	$(HERE)/processing/output/arrests.rds \
	$(HERE)/processing/output/jail.rds \
	$(HERE)/processing/output/address_to_geocode.csv \
	$(HERE)/processing/output/deadly_force.rds

processing_input := \
		$(HERE)/import/output/arrests.rds \
		$(HERE)/import/output/citations.rds \
		$(HERE)/import/output/calls_stops.rds \
		$(HERE)/import/output/use_force.rds \
		$(HERE)/import/output/offenses.rds \
		$(HERE)/import/output/complaints.rds \
		$(HERE)/import/input/ucr_tabula.csv \
		$(HERE)/import/frozen/jail.rds \
		$(HERE)/import/frozen/deadly_force.csv \
		$(HERE)/processing/frozen/offenses_recoded.csv \
		$(HERE)/processing/frozen/citations_coded.csv \
		$(HERE)/processing/frozen/citations_coded2.csv \
		$(HERE)/processing/frozen/Officers.csv \
		$(HERE)/processing/frozen/addresses_to_join.csv \
		$(HERE)/processing/frozen/jail_offenses_recoded.csv \
		$(HERE)/processing/frozen/jail_crimes_recoded.csv \
		$(HERE)/processing/frozen/extra_ucc_recoded.csv


all: $(processing_output)


clean:
	rm output/*


$(processing_output): \
		src/tulsa_processing.R \
		$(processing_input)
	Rscript --vanilla $<

# done
