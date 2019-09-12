#
# Authors:     BR
# Maintainers: BR
# Copyright:   2019, HRW, GPL v2 or later
# ============================================
# HRW-us-tulsa/matching/Makefile
#
HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

.PHONY: all clean

matching_output := \
	$(HERE)/matching/output/force_arrests_matches.rds \
	$(HERE)/matching/extra/full_citation_stop_matches.rds \
	$(HERE)/matching/extra/stop_citation_match_ids.rds \
	$(HERE)/matching/extra/full_arrest_stop_matches.rds \
	$(HERE)/matching/extra/stop_arrest_match_ids.rds \
	$(HERE)/matching/output/citations.rds \
	$(HERE)/matching/output/arrests.rds \
	$(HERE)/matching/output/stops.rds \
	$(HERE)/matching/output/matching_values.rds \
	$(HERE)/matching/output/use_force.rds 


matching_input := \
		$(HERE)/processing/output/use_force.rds \
		$(HERE)/processing/output/calls_stops.rds \
		$(HERE)/processing/output/citations.rds \
		$(HERE)/processing/output/arrests.rds \
		$(HERE)/matching/frozen/force_matches_scored.csv 


all: $(matching_output)

clean:
	rm output/*


$(matching_output): \
		src/matching_script.R \
		$(matching_input)
	Rscript --vanilla $<

# done
