#
# Authors:     TS
# Maintainers: BR, TS
# Copyright:   2019
# =========================================
# HRW-us-tulsa/import/Makefile

.PHONY: all clean

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

outfiles := $(HERE)/import/output/arrests.rds \
	$(HERE)/import/output/calls_stops.rds \
	$(HERE)/import/output/checkpoints.rds \
	$(HERE)/import/output/citations.rds \
	$(HERE)/import/output/complaints.rds \
	$(HERE)/import/output/offenses.rds \
	$(HERE)/import/output/use_force.rds

infiles := \
		$(HERE)/import/input/Item\ A\ (Traffic\ Stops\ and\ Ped\ Checks)/2015_TriTech_Allothercalls.xlsx \
		$(HERE)/import/input/Item\ A\ (Traffic\ Stops\ and\ Ped\ Checks)/2016_TriTech_Allothercalls.xlsx \
		$(HERE)/import/input/Item\ A\ (Traffic\ Stops\ and\ Ped\ Checks)/2014_TriTech_Allothercalls.xlsx \
		$(HERE)/import/input/Item\ A\ (Traffic\ Stops\ and\ Ped\ Checks)/2017_TriTech_Allothercalls.xlsx \
		$(HERE)/import/input/Item\ A\ (Traffic\ Stops\ and\ Ped\ Checks)/2014_TriTech_RedactedCalls.xlsx \
		$(HERE)/import/input/Item\ A\ (Traffic\ Stops\ and\ Ped\ Checks)/2015_TriTech_RedactedCalls.xlsx \
		$(HERE)/import/input/Item\ A\ (Traffic\ Stops\ and\ Ped\ Checks)/2016_TriTech_RedactedCalls.xlsx \
		$(HERE)/import/input/Item\ A\ (Traffic\ Stops\ and\ Ped\ Checks)/2017_TriTech_RedactedCalls.xlsx \
		$(HERE)/import/input/Item\ B\ (Arrests)/Arrests\ 2012-2018YTD.csv \
		$(HERE)/import/input/Item\ C\ (Citations)/Citations_01012016_to_03122018.xlsx \
		$(HERE)/import/input/Item\ D\ (Crime\ Reports)/2016-2017_CrimeReportDetails.xlsx \
		$(HERE)/import/input/Item\ E\ (Checkpoints)/TPD\ Checkpoint\ Summary.xlsx \
		$(HERE)/import/input/Human\ Rights\ Watch\ complaints.csv \
		$(HERE)/import/input/Human\ Rights\ Watch\ use_of_force.csv

all: $(outfiles)

clean:
		-rm -r output/*

$(outfiles): \
		src/tulsa_import.R \
		$(infiles)
	Rscript --vanilla $<

# done.
