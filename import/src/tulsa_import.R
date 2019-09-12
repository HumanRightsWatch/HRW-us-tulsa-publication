#
# Authors:     BR
# Maintainers: BR, TS
# Copyright:   2019
# =========================================
# HRW-us-tulsa/import/src/tulsa_import.R

library(tidyverse)
library(readxl)
library(here)

#traffic/ped stops
calls_stops_files <- c(
    "import/input/Item A (Traffic Stops and Ped Checks)/2015_TriTech_Allothercalls.xlsx",
    "import/input/Item A (Traffic Stops and Ped Checks)/2016_TriTech_Allothercalls.xlsx",
    "import/input/Item A (Traffic Stops and Ped Checks)/2014_TriTech_Allothercalls.xlsx",
    "import/input/Item A (Traffic Stops and Ped Checks)/2017_TriTech_Allothercalls.xlsx",
    "import/input/Item A (Traffic Stops and Ped Checks)/2014_TriTech_RedactedCalls.xlsx",
    "import/input/Item A (Traffic Stops and Ped Checks)/2015_TriTech_RedactedCalls.xlsx",
    "import/input/Item A (Traffic Stops and Ped Checks)/2016_TriTech_RedactedCalls.xlsx",
    "import/input/Item A (Traffic Stops and Ped Checks)/2017_TriTech_RedactedCalls.xlsx"
)

files <- tribble(
    ~name,          ~reader,       ~inputfile, ~badcolumn,
    "calls_stops",  "read_excel",   calls_stops_files, "",
    "arrests",      "read_csv",     "import/input/Item B (Arrests)/Arrests 2012-2018YTD.csv", "ROW",
    "citations",    "read_excel",   "import/input/Item C (Citations)/Citations_01012016_to_03122018.xlsx", "ROW",
    "offenses",     "read_excel",   "import/input/Item D (Crime Reports)/2016-2017_CrimeReportDetails.xlsx", "",
    "checkpoints",  "read_excel",   "import/input/Item E (Checkpoints)/TPD Checkpoint Summary.xlsx", "",
    "complaints",   "read_csv",     "import/input/Human Rights Watch complaints.csv", "",
    "use_force",    "read_csv",     "import/input/Human Rights Watch use_of_force.csv", ""
)


read_input <- function(reader, input) do.call(reader, list(input))
remove_badcolumn <- function(data, badcolumn)
    if (badcolumn == "") data else select(data, -!!badcolumn)

files <- files %>%
    unnest() %>%
    mutate(inputfile = map_chr(inputfile, here)) %>%
    mutate(data = map2(reader, inputfile, read_input),
           data = map2(data, badcolumn, remove_badcolumn))

for_export <- files %>%
    select(name, data) %>% 
    split(.$name) %>%
    map(~bind_rows(.$data))

# checkpoints file has some extra stuff in the excel sheet
for_export$checkpoints <- for_export$checkpoints %>%
    select(1:6) %>%
    filter(!is.na(`Begin Date and Time`))

export <- function(data, name) {
    expected_dimensions <- list(
        calls_stops = c(1160023, 60),
        arrests = c(321300, 7),
        citations = c(133346, 10),
        offenses = c(140200, 13),
        checkpoints = c(15, 6),
        complaints = c(1775, 12),
        use_force = c(1700, 20))

    stopifnot(expected_dimensions[[name]] == dim(data))
    outputfile <- here("import", "output", paste0(name, ".rds"))
    write_rds(data, outputfile)
}

imap(for_export, export)

# done.