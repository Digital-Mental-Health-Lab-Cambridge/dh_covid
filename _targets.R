library(targets)
library(tarchetypes)
library(data.table)
library(dplyr)
library(magrittr)

source("R\\functions.R")

list(
    tar_target(
        raw_data_file,
        "..\\..\\..\\ownCloud - Tom Metherell@cloud.mrc-cbu.cam.ac.uk\\2023_unicef_covid\\raw_data\\DH_12Countries_110222.csv",
        format = "file"
    ),

    tar_target(
        data_raw,
        fread(
            raw_data_file
        ) %>% 
            data.frame()
    ),

    tar_target(
        data_clean,
        clean_data(
            data_raw
        )
    ),

    tar_target(
        NA_by_country,
        country_missingness(
            data_clean
        )
    ),

    tar_target(
        NA_by_language,
        language_missingness(
            data_clean
        )
    )
)